#### This is the script to summarise the climate data monthly, by polygons
library(tidyverse)
library(KrigR)
library(lubridate)
library(sf)
library(terra)
library(exactextractr)

#### Setup directory
Dir.Data <- file.path(getwd(), "output/") # directory of the data downloaded

#### Function to calculate relative humidity from 2m temperature and 2m dewpoint temperature
calculate_relative_humidity <- function(T_raster, Td_raster) {
  
  # Convert from Kelvin to Celsius
  T_celsius <- T_raster - 273.15
  Td_celsius <- Td_raster - 273.15
  
  # Calculate relative humidity using Magnus formula
  # RH = 100 * exp((17.625 * Td)/(243.04 + Td)) / exp((17.625 * T)/(243.04 + T))
  
  # Calculate saturation vapor pressure for dewpoint temperature
  es_td <- exp((17.625 * Td_celsius) / (243.04 + Td_celsius))
  
  # Calculate saturation vapor pressure for air temperature  
  es_t <- exp((17.625 * T_celsius) / (243.04 + T_celsius))
  
  # Calculate relative humidity (as percentage)
  RH_raster <- 100 * (es_td / es_t)
  
  # Set names to match input raster names (useful for time series)
  names(RH_raster) <- paste0("RH_", names(T_raster))
  
  # Copy time information if it exists
  if (!is.null(terra::time(T_raster))) {
    terra::time(RH_raster) <- terra::time(T_raster)
  }
  
  return(RH_raster)
}

#### Helper function to align population resolution to climate data resolution
#### Method from: https://climateestimate.net/content/weighting-schemes.html
align_population_to_climate <- function(pop_raster, climate_raster) {
  
  # Get resolutions
  pop_res <- terra::res(pop_raster)[1]  # 0.008333333
  climate_res <- terra::res(climate_raster)[1]  # 0.1
  
  cat("Population resolution:", pop_res, "\n")
  cat("Climate resolution:", climate_res, "\n")
  
  # Calculate resolution ratio
  res_ratio <- climate_res / pop_res  # Should be 12
  cat("Resolution ratio:", res_ratio, "\n")
  
  # Step 1: Crop population to climate extent first (rough crop)
  climate_ext <- terra::ext(climate_raster)
  pop_cropped <- terra::crop(pop_raster, climate_ext)
  cat("After cropping - Pop extent:", as.vector(terra::ext(pop_cropped)), "\n")
  cat("Climate extent:", as.vector(climate_ext), "\n")
  
  # Step 2: Check if we need to disaggregate for grid alignment
  # Calculate how the climate grid edges align with population grid
  climate_xmin <- climate_ext[1]
  climate_ymin <- climate_ext[3]
  
  # Find the offset in terms of population grid cells
  pop_ext <- terra::ext(pop_cropped)
  x_offset <- (climate_xmin - pop_ext[1]) / pop_res
  y_offset <- (climate_ymin - pop_ext[3]) / pop_res
  
  cat("X offset (in pop cells):", x_offset, "\n")
  cat("Y offset (in pop cells):", y_offset, "\n")
  
  # Check if we have fractional offsets (indicating misalignment)
  x_fractional <- x_offset - floor(x_offset)
  y_fractional <- y_offset - floor(y_offset)
  
  if (abs(x_fractional) > 0.01 || abs(y_fractional) > 0.01) {
    cat("Grid misalignment detected, disaggregating...\n")
    
    # Disaggregate by factor of 2 to align grids
    disagg_factor <- 2
    pop_disagg <- terra::disagg(pop_cropped, fact = disagg_factor) / (disagg_factor^2)
    
    # Crop again to exact climate extent
    pop_disagg <- terra::crop(pop_disagg, climate_ext)
    
    # Calculate new aggregation factor
    new_pop_res <- terra::res(pop_disagg)[1]
    agg_factor <- round(climate_res / new_pop_res)
    
    cat("After disaggregation - resolution:", new_pop_res, "\n")
    cat("Aggregation factor needed:", agg_factor, "\n")
    
  } else {
    cat("Grids already aligned, no disaggregation needed\n")
    pop_disagg <- pop_cropped
    agg_factor <- round(res_ratio)
  }
  
  # Step 3: Aggregate to match climate resolution
  if (agg_factor > 1) {
    pop_aligned <- terra::aggregate(pop_disagg, fact = agg_factor, fun = sum, na.rm = TRUE)
    cat("After aggregation - resolution:", terra::res(pop_aligned)[1], "\n")
  } else {
    pop_aligned <- pop_disagg
  }
  
  # Step 4: Final crop to exact climate extent and ensure perfect alignment
  pop_final <- terra::crop(pop_aligned, climate_ext)
  
  # Step 5: Handle any remaining slight misalignments by resampling
  if (!identical(terra::ext(pop_final), terra::ext(climate_raster))) {
    cat("Final resampling for perfect alignment...\n")
    pop_final <- terra::resample(pop_final, climate_raster, method = "near")
  }
  
  # Step 6: Set minimum population of 1 to avoid zero weights
  pop_final[pop_final == 0] <- 1
  pop_final[is.na(pop_final)] <- 1
  
  cat("Final alignment check:\n")
  cat("Climate extent:", as.vector(terra::ext(climate_raster)), "\n")
  cat("Population extent:", as.vector(terra::ext(pop_final)), "\n")
  cat("Extents identical:", identical(terra::ext(climate_raster), terra::ext(pop_final)), "\n")
  
  return(pop_final)
}

#### Function to summarise temperature data with population weighting
temperature_monthly_summary <- function(data, admin_shp, pop_raster = NULL){
  
  # Get the dates from your raster
  dates <- terra::time(data)
  
  # Create month-year labels
  months <- format(dates, "%Y-%m")
  unique_months <- unique(months)
  
  # Aggregate daily data to monthly means
  monthly_temp <- terra::tapp(data, months, fun = mean, na.rm = TRUE)
  
  # Set proper time stamps for monthly data
  monthly_dates <- as.Date(paste0(unique_months, "-01"))
  terra::time(monthly_temp) <- monthly_dates
  
  # Calculate zonal statistics
  monthly_temp_list <- list()
  layer_name_to_date <- tibble(month_year = names(monthly_temp),
                               date = monthly_dates)
  
  for (i in 1:terra::nlyr(monthly_temp)) {
    layer_name <- names(monthly_temp)[i]
    
    if (!is.null(pop_raster)) {
      # Ensure both rasters have the same CRS
      terra::crs(monthly_temp[[i]]) <- "EPSG:4326"  # Set explicit CRS for climate data
      
      # Aligning population raster resolution: https://climateestimate.net/content/weighting-schemes.html
      pop_raster_aligned <- align_population_to_climate(pop_raster, monthly_temp[[i]])
      
      # sum(terra::values(pop_raster_aligned, na.rm = TRUE), na.rm = TRUE)
      # sum(terra::values(pop_raster, na.rm = TRUE), na.rm = TRUE)
      
      # Ensure admin_shp has correct CRS
      admin_shp <- sf::st_transform(admin_shp, "EPSG:4326")
      
      # Population-weighted and regular mean
      # exactextractr handles different resolutions automatically
      temp_values <- exactextractr::exact_extract(
        monthly_temp[[i]], 
        admin_shp, 
        fun = c('mean', 'weighted_mean'),
        weights = pop_raster_aligned,  # Use original resolution
        append_cols = TRUE
      )
      temp_values$temperature_celsius <- temp_values$mean - 273.15
      temp_values$temperature_celsius_pop_weighted <- temp_values$weighted_mean - 273.15
      
      # Extract total population for reference
      pop_values <- exactextractr::exact_extract(
        pop_raster,  # Use original resolution
        admin_shp,
        fun = 'sum',
        append_cols = TRUE
      )
      temp_values$total_population <- pop_values$sum
      
    } else {
      # Regular mean only
      temp_values <- exactextractr::exact_extract(
        monthly_temp[[i]], 
        admin_shp, 
        'mean',
        append_cols = TRUE
      )
      temp_values$temperature_celsius <- temp_values$mean - 273.15
    }
    
    temp_values$month_year <- layer_name
    monthly_temp_list[[i]] <- temp_values %>% left_join(layer_name_to_date)
  }
  
  # Combine all months
  data_summary <- do.call(rbind, monthly_temp_list)
  
  return(data_summary)
}

#### Function to summarise precipitation data with population weighting
precipitation_monthly_summary <- function(data, admin_shp, pop_raster = NULL){
  
  # Get the dates from your raster
  dates <- terra::time(data)
  
  # Create month-year labels
  months <- format(dates, "%Y-%m")
  unique_months <- unique(months)
  
  # Aggregate daily data to monthly sums
  monthly_prec <- terra::tapp(data, months, fun = sum, na.rm = TRUE)
  
  # Set proper time stamps for monthly data
  monthly_dates <- as.Date(paste0(unique_months, "-01"))
  terra::time(monthly_prec) <- monthly_dates
  
  # Calculate zonal statistics
  monthly_prec_list <- list()
  layer_name_to_date <- tibble(month_year = names(monthly_prec),
                               date = monthly_dates)
  
  for (i in 1:terra::nlyr(monthly_prec)) {
    layer_name <- names(monthly_prec)[i]
    
    if (!is.null(pop_raster)) {
      # Ensure both rasters have the same CRS
      terra::crs(monthly_prec[[i]]) <- "EPSG:4326"  # Set explicit CRS for climate data
      
      # Aligning population raster resolution: https://climateestimate.net/content/weighting-schemes.html
      pop_raster_aligned <- align_population_to_climate(pop_raster, monthly_prec[[i]])
      
      # sum(terra::values(pop_raster_aligned, na.rm = TRUE), na.rm = TRUE)
      # sum(terra::values(pop_raster, na.rm = TRUE), na.rm = TRUE)
      
      # Ensure admin_shp has correct CRS
      admin_shp <- sf::st_transform(admin_shp, "EPSG:4326")
      
      # Population-weighted and regular mean
      # exactextractr handles different resolutions automatically
      prec_values <- exactextractr::exact_extract(
        monthly_prec[[i]], 
        admin_shp, 
        fun = c('mean', 'weighted_mean'),
        weights = pop_raster_aligned,  # Use original resolution
        append_cols = TRUE
      )
      prec_values$precipitation_mm <- prec_values$mean * 1000
      prec_values$precipitation_mm_pop_weighted <- prec_values$weighted_mean * 1000
      
      # Extract total population for reference
      pop_values <- exactextractr::exact_extract(
        pop_raster,  # Use original resolution
        admin_shp,
        fun = 'sum',
        append_cols = TRUE
      )
      prec_values$total_population <- pop_values$sum
      
    } else {
      # Regular mean only
      prec_values <- exactextractr::exact_extract(
        monthly_prec[[i]], 
        admin_shp, 
        'mean',
        append_cols = TRUE
      )
      prec_values$precipitation_mm <- prec_values$mean * 1000
    }
    
    prec_values$month_year <- layer_name
    monthly_prec_list[[i]] <- prec_values %>% left_join(layer_name_to_date)
  }
  
  # Combine all months
  data_summary <- do.call(rbind, monthly_prec_list)
  
  return(data_summary)
}

#### Function to summarise relative humidity data with population weighting
rhumidity_monthly_summary <- function(data, admin_shp, pop_raster = NULL){
  
  # Get the dates from your raster
  dates <- terra::time(data)
  
  # Create month-year labels
  months <- format(dates, "%Y-%m")
  unique_months <- unique(months)
  
  # Aggregate daily data to monthly means
  monthly_temp <- terra::tapp(data, months, fun = mean, na.rm = TRUE)
  
  # Set proper time stamps for monthly data
  monthly_dates <- as.Date(paste0(unique_months, "-01"))
  terra::time(monthly_temp) <- monthly_dates
  
  # Calculate zonal statistics
  monthly_temp_list <- list()
  layer_name_to_date <- tibble(month_year = names(monthly_temp),
                               date = monthly_dates)
  
  for (i in 1:terra::nlyr(monthly_temp)) {
    layer_name <- names(monthly_temp)[i]
    
    if (!is.null(pop_raster)) {
      # Ensure both rasters have the same CRS
      terra::crs(monthly_temp[[i]]) <- "EPSG:4326"  # Set explicit CRS for climate data
      
      # Aligning population raster resolution: https://climateestimate.net/content/weighting-schemes.html
      pop_raster_aligned <- align_population_to_climate(pop_raster, monthly_temp[[i]])
      
      # sum(terra::values(pop_raster_aligned, na.rm = TRUE), na.rm = TRUE)
      # sum(terra::values(pop_raster, na.rm = TRUE), na.rm = TRUE)
      
      # Ensure admin_shp has correct CRS
      admin_shp <- sf::st_transform(admin_shp, "EPSG:4326")
      
      # Population-weighted and regular mean
      # exactextractr handles different resolutions automatically
      temp_values <- exactextractr::exact_extract(
        monthly_temp[[i]], 
        admin_shp, 
        fun = c('mean', 'weighted_mean'),
        weights = pop_raster_aligned,  # Use original resolution
        append_cols = TRUE
      )
      temp_values$rhumidity <- temp_values$mean
      temp_values$rhumidity_pop_weighted <- temp_values$weighted_mean
      
      # Extract total population for reference
      pop_values <- exactextractr::exact_extract(
        pop_raster,  # Use original resolution
        admin_shp,
        fun = 'sum',
        append_cols = TRUE
      )
      temp_values$total_population <- pop_values$sum
      
    } else {
      # Regular mean only
      temp_values <- exactextractr::exact_extract(
        monthly_temp[[i]], 
        admin_shp, 
        'mean',
        append_cols = TRUE
      )
      temp_values$temperature_celsius <- temp_values$mean - 273.15
    }
    
    temp_values$month_year <- layer_name
    monthly_temp_list[[i]] <- temp_values %>% left_join(layer_name_to_date)
  }
  
  # Combine all months
  data_summary <- do.call(rbind, monthly_temp_list)
  
  return(data_summary)
}

#### Read shapefiles
admin1_shp <- read_sf(dsn="data/shapefiles/admin1",layer="admin1_38", stringsAsFactors = FALSE)
admin2_shp <- read_sf(dsn="data/shapefiles/admin2",layer="admin2_34", stringsAsFactors = FALSE)

#### Read temperature and precipitation data
temp_list <- list()
prec_list <- list()
pop_admin1_list <- list()
pop_admin2_list <- list()

year_download <- 2015:2024 # year as a vector

for(i in seq_len(length(year_download))){
  
  processed_temp_file <- file.path(Dir.Data, paste0("mean_temperature_idn_",year_download[i],".nc"))
  temp_list[[i]] <- terra::rast(processed_temp_file)
  
  processed_prec_file <- file.path(Dir.Data, paste0("mean_precipitation_idn_",year_download[i],".nc"))
  prec_list[[i]] <- terra::rast(processed_prec_file)
  
  if (i <= 8){
    processed_pop_admin1_file <- paste0("data/pop/masked/landscan-idn-admin1-",year_download[i],".tif")
    pop_admin1_list[[i]] <- terra::rast(processed_pop_admin1_file)
    processed_pop_admin2_file <- paste0("data/pop/masked/landscan-idn-admin2-",year_download[i],".tif")
    pop_admin2_list[[i]] <- terra::rast(processed_pop_admin2_file)
  } else {
    processed_pop_admin1_file <- paste0("data/pop/masked/landscan-idn-admin1-",year_download[8],".tif")
    pop_admin1_list[[i]] <- terra::rast(processed_pop_admin1_file)
    processed_pop_admin2_file <- paste0("data/pop/masked/landscan-idn-admin2-",year_download[8],".tif")
    pop_admin2_list[[i]] <- terra::rast(processed_pop_admin2_file)
  }
  
}

#### Summarise temperature and precipitation by month at admin1 level
temp_monthly_admin1_list <- list()
prec_monthly_admin1_list <- list()

temp_monthly_admin2_list <- list()
prec_monthly_admin2_list <- list()

for (i in seq_len(length(temp_list))){
  
  temp_monthly_admin1_list[[i]] <- 
    temperature_monthly_summary(temp_list[[i]],admin1_shp,pop_admin1_list[[i]])
  prec_monthly_admin1_list[[i]] <-
    precipitation_monthly_summary(prec_list[[i]],admin1_shp,pop_admin1_list[[i]])
  
  temp_monthly_admin2_list[[i]] <- 
    temperature_monthly_summary(temp_list[[i]],admin2_shp,pop_admin2_list[[i]])
  prec_monthly_admin2_list[[i]] <-
    precipitation_monthly_summary(prec_list[[i]],admin2_shp,pop_admin2_list[[i]])
  
  print(paste0("Done year: ",year_download[i]))
  
}

temp_monthly_admin1 <- bind_rows(temp_monthly_admin1_list)
prec_monthly_admin1 <- bind_rows(prec_monthly_admin1_list)

temp_monthly_admin2 <- bind_rows(temp_monthly_admin2_list)
prec_monthly_admin2 <- bind_rows(prec_monthly_admin2_list)

saveRDS(temp_monthly_admin1,"output/temp_monthly_admin1.rds")
saveRDS(temp_monthly_admin2,"output/temp_monthly_admin2.rds")

saveRDS(prec_monthly_admin1,"output/prec_monthly_admin1.rds")
saveRDS(prec_monthly_admin2,"output/prec_monthly_admin2.rds")

#### Process relative humidity
d_temp_list <- list()
temp_list <- list()
pop_admin1_list <- list()
pop_admin2_list <- list()
humid_list <- list()
for(i in seq_len(length(year_download))){
  
  processed_d_temp_file <- file.path(Dir.Data, paste0("mean_d_temperature_idn_",year_download[i],".nc"))
  d_temp_list[[i]] <- terra::rast(processed_d_temp_file)
  
  processed_temp_file <- file.path(Dir.Data, paste0("mean_temperature_idn_",year_download[i],".nc"))
  temp_list[[i]] <- terra::rast(processed_temp_file)
  
  humid_list[[i]] <- calculate_relative_humidity(temp_list[[i]], d_temp_list[[i]])
  
  if (i <= 8){
    processed_pop_admin1_file <- paste0("data/pop/masked/landscan-idn-admin1-",year_download[i],".tif")
    pop_admin1_list[[i]] <- terra::rast(processed_pop_admin1_file)
    processed_pop_admin2_file <- paste0("data/pop/masked/landscan-idn-admin2-",year_download[i],".tif")
    pop_admin2_list[[i]] <- terra::rast(processed_pop_admin2_file)
  } else {
    processed_pop_admin1_file <- paste0("data/pop/masked/landscan-idn-admin1-",year_download[8],".tif")
    pop_admin1_list[[i]] <- terra::rast(processed_pop_admin1_file)
    processed_pop_admin2_file <- paste0("data/pop/masked/landscan-idn-admin2-",year_download[8],".tif")
    pop_admin2_list[[i]] <- terra::rast(processed_pop_admin2_file)
  }
  
  print(paste0("Done year: ",year_download[i]))
  
}

#### Summarise relative humidity by month at admin1 and admin2 level
humid_monthly_admin1_list <- list()
humid_monthly_admin2_list <- list()

for (i in seq_len(length(temp_list))){
  
  humid_monthly_admin1_list[[i]] <- 
    rhumidity_monthly_summary(humid_list[[i]],admin1_shp,pop_admin1_list[[i]])
  
  humid_monthly_admin2_list[[i]] <- 
    rhumidity_monthly_summary(humid_list[[i]],admin2_shp,pop_admin2_list[[i]])
  
  print(paste0("Done year: ",year_download[i]))
  
}

humid_monthly_admin1 <- bind_rows(humid_monthly_admin1_list)
humid_monthly_admin2 <- bind_rows(humid_monthly_admin2_list)

saveRDS(humid_monthly_admin1,"output/humid_monthly_admin1.rds")
saveRDS(humid_monthly_admin2,"output/humid_monthly_admin2.rds")

#### national level summary
admin0_shp <- admin1_shp %>%
  summarise(geometry = st_union(geometry))

admin0_shp %>% ggplot() + geom_sf()

#### Read temperature, dew point temperature and precipitation data
temp_list <- list()
prec_list <- list()
pop_admin1_list <- list()
pop_admin2_list <- list()
d_temp_list <- list()
humid_list <- list()

year_download <- 2015:2024 # year as a vector

for(i in seq_len(length(year_download))){
  
  processed_temp_file <- file.path(Dir.Data, paste0("mean_temperature_idn_",year_download[i],".nc"))
  temp_list[[i]] <- terra::rast(processed_temp_file)
  
  processed_prec_file <- file.path(Dir.Data, paste0("mean_precipitation_idn_",year_download[i],".nc"))
  prec_list[[i]] <- terra::rast(processed_prec_file)
  
  processed_d_temp_file <- file.path(Dir.Data, paste0("mean_d_temperature_idn_",year_download[i],".nc"))
  d_temp_list[[i]] <- terra::rast(processed_d_temp_file)
  
  humid_list[[i]] <- calculate_relative_humidity(temp_list[[i]], d_temp_list[[i]])
  
  if (i <= 8){
    processed_pop_admin1_file <- paste0("data/pop/masked/landscan-idn-admin1-",year_download[i],".tif")
    pop_admin1_list[[i]] <- terra::rast(processed_pop_admin1_file)
    processed_pop_admin2_file <- paste0("data/pop/masked/landscan-idn-admin2-",year_download[i],".tif")
    pop_admin2_list[[i]] <- terra::rast(processed_pop_admin2_file)
  } else {
    processed_pop_admin1_file <- paste0("data/pop/masked/landscan-idn-admin1-",year_download[8],".tif")
    pop_admin1_list[[i]] <- terra::rast(processed_pop_admin1_file)
    processed_pop_admin2_file <- paste0("data/pop/masked/landscan-idn-admin2-",year_download[8],".tif")
    pop_admin2_list[[i]] <- terra::rast(processed_pop_admin2_file)
  }
  
}

#### Summarise temperature and precipitation by month at admin1 level
temp_monthly_admin0_list <- list()
prec_monthly_admin0_list <- list()
humid_monthly_admin0_list <- list()

for (i in seq_len(length(temp_list))){
  
  temp_monthly_admin0_list[[i]] <- 
    temperature_monthly_summary(temp_list[[i]],admin0_shp,pop_admin1_list[[i]])
  prec_monthly_admin0_list[[i]] <-
    precipitation_monthly_summary(prec_list[[i]],admin0_shp,pop_admin1_list[[i]])
  humid_monthly_admin0_list[[i]] <- 
    rhumidity_monthly_summary(humid_list[[i]],admin0_shp,pop_admin1_list[[i]])
  
  print(paste0("Done year: ",year_download[i]))
  
}

temp_monthly_admin0 <- bind_rows(temp_monthly_admin0_list) %>% 
  mutate(admin1="INDONESIA")
prec_monthly_admin0 <- bind_rows(prec_monthly_admin0_list) %>% 
  mutate(admin1="INDONESIA")
humid_monthly_admin0 <- bind_rows(humid_monthly_admin0_list) %>% 
  mutate(admin1="INDONESIA")

saveRDS(temp_monthly_admin0,"output/temp_monthly_admin0.rds")
saveRDS(prec_monthly_admin0,"output/prec_monthly_admin0.rds")
saveRDS(humid_monthly_admin0,"output/humid_monthly_admin0.rds")