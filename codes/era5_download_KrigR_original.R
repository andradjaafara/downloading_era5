# here I would like to write a tutorial on how to download ERA5 climate data
# using R package KrigR
# there's already an extensive tutorial by the creator of the package here:
# https://www.erikkusch.com/courses/krigr/
# and you can also read the github documentation https://github.com/ErikKusch/KrigR
# as well as a publication on the package https://iopscience.iop.org/article/10.1088/1748-9326/ac48b3
# however, it seems like updates in the package weren't incorporated in the tutorial
# and I hope this tutorial would make it easier for people who would like
# to use this package to get things done easier
# but I still encourage to read about the package in details as what I do is
# definitely very limited to the context of my work and there are many things 
# that are not covered here
# hopefully still good enough to those who want to do initial exploration on the data

# let's start with loading some packages
#### library
library(tidyverse)
library(KrigR)
library(lubridate)
library(sf)
library(terra)

# you need to have username and password to download era5 data from
# click login - register on the top right of this website https://cds.climate.copernicus.eu/
# register yourself if you havent done it
# if you've registered click Your Profile on the top right
# then copy your API Token
# here, create a variable API_Key and paste your API token there
# for API_User, use the email account you use for registration
# now, everything should be all set in terms of setting up an account for downloading data
# you can also use the website for data downloading
# but I feel it's also good to have everything in a single set of code
# in case you want to revisit it in the future
#### set your user name and API token
API_User <- "enter.your.email@gmail.com"
API_Key <- "enter-your-api-token"

#### your output directory
dir_data <- file.path(getwd(), "output/")

# in this example, i'll use jakarta shapefile
# (jakarta is located in Indonesia for those who don't know!)
# of course you can download all data without boundary, but that would be a big dataset
# in this tutorial, we'll start from a relatively small area and then you can expand
# the context of your data downloading process yourself
# the shapefiles I used are downloaded from humdata: https://data.humdata.org/dataset/geoboundaries-admin-boundaries-for-indonesia
#### read shapefile
admin1_shp <- read_sf(dsn="data/shapefiles/admin1",layer="admin1_34", stringsAsFactors = FALSE)
admin2_shp <- read_sf(dsn="data/shapefiles/admin2",layer="admin2_34", stringsAsFactors = FALSE)

# for jakarta admin1, I excluded the thousand island district located in the north of Jakarta
#### get jakarta shapefile
jakarta_shp <- admin1_shp %>% filter(shapeName == "Jakarta")
jakarta_admin2_shp <- admin2_shp %>% filter(admin1 == "DKI JAKARTA")

# let's do quick check on how jakarta looks like
# first, let's see the areas located in java island
#### check shapefile of jakarta
ggplot() +
  geom_sf(data=jakarta_shp) +
  theme_bw()

# and this is including the thousand island district
ggplot() +
  geom_sf(data=jakarta_shp) +
  geom_sf(data=jakarta_admin2_shp) +
  theme_bw()

# let's get the extent (bounding box) of jakarta, excluding thousand island district
jakarta_extent <- ext(st_bbox(jakarta_shp))

# let's do a quick download of temperature data using this bounding box
# we use the CDownloadS function from KrigR package
# you can always check the package help to see what's inside CDownloadS function
# ERA5 data is available hourly, you'll get the temporary hourly netCDF data in your
# download folder
# but the example here is to download daily summary over a period of interest
# within the extent of boundary we set
download_temperature_jkt <- CDownloadS(
  ## Variable and Data Product
  Variable = "2m_temperature", # this is air temperature
  DataSet = "reanalysis-era5-land", # data product from which we want to download
  ## Time-Window
  DateStart = "2022-08-01", # date at which time window opens
  DateStop = "2022-09-30", # date at which time window terminates
  TZone = "Etc/GMT+7", # GMT+7 to align with our study region
  ## Temporal Aggregation
  TResolution = "day", # we want daily aggregates
  TStep = 1, # we want aggregates of 1 day each
  FUN = "mean", # aggregating function for the daily summary
  ## Spatial Limiting
  Extent = jakarta_extent, # our rectangular bounding box
  ## File Storing
  Dir = dir_data, # where to store the data
  FileName = "download_temperature_jkt", # what to call the resulting file
  ## API User Credentials
  API_User = API_User,
  API_Key = API_Key
)

# when you run code, you'll see that querying process is starting
# and then the data download process starts
# it will take time for the download to proceed and finish
# depending on connection, how busy the server is, and how big your data is
# also, if you download precipitation, there's disaggregating process that needs
# to be done, which will also takes time
# if you already have data with the same filename, it will refuse to download
# so check the message in your console, if you want to start a big download, make sure that
# you dont have the files already in your computer folder
# then after it finishes what happened? you'll get an error
# this is a weird thing about this package that I don't know why this happen
# but if you check your folder, you'll see two files
# one is a netCDF file with the specified file name, and the other one is 
# a temporary file with bigger size (this is the hourly data you downloaded)
# I was looking around and tried LLM to help with this and basically the solution
# since the data is downloaded successfully, to just bypass the error and 
# read the data manually!
# you can do that by using this code
# but firstly, delete all the data you just downloaded from your folder
# you can get all the variables that you can download by checking Meta.Variables() 
download_temperature_jkt <- tryCatch({
  CDownloadS(
    ## Variable and Data Product
    Variable = "2m_temperature", # this is air temperature
    DataSet = "reanalysis-era5-land", # data product from which we want to download
    ## Time-Window
    DateStart = "2022-08-01 00:00:00", # date at which time window opens
    DateStop = "2022-09-30 23:00:00", # date at which time window terminates
    TZone = "Etc/GMT+7", # GMT+7 to align with our study region
    ## Temporal Aggregation
    TResolution = "day", # we want daily aggregates
    TStep = 1, # we want aggregates of 1 day each
    FUN = "mean", # aggregating function for the daily summary
    ## Spatial Limiting
    Extent = jakarta_extent, # our rectangular bounding box
    ## File Storing
    Dir = dir_data, # where to store the data
    FileName = "download_temperature_jkt", # what to call the resulting file
    ## API User Credentials
    API_User = API_User,
    API_Key = API_Key
  )
}, error = function(e) {
  # If there's an error, try to load the file that was likely created
  cat("Metadata writing failed, but attempting to load created file...\n")
  file_path <- file.path(dir_data, "download_temperature_jkt.nc")
  if (file.exists(file_path)) {
    cat("Successfully loaded data despite metadata error!\n")
    return(terra::rast(file_path))
  } else {
    stop("Data file was not created: ", e$message)
  }
})

# look at the message in your console, yes you got error, but you actually successfully
# downloaded the data and load it to your R with the same name specified
# let's check

download_temperature_jkt

# you've got 2 x 3 pixels of data with 61 layers which is two months worth of data
# as we specified august and september 2022, each 31 and 30 days
# from here, you can then specify the location and time yourself, and try other data
# however, I would like to show to other examples which are precipitation and relative humidity.
# for precipitation, what you'll get is hourly cumulative precipitation, instead of
# an independent hourly values, which is then why the code set up is a little bit different.
# after downloading, it will do a disaggregation of the cumulative data before resummarising it again
# which is why it could take longer
# I personally use cluster to download precipitation data as I takes really long
# using my personal computer to download 1 year worth of daily data (>24 hours)

download_precipitation_jkt <- tryCatch({
  CDownloadS(
    Variable = "total_precipitation",
    CumulVar = TRUE, # TRUE for precipitation as it is recorded cumulatively
    DataSet = "reanalysis-era5-land",
    DateStart = "2022-08-01 00:00:00",
    DateStop = "2022-09-30 23:00:00",
    TZone = "Etc/GMT+7",
    TResolution = "day",
    TStep = 1,
    FUN = "sum", # we would like to get the total precipitation each day
    Extent = jakarta_shp, # you can also do something like this instead of using bounding box
    Dir = dir_data,
    FileName = "download_precipitation_jkt",
    API_User = API_User,
    API_Key = API_Key
  )
}, error = function(e) {
  # If there's an error, try to load the file that was likely created
  cat("Metadata writing failed, but attempting to load created file...\n")
  file_path <- file.path(dir_data, "download_precipitation_jkt.nc")
  if (file.exists(file_path)) {
    cat("Successfully loaded data despite metadata error!\n")
    return(terra::rast(file_path))
  } else {
    stop("Data file was not created: ", e$message)
  }
})

# and the last example is on how do download relative humidity
# this is not as straightforward as the previous ones as relative humidity can't be 
# downloaded directly. instead, it's a combination of two climate variables:
# the 2m temperature and 2m dewpoint temperature
# so to get relative humidity, as previously we have downloaded the 2m temperature
# let's download 2m dewpoint temperature
download_dewpoint_temperature_jkt <- tryCatch({
  CDownloadS(
    ## Variable and Data Product
    Variable = "2m_dewpoint_temperature", # this is air temperature
    DataSet = "reanalysis-era5-land", # data product from which we want to download
    ## Time-Window
    DateStart = "2022-08-01 00:00:00", # date at which time window opens
    DateStop = "2022-09-30 23:00:00", # date at which time window terminates
    TZone = "Etc/GMT+7", # GMT+7 to align with our study region
    ## Temporal Aggregation
    TResolution = "day", # we want daily aggregates
    TStep = 1, # we want aggregates of 1 day each
    FUN = "mean", # aggregating function for the daily summary
    ## Spatial Limiting
    Extent = jakarta_extent, # our rectangular bounding box
    ## File Storing
    Dir = dir_data, # where to store the data
    FileName = "download_dewpoint_temperature_jkt", # what to call the resulting file
    ## API User Credentials
    API_User = API_User,
    API_Key = API_Key
  )
}, error = function(e) {
  # If there's an error, try to load the file that was likely created
  cat("Metadata writing failed, but attempting to load created file...\n")
  file_path <- file.path(dir_data, "download_dewpoint_temperature_jkt.nc")
  if (file.exists(file_path)) {
    cat("Successfully loaded data despite metadata error!\n")
    return(terra::rast(file_path))
  } else {
    stop("Data file was not created: ", e$message)
  }
})

# below is the function to calculate relative humidity from temperature
# and dewpoint temperature rasters
# you can find the formula here: https://agupubs.onlinelibrary.wiley.com/doi/full/10.1029/2022GL099740
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

# still a few steps before using the function
# first, convert the netcdf to raster
# get the location of netCDF file
temperature_file <- file.path(dir_data, paste0("download_temperature_jkt.nc"))
temperature_raster <- terra::rast(temperature_file)
dewpoint_temperature_file <- file.path(dir_data, paste0("download_dewpoint_temperature_jkt.nc"))
dewpoint_temperature_raster <- terra::rast(dewpoint_temperature_file)

# the, use the function
relative_humidity_jkt <- calculate_relative_humidity(temperature_raster, 
                                                     dewpoint_temperature_raster)

# and that's it, you've now should be able to download era 5 data
# be mindful about the variables you're going to download (whether they're cumulative or not),
# spatial extent, time frame, filename, time resolution, time zone, and aggregating function
# you're going to use when downloading data
# if you want to download multiple data in multiple time frame all at once
# you can see the example I did for indonesia in the codes/era5-download-script.R
# there, you can also see in the process that I automatically delete the temporary files
# before downloading the next one
# hopefully this can be adapted easily to your context
# if you're happy with daily raster data, then this should be enough for you
# however, if you're interested in summarising your data to certain shapefiles
# with also different time frame, i.e., weekly, monthly, yearly, etc
# I have prepared some codes below

# you can look at codes/era5-monthly-aggregation-weighted-script.R
# there, from line 12 to line 370, I have prepared some functions I used for 
# summarising the data. I used functions from exactextractr package
# to do summarisation as they allow me to account for rasters that 
# only partly within certain shapefile boundaries
# they also allow for weighted averaging, in my case, I weighted the summary
# with population rasters
# as population rasters that I used have different resolutions to the 
# era5 resolution, there's also code for alignment of the resolution there
# I used population data from landscan: https://landscan.ornl.gov/
# they have an article about their data here: https://www.nature.com/articles/s41597-025-04817-z

# following are the functions I created for climate data processing
# I did monthly summary based on the shapefiles I used
# you can edit the function yourself to get the temporality you need
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

# first we need to convert our data from netCDF to raster first
temperature_file <- file.path(dir_data, paste0("download_temperature_jkt.nc"))
temperature_raster <- terra::rast(temperature_file)
dewpoint_temperature_file <- file.path(dir_data, paste0("download_dewpoint_temperature_jkt.nc"))
dewpoint_temperature_raster <- terra::rast(dewpoint_temperature_file)
precipitation_file <- file.path(dir_data, paste0("download_precipitation_jkt.nc"))
precipitation_raster <- terra::rast(precipitation_file)

# then get relative humidity raster
relative_humidity_raster <- calculate_relative_humidity(temperature_raster, 
                                                        dewpoint_temperature_raster)

# then we can use the functions to summarise them, I'll get province-level and
# district-level estimates, but first need to remove thousand island district first from 
# admin2 map of jakarta
jakarta_district_shp <- jakarta_admin2_shp %>% 
  filter(admin2 != "KEPULAUAN SERIBU")

# let's check how it looks
jakarta_district_shp %>% ggplot() + geom_sf()

# then, load population data
# masked meaning that i've cropped the whole raster data based on the shapefiles that
# I have, so the size is not as big as it was initially
# you can always work with the whole raster
processed_pop_province_file <- paste0("data/pop/masked/landscan-idn-admin1-",2022,".tif")
pop_province <- terra::rast(processed_pop_province_file)
processed_pop_district_file <- paste0("data/pop/masked/landscan-idn-admin2-",2022,".tif")
pop_district <- terra::rast(processed_pop_district_file)

# then let's do the summary
# for temperature
# temperature is in celcius
temperature_province_monthly <- 
  temperature_monthly_summary(temperature_raster,jakarta_shp,pop_province)
temperature_district_monthly <- 
  temperature_monthly_summary(temperature_raster,jakarta_district_shp,pop_district)

# for precipitation
# within the function, I did multiplication by 1,000 to get the precipitation in mm
precipitation_province_monthly <- 
  precipitation_monthly_summary(precipitation_raster,jakarta_shp,pop_province)
precipitation_district_monthly <- 
  precipitation_monthly_summary(precipitation_raster,jakarta_district_shp,pop_district)

# for relative humidity
relative_humidity_province_monthly <- 
  rhumidity_monthly_summary(relative_humidity_raster,jakarta_shp,pop_province)
relative_humidity_district_monthly <- 
  rhumidity_monthly_summary(relative_humidity_raster,jakarta_district_shp,pop_district)

# and you can check how the summary looks like for each data




