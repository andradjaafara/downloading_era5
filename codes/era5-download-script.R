#### Script for ERA5 climate data download via KrigR package
#### KrigR package will download raw climate data and 
#### summarise it to the temporal aggregation specified
#### The output will be: 1 aggregated netCDF 2 TEMP raw data
#### Sometimes the process will give error message but the data is still downloaded
#### Script is designed to give message if this is the case
#### TEMP data (raw hourly) will be removed to preserve space
#### Resolution of ERA 5 land data is approximately 9 x 9 km^2

#### Here, we are downloading daily data for the whole year
#### Then, we aggregate them into monthly data
#### Temperature: mean
#### Precipitation: sum

#### Library load
library(tidyverse)
library(KrigR)
library(lubridate)
library(sf)
library(terra)
library(exactextractr)

#### Setup variables for API key etc
API_User <- "bimandra.djaafara@gmail.com"
API_Key <- "a642264f-8817-4f56-aa36-410bc0f2ff01"
Dir.Data <- file.path(getwd(), "output/") # directory of the data downloaded

#### Load shapefiles for extent of raster files downloaded and post-processing
admin1_shp <- read_sf(dsn="data/shapefiles/admin1",layer="admin1_34", stringsAsFactors = FALSE)
admin2_shp <- read_sf(dsn="data/shapefiles/admin2",layer="admin2_34", stringsAsFactors = FALSE)

#### Set up variables for time scale of data to be downloaded
year_download <- 2015:2024 # year as a vector

#### Download mean temperature data
filename_temp <- paste0("mean_temperature_idn_",year_download)
datestart_vec <- paste0(year_download,"-01-01 00:00:00")
datestop_vec <- paste0(year_download,"-12-31 23:00:00")

temp_download_list <- list()

#### Loop over the download years
for (i in seq_len(length(year_download))){
  
  # Download temperature data, put it in a list
  temp_download_list[[i]] <- tryCatch({
    CDownloadS(
      Variable = "2m_temperature", # temperature
      DataSet = "reanalysis-era5-land", # dataset to download from
      DateStart = datestart_vec[i], # starting date
      DateStop = datestop_vec[i], # end date
      TZone = "Etc/GMT+7", # timezone
      TResolution = "day", # summarising time resolution
      TStep = 1, # timestep of the time resolution
      FUN = "mean", # summary statistics for the aggregation
      Extent = admin2_shp, # boundaries (can use shapefiles)
      Dir = Dir.Data, # output directory
      FileName = filename_temp[i], # filename output
      API_User = API_User, # your email address
      API_Key = API_Key # your API key
    )
  }, error = function(e) {
    # If there's an error, try to load the file that was likely created
    cat("Metadata writing failed, but attempting to load created file...\n")
    file_path <- file.path(Dir.Data, paste0(filename_temp[i],".nc"))
    if (file.exists(file_path)) {
      cat("Successfully loaded data despite metadata error!\n")
      return(terra::rast(file_path))
    } else {
      stop("Data file was not created: ", e$message)
    }
  })
  
  # Give name to the list
  names(temp_download_list)[i] <- filename_temp[i]
  
  # Checking status of processed files and remove TEMP files
  # Check file sizes first (optional - to see how much space you'll save)
  temp_files <- list.files(Dir.Data, pattern = "TEMP_.*", full.names = TRUE)
  processed_file <- file.path(Dir.Data, paste0(filename_temp[i],".nc"))
  
  cat("File sizes:\n")
  for (file in c(temp_files, processed_file)) {
    if (file.exists(file)) {
      size_mb <- round(file.size(file) / 1024^2, 2)
      cat(basename(file), ":", size_mb, "MB\n")
    }
  }
  
  # Verify your processed file exists and works
  if (file.exists(processed_file)) {
    test_load <- terra::rast(processed_file)
    cat("\nProcessed file loads successfully:", !is.null(test_load), "\n")
    
    # If everything looks good, delete TEMP files
    if (!is.null(test_load)) {
      for (temp_file in temp_files) {
        if (file.exists(temp_file)) {
          file.remove(temp_file)
          cat("Deleted:", basename(temp_file), "\n")
        }
      }
      cat("Cleanup complete! Space saved.\n")
    }
  } else {
    cat("WARNING: Processed file not found. Keep TEMP files for now.\n")
  }
  
}

# saveRDS(temp_download_list,"output/temp_indonesia_2015_2024.rds") # we can't save as RDS

#### Download cumulative precipitation data
filename_prec <- paste0("mean_precipitation_idn_",year_download)
datestart_vec <- paste0(year_download,"-01-01 00:00:00")
datestop_vec <- paste0(year_download,"-12-31 23:00:00")

prec_download_list <- list()

#### Loop over the download years
for (i in seq_len(length(year_download))){
  
  # Download temperature data, put it in a list
  prec_download_list[[i]] <- tryCatch({
    CDownloadS(
      Variable = "total_precipitation", # temperature
      CumulVar = TRUE, # TRUE for precipitation as it is recorded cumulatively
      DataSet = "reanalysis-era5-land", # dataset to download from
      DateStart = datestart_vec[i], # starting date
      DateStop = datestop_vec[i], # end date
      TZone = "Etc/GMT+7", # timezone
      TResolution = "day", # summarising time resolution
      TStep = 1, # timestep of the time resolution
      FUN = "sum", # summary statistics for the aggregation, we look for total
      Extent = admin2_shp, # boundaries (can use shapefiles)
      Dir = Dir.Data, # output directory
      FileName = filename_prec[i], # filename output
      API_User = API_User, # your email address
      API_Key = API_Key, # your API key
      Cores = 4 # going to need more cores to process huge areas
    )
  }, error = function(e) {
    # If there's an error, try to load the file that was likely created
    cat("Metadata writing failed, but attempting to load created file...\n")
    file_path <- file.path(Dir.Data, paste0(filename_prec[i],".nc"))
    if (file.exists(file_path)) {
      cat("Successfully loaded data despite metadata error!\n")
      return(terra::rast(file_path))
    } else {
      stop("Data file was not created: ", e$message)
    }
  })
  
  # Give name to the list
  names(prec_download_list)[i] <- filename_prec[i]
  
  # Checking status of processed files and remove TEMP files
  # Check file sizes first (optional - to see how much space you'll save)
  temp_files <- list.files(Dir.Data, pattern = "TEMP_.*", full.names = TRUE)
  processed_file <- file.path(Dir.Data, paste0(filename_prec[i],".nc"))
  
  cat("File sizes:\n")
  for (file in c(temp_files, processed_file)) {
    if (file.exists(file)) {
      size_mb <- round(file.size(file) / 1024^2, 2)
      cat(basename(file), ":", size_mb, "MB\n")
    }
  }
  
  # Verify your processed file exists and works
  if (file.exists(processed_file)) {
    test_load <- terra::rast(processed_file)
    cat("\nProcessed file loads successfully:", !is.null(test_load), "\n")
    
    # If everything looks good, delete TEMP files
    if (!is.null(test_load)) {
      for (temp_file in temp_files) {
        if (file.exists(temp_file)) {
          file.remove(temp_file)
          cat("Deleted:", basename(temp_file), "\n")
        }
      }
      cat("Cleanup complete! Space saved.\n")
    }
  } else {
    cat("WARNING: Processed file not found. Keep TEMP files for now.\n")
  }
  
}

# use more cores

# saveRDS(prec_download_list,"output/prec_indonesia_2015_2024.rds")

#### Download mean 2m dewpoint temperature data
filename_d_temp <- paste0("mean_d_temperature_idn_",year_download)
datestart_vec <- paste0(year_download,"-01-01 00:00:00")
datestop_vec <- paste0(year_download,"-12-31 23:00:00")

d_temp_download_list <- list()

#### Loop over the download years
# for (i in seq_len(length(year_download))){
for (i in 4:10){
  
  # Download temperature data, put it in a list
  d_temp_download_list[[i]] <- tryCatch({
    CDownloadS(
      Variable = "2m_dewpoint_temperature", # temperature
      DataSet = "reanalysis-era5-land", # dataset to download from
      DateStart = datestart_vec[i], # starting date
      DateStop = datestop_vec[i], # end date
      TZone = "Etc/GMT+7", # timezone
      TResolution = "day", # summarising time resolution
      TStep = 1, # timestep of the time resolution
      FUN = "mean", # summary statistics for the aggregation
      Extent = admin2_shp, # boundaries (can use shapefiles)
      Dir = Dir.Data, # output directory
      FileName = filename_d_temp[i], # filename output
      API_User = API_User, # your email address
      API_Key = API_Key # your API key
    )
  }, error = function(e) {
    # If there's an error, try to load the file that was likely created
    cat("Metadata writing failed, but attempting to load created file...\n")
    file_path <- file.path(Dir.Data, paste0(filename_d_temp[i],".nc"))
    if (file.exists(file_path)) {
      cat("Successfully loaded data despite metadata error!\n")
      return(terra::rast(file_path))
    } else {
      stop("Data file was not created: ", e$message)
    }
  })
  
  # Give name to the list
  names(d_temp_download_list)[i] <- filename_d_temp[i]
  
  # Checking status of processed files and remove TEMP files
  # Check file sizes first (optional - to see how much space you'll save)
  temp_files <- list.files(Dir.Data, pattern = "TEMP_.*", full.names = TRUE)
  processed_file <- file.path(Dir.Data, paste0(filename_d_temp[i],".nc"))
  
  cat("File sizes:\n")
  for (file in c(temp_files, processed_file)) {
    if (file.exists(file)) {
      size_mb <- round(file.size(file) / 1024^2, 2)
      cat(basename(file), ":", size_mb, "MB\n")
    }
  }
  
  # Verify your processed file exists and works
  if (file.exists(processed_file)) {
    test_load <- terra::rast(processed_file)
    cat("\nProcessed file loads successfully:", !is.null(test_load), "\n")
    
    # If everything looks good, delete TEMP files
    if (!is.null(test_load)) {
      for (temp_file in temp_files) {
        if (file.exists(temp_file)) {
          file.remove(temp_file)
          cat("Deleted:", basename(temp_file), "\n")
        }
      }
      cat("Cleanup complete! Space saved.\n")
    }
  } else {
    cat("WARNING: Processed file not found. Keep TEMP files for now.\n")
  }
  
}