#####################################GLDAS_processing

# Load necessary library
library(ncdf4)

# Open the NetCDF file
nc_data <- nc_open("GLDAS_NOAH025_M.A200204.021.nc4")

# Print a summary of the file's contents
print(nc_data)

# Extract basic metadata
file_info <- ncatt_get(nc_data, 0)
print(file_info)

# List variables in the file
variable_names <- names(nc_data$var)
print(variable_names)

# Extract dimension details (latitude, longitude, time, etc.)
dimension_info <- lapply(nc_data$dim, function(dim) {
  list(name = dim$name, units = dim$units, len = dim$len)
})
print(dimension_info)

# Close the NetCDF file
nc_close(nc_data)






###############################################################################extracting and averaging soil_moisture, subeting to Kenya,  interpolating using Kriging

# Load necessary libraries
library(ncdf4)
library(dplyr)
library(parallel)
library(sp)       # Spatial data handling
library(gstat)    # Kriging interpolation

# Function to interpolate missing values using Kriging
kriging_interpolation <- function(data) {
  # Remove rows where avg_soil_moisture is NA to create a spatial model
  data_non_na <- data[!is.na(data$avg_soil_moisture), ]
  
  if (nrow(data_non_na) < 3) {
    return(data)  # If not enough data points, return as is
  }
  
  # Convert data into a SpatialPointsDataFrame
  coordinates(data_non_na) <- ~ lon + lat
  
  # Build the variogram model
  variogram_model <- variogram(avg_soil_moisture ~ 1, data_non_na)
  
  # Fit the variogram using a model
  variogram_fit <- fit.variogram(variogram_model, model = vgm("Sph"))
  
  # Convert entire data to SpatialPointsDataFrame for interpolation
  coordinates(data) <- ~ lon + lat
  
  # Perform Kriging
  data_kriged <- krige(avg_soil_moisture ~ 1, data_non_na, newdata = data, model = variogram_fit)
  
  # Replace missing values with kriged predictions
  data$avg_soil_moisture[is.na(data$avg_soil_moisture)] <- data_kriged$var1.pred[is.na(data$avg_soil_moisture)]
  
  return(data)
}

# Function to process a single file and extract soil moisture efficiently, limited to Kenya region
process_file_chunked <- function(file) {
  # Open the NetCDF file
  nc_data <- nc_open(file)
  
  # Extract latitude, longitude, and time
  lat <- ncvar_get(nc_data, "lat")
  lon <- ncvar_get(nc_data, "lon")
  time <- ncvar_get(nc_data, "time")
  
  # Convert time (in days since a reference date) to actual dates
  time_units <- ncatt_get(nc_data, "time", "units")$value
  reference_date <- as.Date(sub("days since ", "", time_units), format="%Y-%m-%d")
  date <- reference_date + time  # Convert time from days to actual dates
  
  
  
  # Pre-allocate memory for results
  result <- data.frame(lat = rep(lat_kenya, each = nlon),
                       lon = rep(lon_kenya, nlat),
                       date = rep(date, each = nlat * nlon),
                       avg_soil_moisture = numeric(nlat * nlon * length(date)))
  
  # Process the file in chunks to avoid memory overload
  for (i in seq_len(nlat)) {
    # Read in chunks of soil moisture data for each depth layer for the Kenya region
    SoilMoi0_10cm <- ncvar_get(nc_data, "SoilMoi0_10cm_inst", 
                               start = c(kenya_lon_idx[1], kenya_lat_idx[i], 1), 
                               count = c(nlon, 1, -1))
    SoilMoi10_40cm <- ncvar_get(nc_data, "SoilMoi10_40cm_inst", 
                                start = c(kenya_lon_idx[1], kenya_lat_idx[i], 1), 
                                count = c(nlon, 1, -1))
    SoilMoi40_100cm <- ncvar_get(nc_data, "SoilMoi40_100cm_inst", 
                                 start = c(kenya_lon_idx[1], kenya_lat_idx[i], 1), 
                                 count = c(nlon, 1, -1))
    SoilMoi100_200cm <- ncvar_get(nc_data, "SoilMoi100_200cm_inst", 
                                  start = c(kenya_lon_idx[1], kenya_lat_idx[i], 1), 
                                  count = c(nlon, 1, -1))
    
    # Average the soil moisture values across depths
    avg_soil_moisture <- (SoilMoi0_10cm + SoilMoi10_40cm + SoilMoi40_100cm + SoilMoi100_200cm) / 4
    
    # Store the results in the pre-allocated result data frame
    result$avg_soil_moisture[(i - 1) * nlon + seq_len(nlon)] <- as.vector(avg_soil_moisture)
  }
  
  # Close the NetCDF file
  nc_close(nc_data)
  
  # Apply Kriging interpolation to fill missing values
  result <- kriging_interpolation(result)
  
  return(result)
}

# Function to extract data from SpatialPointsDataFrame and ensure lat/lon are included
extract_data_from_spdf <- function(spdf) {
  if (inherits(spdf, "SpatialPointsDataFrame")) {
    data <- spdf@data  # Extract the data frame from the SpatialPointsDataFrame
    if (!all(c("lat", "lon") %in% names(data))) {
      data$lat <- coordinates(spdf)[, 2]  # Extract latitude from coordinates
      data$lon <- coordinates(spdf)[, 1]  # Extract longitude from coordinates
    }
    return(data)
  } else {
    return(NULL)
  }
}

# Example file pattern for 2002 dataset (Adjust as necessary)
files_2002 <- list.files(pattern = "GLDAS_NOAH025_M.A2002[0-9]{2}.021.nc4")

# Use parallel processing to process multiple files at once
n_cores <- detectCores() - 1  # Use one less than available cores for safety
cl <- makeCluster(n_cores)

# Load necessary libraries and export variables to the workers
clusterEvalQ(cl, {
  library(sp)
  library(gstat)
  library(ncdf4)
  library(dplyr)
})

# Export necessary functions and libraries to the workers
clusterExport(cl, c("process_file_chunked", "kriging_interpolation", "variogram", 
                    "fit.variogram", "krige", "vgm", "ncvar_get", "ncatt_get", "nc_close"))

# Define a function to handle errors and collect results
safe_process_file_chunked <- function(file) {
  tryCatch({
    process_file_chunked(file)
  }, error = function(e) {
    message("Error processing file: ", file, "\n", e)
    return(NULL)  # Return NULL in case of error
  })
}

# Process all files in parallel and combine the results into a single data frame
all_data <- parLapply(cl, files_2002, safe_process_file_chunked)
stopCluster(cl)  # Stop the parallel cluster

# Apply the extraction function to each element in all_data
data_frames <- lapply(all_data, extract_data_from_spdf)

# Remove NULL elements from the list
clean_data_frames <- Filter(Negate(is.null), data_frames)

# Check if there are any data frames left
if (length(clean_data_frames) > 0) {
  # Combine the list of data frames into a single data frame
  final_data <- bind_rows(clean_data_frames)
  
  # Save the final data to a CSV file
  write.csv(final_data, "averaged_soil_moisture_kenya_2002_kriging.csv", row.names = FALSE)
  
  # Print a message indicating successful saving
  cat("Data successfully saved to 'averaged_soil_moisture_kenya_2002_kriging.csv'.\n")
} else {
  warning("No valid data frames to combine.")
}



############2003 processing

# Example file pattern for 2003 dataset (Adjust as necessary)
files_2003 <- list.files(pattern = "GLDAS_NOAH025_M.A2003[0-9]{2}.021.nc4")

# Use parallel processing to process multiple files at once
n_cores <- detectCores() - 1  # Use one less than available cores for safety
cl <- makeCluster(n_cores)

# Load necessary libraries and export variables to the workers
clusterEvalQ(cl, {
  library(sp)
  library(gstat)
  library(ncdf4)
  library(dplyr)
})

# Export necessary functions and libraries to the workers
clusterExport(cl, c("process_file_chunked", "kriging_interpolation", "variogram", 
                    "fit.variogram", "krige", "vgm", "ncvar_get", "ncatt_get", "nc_close"))

# Define a function to handle errors and collect results
safe_process_file_chunked <- function(file) {
  tryCatch({
    process_file_chunked(file)
  }, error = function(e) {
    message("Error processing file: ", file, "\n", e)
    return(NULL)  # Return NULL in case of error
  })
}

# Process all files in parallel and combine the results into a single data frame
all_data <- parLapply(cl, files_2003, safe_process_file_chunked)
stopCluster(cl)  # Stop the parallel cluster

# Apply the extraction function to each element in all_data
data_frames <- lapply(all_data, extract_data_from_spdf)

# Remove NULL elements from the list
clean_data_frames <- Filter(Negate(is.null), data_frames)

# Check if there are any data frames left
if (length(clean_data_frames) > 0) {
  # Combine the list of data frames into a single data frame
  final_data <- bind_rows(clean_data_frames)
  
  # Save the final data to a CSV file
  write.csv(final_data, "averaged_soil_moisture_kenya_2003_kriging.csv", row.names = FALSE)
  
  # Print a message indicating successful saving
  cat("Data successfully saved to 'averaged_soil_moisture_kenya_2003_kriging.csv'.\n")
} else {
  warning("No valid data frames to combine.")
}


###########2004 processing


# Load necessary libraries
library(ncdf4)
library(dplyr)
library(parallel)
library(sp)       # Spatial data handling
library(gstat)    # Kriging interpolation

# Function to interpolate missing values using Kriging
kriging_interpolation <- function(data) {
  # Remove rows where avg_soil_moisture is NA to create a spatial model
  data_non_na <- data[!is.na(data$avg_soil_moisture), ]
  
  if (nrow(data_non_na) < 3) {
    return(data)  # If not enough data points, return as is
  }
  
  # Convert data into a SpatialPointsDataFrame
  coordinates(data_non_na) <- ~ lon + lat
  
  # Build the variogram model
  variogram_model <- variogram(avg_soil_moisture ~ 1, data_non_na)
  
  # Fit the variogram using a model
  variogram_fit <- fit.variogram(variogram_model, model = vgm("Sph"))
  
  # Convert entire data to SpatialPointsDataFrame for interpolation
  coordinates(data) <- ~ lon + lat
  
  # Perform Kriging
  data_kriged <- krige(avg_soil_moisture ~ 1, data_non_na, newdata = data, model = variogram_fit)
  
  # Replace missing values with kriged predictions
  data$avg_soil_moisture[is.na(data$avg_soil_moisture)] <- data_kriged$var1.pred[is.na(data$avg_soil_moisture)]
  
  return(data)
}

# Function to process a single file and extract soil moisture efficiently
process_file_chunked <- function(file) {
  # Open the NetCDF file
  nc_data <- nc_open(file)
  
  # Extract latitude, longitude, and time
  lat <- ncvar_get(nc_data, "lat")
  lon <- ncvar_get(nc_data, "lon")
  time <- ncvar_get(nc_data, "time")
  
  # Convert time (in days since a reference date) to actual dates
  time_units <- ncatt_get(nc_data, "time", "units")$value
  reference_date <- as.Date(sub("days since ", "", time_units), format="%Y-%m-%d")
  date <- reference_date + time  # Convert time from days to actual dates
  
  # Get the indices for the Kenya region
  kenya_lat_idx <- which(lat >= -5.0 & lat <= 5.0)
  kenya_lon_idx <- which(lon >= 34.0 & lon <= 42.0)
  
  # Subset lat and lon for Kenya
  lat_kenya <- lat[kenya_lat_idx]
  lon_kenya <- lon[kenya_lon_idx]
  
  # Get the size of the subsetted grid (lat, lon for Kenya)
  nlat <- length(lat_kenya)
  nlon <- length(lon_kenya)
  
  # Pre-allocate memory for results
  result <- data.frame(lat = rep(lat_kenya, each = nlon),
                       lon = rep(lon_kenya, nlat),
                       date = rep(date, each = nlat * nlon),
                       avg_soil_moisture = numeric(nlat * nlon * length(date)))
  
  # Process the file in chunks to avoid memory overload
  for (i in seq_len(nlat)) {
    # Read in chunks of soil moisture data for each depth layer for the Kenya region
    SoilMoi0_10cm <- ncvar_get(nc_data, "SoilMoi0_10cm_inst", 
                               start = c(kenya_lon_idx[1], kenya_lat_idx[i], 1), 
                               count = c(nlon, 1, -1))
    SoilMoi10_40cm <- ncvar_get(nc_data, "SoilMoi10_40cm_inst", 
                                start = c(kenya_lon_idx[1], kenya_lat_idx[i], 1), 
                                count = c(nlon, 1, -1))
    SoilMoi40_100cm <- ncvar_get(nc_data, "SoilMoi40_100cm_inst", 
                                 start = c(kenya_lon_idx[1], kenya_lat_idx[i], 1), 
                                 count = c(nlon, 1, -1))
    SoilMoi100_200cm <- ncvar_get(nc_data, "SoilMoi100_200cm_inst", 
                                  start = c(kenya_lon_idx[1], kenya_lat_idx[i], 1), 
                                  count = c(nlon, 1, -1))
    
    # Average the soil moisture values across depths
    avg_soil_moisture <- (SoilMoi0_10cm + SoilMoi10_40cm + SoilMoi40_100cm + SoilMoi100_200cm) / 4
    
    # Store the results in the pre-allocated result data frame
    result$avg_soil_moisture[(i - 1) * nlon + seq_len(nlon)] <- as.vector(avg_soil_moisture)
  }
  
  # Close the NetCDF file
  nc_close(nc_data)
  
  # Apply Kriging interpolation to fill missing values
  result <- kriging_interpolation(result)
  
  return(result)
}

# Define the files pattern for the 2004 dataset (January to December)
files_2004 <- list.files(pattern = "GLDAS_NOAH025_M.A2004[0-9]{2}.021.nc4")

# Use parallel processing to process multiple files at once
n_cores <- detectCores() - 1  # Use one less than available cores for safety
cl <- makeCluster(n_cores)

# Load necessary libraries and export variables to the workers
clusterEvalQ(cl, {
  library(sp)
  library(gstat)
  library(ncdf4)
  library(dplyr)
})

# Export necessary functions and libraries to the workers
clusterExport(cl, c("process_file_chunked", "kriging_interpolation", "variogram", 
                    "fit.variogram", "krige", "vgm", "ncvar_get", "ncatt_get", "nc_close"))

# Define a function to handle errors and collect results
safe_process_file_chunked <- function(file) {
  tryCatch({
    process_file_chunked(file)
  }, error = function(e) {
    message("Error processing file: ", file, "\n", e)
    return(NULL)  # Return NULL in case of error
  })
}

# Process all files in parallel and combine the results into a single data frame
all_data <- parLapply(cl, files_2004, safe_process_file_chunked)
stopCluster(cl)  # Stop the parallel cluster

# Function to extract data from SpatialPointsDataFrame and ensure lat/lon are included
extract_data_from_spdf <- function(spdf) {
  if (inherits(spdf, "SpatialPointsDataFrame")) {
    data <- spdf@data  # Extract the data frame from the SpatialPointsDataFrame
    if (!all(c("lat", "lon") %in% names(data))) {
      data$lat <- coordinates(spdf)[, 2]  # Extract latitude from coordinates
      data$lon <- coordinates(spdf)[, 1]  # Extract longitude from coordinates
    }
    return(data)
  } else {
    return(NULL)
  }
}

# Apply the extraction function to each element in all_data
data_frames <- lapply(all_data, extract_data_from_spdf)

# Remove NULL elements from the list
clean_data_frames <- Filter(Negate(is.null), data_frames)

# Check if there are any data frames left
if (length(clean_data_frames) > 0) {
  # Combine the list of data frames into a single data frame
  final_data <- bind_rows(clean_data_frames)
  
  # Save the final data to a CSV file
  write.csv(final_data, "averaged_soil_moisture_kenya_2004_kriging.csv", row.names = FALSE)
  
  # Print a message indicating successful saving
  cat("Data successfully saved to 'averaged_soil_moisture_kenya_2004_kriging.csv'.\n")
} else {
  warning("No valid data frames to combine.")
}


############2006 processing

# Example file pattern for 2006 dataset
files_2006 <- list.files(pattern = "GLDAS_NOAH025_M.A2006[0-9]{2}.021.nc4")

# Use parallel processing to process multiple files at once
n_cores <- detectCores() - 1  # Use one less than available cores for safety
cl <- makeCluster(n_cores)

# Load necessary libraries and export variables to the workers
clusterEvalQ(cl, {
  library(sp)
  library(gstat)
  library(ncdf4)
  library(dplyr)
})

# Export necessary functions and libraries to the workers
clusterExport(cl, c("process_file_chunked", "kriging_interpolation", "variogram", 
                    "fit.variogram", "krige", "vgm", "ncvar_get", "ncatt_get", "nc_close"))

# Define a function to handle errors and collect results
safe_process_file_chunked <- function(file) {
  tryCatch({
    process_file_chunked(file)
  }, error = function(e) {
    message("Error processing file: ", file, "\n", e)
    return(NULL)  # Return NULL in case of error
  })
}

# Process all files in parallel and combine the results into a single data frame
all_data <- parLapply(cl, files_2006, safe_process_file_chunked)
stopCluster(cl)  # Stop the parallel cluster

# Apply the extraction function to each element in all_data
data_frames <- lapply(all_data, extract_data_from_spdf)

# Remove NULL elements from the list
clean_data_frames <- Filter(Negate(is.null), data_frames)

# Check if there are any data frames left
if (length(clean_data_frames) > 0) {
  # Combine the list of data frames into a single data frame
  final_data <- bind_rows(clean_data_frames)
  
  # Save the final data to a CSV file
  write.csv(final_data, "averaged_soil_moisture_kenya_2006_kriging.csv", row.names = FALSE)
  
  # Print a message indicating successful saving
  cat("Data successfully saved to 'averaged_soil_moisture_kenya_2006_kriging.csv'.\n")
} else {
  warning("No valid data frames to combine.")
}


##########2007

# Example file pattern for 2007 dataset
files_2007 <- list.files(pattern = "GLDAS_NOAH025_M.A2007[0-9]{2}.021.nc4")

# Use parallel processing to process multiple files at once
n_cores <- detectCores() - 1  # Use one less than available cores for safety
cl <- makeCluster(n_cores)

# Load necessary libraries and export variables to the workers
clusterEvalQ(cl, {
  library(sp)
  library(gstat)
  library(ncdf4)
  library(dplyr)
})

# Export necessary functions and libraries to the workers
clusterExport(cl, c("process_file_chunked", "kriging_interpolation", "variogram", 
                    "fit.variogram", "krige", "vgm", "ncvar_get", "ncatt_get", "nc_close"))

# Define a function to handle errors and collect results
safe_process_file_chunked <- function(file) {
  tryCatch({
    process_file_chunked(file)
  }, error = function(e) {
    message("Error processing file: ", file, "\n", e)
    return(NULL)  # Return NULL in case of error
  })
}

# Process all files in parallel and combine the results into a single data frame
all_data <- parLapply(cl, files_2007, safe_process_file_chunked)
stopCluster(cl)  # Stop the parallel cluster

# Apply the extraction function to each element in all_data
data_frames <- lapply(all_data, extract_data_from_spdf)

# Remove NULL elements from the list
clean_data_frames <- Filter(Negate(is.null), data_frames)

# Check if there are any data frames left
if (length(clean_data_frames) > 0) {
  # Combine the list of data frames into a single data frame
  final_data <- bind_rows(clean_data_frames)
  
  # Save the final data to a CSV file
  write.csv(final_data, "averaged_soil_moisture_kenya_2007_kriging.csv", row.names = FALSE)
  
  # Print a message indicating successful saving
  cat("Data successfully saved to 'averaged_soil_moisture_kenya_2007_kriging.csv'.\n")
} else {
  warning("No valid data frames to combine.")
}


#########2008 processing

# Example file pattern for 2008 dataset
files_2008 <- list.files(pattern = "GLDAS_NOAH025_M.A2008[0-9]{2}.021.nc4")

# Use parallel processing to process multiple files at once
n_cores <- detectCores() - 1  # Use one less than available cores for safety
cl <- makeCluster(n_cores)

# Load necessary libraries and export variables to the workers
clusterEvalQ(cl, {
  library(sp)
  library(gstat)
  library(ncdf4)
  library(dplyr)
})

# Export necessary functions and libraries to the workers
clusterExport(cl, c("process_file_chunked", "kriging_interpolation", "variogram", 
                    "fit.variogram", "krige", "vgm", "ncvar_get", "ncatt_get", "nc_close"))

# Define a function to handle errors and collect results
safe_process_file_chunked <- function(file) {
  tryCatch({
    process_file_chunked(file)
  }, error = function(e) {
    message("Error processing file: ", file, "\n", e)
    return(NULL)  # Return NULL in case of error
  })
}

# Process all files in parallel and combine the results into a single data frame
all_data <- parLapply(cl, files_2008, safe_process_file_chunked)
stopCluster(cl)  # Stop the parallel cluster

# Apply the extraction function to each element in all_data
data_frames <- lapply(all_data, extract_data_from_spdf)

# Remove NULL elements from the list
clean_data_frames <- Filter(Negate(is.null), data_frames)

# Check if there are any data frames left
if (length(clean_data_frames) > 0) {
  # Combine the list of data frames into a single data frame
  final_data <- bind_rows(clean_data_frames)
  
  # Save the final data to a CSV file
  write.csv(final_data, "averaged_soil_moisture_kenya_2008_kriging.csv", row.names = FALSE)
  
  # Print a message indicating successful saving
  cat("Data successfully saved to 'averaged_soil_moisture_kenya_2008_kriging.csv'.\n")
} else {
  warning("No valid data frames to combine.")
}



##################2009 processing


# Example file pattern for 2009 dataset
files_2009 <- list.files(pattern = "GLDAS_NOAH025_M.A2009[0-9]{2}.021.nc4")

# Use parallel processing to process multiple files at once
n_cores <- detectCores() - 1  # Use one less than available cores for safety
cl <- makeCluster(n_cores)

# Load necessary libraries and export variables to the workers
clusterEvalQ(cl, {
  library(sp)
  library(gstat)
  library(ncdf4)
  library(dplyr)
})

# Export necessary functions and libraries to the workers
clusterExport(cl, c("process_file_chunked", "kriging_interpolation", "variogram", 
                    "fit.variogram", "krige", "vgm", "ncvar_get", "ncatt_get", "nc_close"))

# Define a function to handle errors and collect results
safe_process_file_chunked <- function(file) {
  tryCatch({
    process_file_chunked(file)
  }, error = function(e) {
    message("Error processing file: ", file, "\n", e)
    return(NULL)  # Return NULL in case of error
  })
}

# Process all files in parallel and combine the results into a single data frame
all_data <- parLapply(cl, files_2009, safe_process_file_chunked)
stopCluster(cl)  # Stop the parallel cluster

# Apply the extraction function to each element in all_data
data_frames <- lapply(all_data, extract_data_from_spdf)

# Remove NULL elements from the list
clean_data_frames <- Filter(Negate(is.null), data_frames)

# Check if there are any data frames left
if (length(clean_data_frames) > 0) {
  # Combine the list of data frames into a single data frame
  final_data <- bind_rows(clean_data_frames)
  
  # Save the final data to a CSV file
  write.csv(final_data, "averaged_soil_moisture_kenya_2009_kriging.csv", row.names = FALSE)
  
  # Print a message indicating successful saving
  cat("Data successfully saved to 'averaged_soil_moisture_kenya_2009_kriging.csv'.\n")
} else {
  warning("No valid data frames to combine.")
}



############# repeat same code and update file name and year for processing from 2010-2024


##########################################visualization plots


# Load necessary libraries
library(data.table)
library(ggplot2)
library(gridExtra)
library(lubridate)
library(sf)       # For handling spatial data
library(raster)   # For raster data operations
library(dplyr)    # For data manipulation
library(viridis)  # For color scales

# Define the function to read and combine CSV files, skipping missing years
read_combined_data <- function(years) {
  file_paths <- paste0("averaged_soil_moisture_kenya_", years, "_kriging.csv")
  existing_files <- file_paths[file.exists(file_paths)]
  
  # Extract years from existing files
  existing_years <- as.integer(gsub("[^0-9]", "", sub("averaged_soil_moisture_kenya_", "", gsub("_kriging.csv", "", existing_files))))
  
  # Filter out years not present in the files
  years <- years[years %in% existing_years]
  
  # Read data only for existing files
  file_paths <- paste0("averaged_soil_moisture_kenya_", years, "_kriging.csv")
  data_list <- lapply(file_paths, function(file) {
    tryCatch({
      fread(file)  # Use fread for faster reading
    }, error = function(e) {
      message("Error reading file: ", file, "\n", e)
      return(NULL)  # Return NULL in case of error
    })
  })
  
  combined_data <- rbindlist(data_list, use.names = TRUE, fill = TRUE)
  combined_data$year <- as.integer(gsub("[^0-9]", "", combined_data$year))
  return(combined_data)
}

# Load data for the full range of years, skipping missing years
data <- read_combined_data(2002:2024)

# Convert date column to Date type if not already
data$date <- as.Date(data$date)

# Add a year column to the data
data$year <- year(data$date)

# Load Kenya shapefile
kenya_shape <- st_read("kenya.shp")

# Ensure both datasets are in the same CRS (coordinate reference system)
kenya_shape <- st_transform(kenya_shape, st_crs(kenya_shape))

# Convert data to an sf object
data_sf <- st_as_sf(data, coords = c("lon", "lat"), crs = st_crs(kenya_shape))

# Clip data to Kenya boundary strictly (remove points outside the boundary)
data_clipped <- st_intersection(data_sf, kenya_shape)

# Define year ranges for visualization
ranges <- list(
  "2002-2006" = 2002:2006,
  "2007-2011" = 2007:2011,
  "2012-2016" = 2012:2016,
  "2017-2021" = 2017:2021,
  "2022-2024" = 2022:2024
)

# Function to filter data by year range
filter_by_range <- function(data, year_range) {
  data %>% filter(year %in% year_range)
}

# Function to create the soil moisture plot
plot_soil_moisture <- function(data, title) {
  ggplot() +
    geom_sf(data = kenya_shape, fill = "lightgray", color = "black") +  # Kenya boundary in light gray
    geom_sf(data = data, aes(color = avg_soil_moisture), size = 0.5) +  # Plot the soil moisture data
    scale_color_viridis_c(name = "Average Soil Moisture", option = "D") +  # Use a bright color palette
    coord_sf(xlim = st_bbox(kenya_shape)[c("xmin", "xmax")],  # Limit x-axis to Kenya's bounding box
             ylim = st_bbox(kenya_shape)[c("ymin", "ymax")]) +  # Limit y-axis to Kenya's bounding box
    labs(title = title, x = NULL, y = NULL) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 10),
      legend.position = "right",  # Legend placed on the right
      legend.key = element_rect(fill = "white", color = "black"),
      legend.key.size = unit(0.5, "cm")  # Adjust the size of legend keys
    )
}

# Create and save plots for each range
for (range_name in names(ranges)) {
  filtered_data <- filter_by_range(data_clipped, ranges[[range_name]])
  
  # Generate the plot for the range
  plot <- plot_soil_moisture(filtered_data, paste("Soil Moisture in Kenya (", range_name, ")", sep = ""))
  
  # Save the plot as a PNG file
  file_name <- paste0("soil_moisture_", range_name, ".png")
  ggsave(filename = file_name, plot = plot, width = 10, height = 8, dpi = 300)
  cat("Plot saved as: ", file_name, "\n")
}


####################summary statistics for years


# Load necessary libraries
library(dplyr)

# Function to calculate summary statistics
calculate_summary_stats <- function(data, year_range) {
  data_filtered <- filter_by_range(data, year_range)
  
  # Calculate summary statistics
  summary_stats <- data_filtered %>%
    summarise(
      mean_soil_moisture = mean(avg_soil_moisture, na.rm = TRUE),
      median_soil_moisture = median(avg_soil_moisture, na.rm = TRUE),
      sd_soil_moisture = sd(avg_soil_moisture, na.rm = TRUE),
      min_soil_moisture = min(avg_soil_moisture, na.rm = TRUE),
      max_soil_moisture = max(avg_soil_moisture, na.rm = TRUE)
    )
  
  return(summary_stats)
}

# Function to print summary statistics for each year range
print_summary_statistics <- function(data, ranges) {
  for (range_name in names(ranges)) {
    summary_stats <- calculate_summary_stats(data, ranges[[range_name]])
    
    # Print summary for the current range
    cat("Summary Statistics for Soil Moisture (", range_name, "):\n", sep = "")
    print(summary_stats)
    cat("\n")
  }
}

# Print summary statistics for all ranges
print_summary_statistics(data_clipped, ranges)



####################################trend analysis using Seasonal and Trend decomposition using Loess (STL) method, Mann-Kendall test, and Sen's slope



# Load necessary libraries
library(data.table)
library(dplyr)
library(ggplot2)
library(Kendall)   # For Mann-Kendall test
library(trend)     # For Sen's Slope analysis
library(stats)     # For STL decomposition

# Function to read and combine CSV files, skipping 2005
read_combined_data <- function(years) {
  file_paths <- paste0("averaged_soil_moisture_kenya_", years, "_kriging.csv")
  existing_files <- file_paths[file.exists(file_paths)]
  
  # Read data only for existing files
  data_list <- lapply(existing_files, fread)
  
  # Combine data
  combined_data <- bind_rows(data_list)

  # Create a year column based on existing files
  years_found <- years[years %in% as.integer(gsub("[^0-9]", "", 
                                    sub("averaged_soil_moisture_kenya_", "", 
                                    gsub("_kriging.csv", "", existing_files))))]

  combined_data$year <- rep(years_found, each = nrow(combined_data) / length(years_found))
  
  return(combined_data)
}

# Load data for the years, skipping 2005
years <- 2002:2024
data <- read_combined_data(years[-4])  # Exclude 2005

# Aggregate data to yearly averages
yearly_soil_moisture <- data %>%
  group_by(year) %>%
  summarize(mean_soil_moisture = mean(avg_soil_moisture, na.rm = TRUE))

# Create a time series object for avg_soil_moisture
soil_moisture_ts <- ts(yearly_soil_moisture$mean_soil_moisture, start = 2002, frequency = 1)

# Perform Mann-Kendall test
mk_test <- MannKendall(soil_moisture_ts)
print(mk_test)

# Perform Sen's Slope analysis
sen_slope <- sens.slope(yearly_soil_moisture$mean_soil_moisture)
print(sen_slope)

# Check for sufficient data and periodicity for STL decomposition
if (length(soil_moisture_ts) >= 2 && frequency(soil_moisture_ts) > 1) {
  soil_moisture_stl <- stl(soil_moisture_ts, s.window = "periodic")
  plot(soil_moisture_stl)
} else {
  cat("Not enough data for STL decomposition or the series is not periodic. Skipping this step.\n")
}

# Prepare data for visualization
trend_data <- data.frame(
  year = yearly_soil_moisture$year,
  mean_soil_moisture = yearly_soil_moisture$mean_soil_moisture,
  type = "Observed"
)

# Plot the observed data with improved visualization
ggplot(trend_data, aes(x = year, y = mean_soil_moisture)) +
  geom_point(size = 3, shape = 19, color = "blue") +  # Points for observed data
  geom_line(size = 1, color = "blue") +  # Line connecting the points
  labs(
    title = "Trend in Average Soil Moisture (2002-2024)",
    x = "Year",
    y = "Mean Soil Moisture (units)"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5, color = "darkgreen"),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 14),
    panel.border = element_rect(color = "black", fill = NA, size = 1)
  )

# Save the plot as a PNG file
ggsave("soil_moisture_trend.png", width = 12, height = 8, dpi = 300)


