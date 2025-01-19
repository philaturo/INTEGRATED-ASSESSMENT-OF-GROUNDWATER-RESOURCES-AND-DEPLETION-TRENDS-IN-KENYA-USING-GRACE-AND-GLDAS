#################################################################################################################gee climate data processing 

2002 temp


// Initialize Earth Engine
// Define the time range for the data collection
var startDate = ee.Date('2002-04-04');
var endDate = ee.Date('2002-12-04');

// Define the Kenya bounding box using the maximum and minimum latitude and longitude values
var kenyaBounds = ee.Geometry.Rectangle([34.0, -5.0, 42.0, 5.0]); // [West, South, East, North]

// Define the dataset (temperature)
var dataset = ee.ImageCollection('ECMWF/ERA5_LAND/HOURLY')
                .filterBounds(kenyaBounds)
                .filterDate(startDate, endDate)
                .select('temperature_2m');

// Convert temperature from Kelvin to Celsius
var kelvinToCelsius = function(image) {
  return image.subtract(273.15).copyProperties(image, ["system:time_start"]);
};

// Apply temperature conversion
var temperature = dataset.map(kelvinToCelsius);

// Generate a grid of points over Kenya within the latitude and longitude bounds
// The spacing controls how many points will be sampled (smaller spacing = more points)
var latMin = -5.0;
var latMax = 5.0;
var lonMin = 34.0;
var lonMax = 42.0;
var spacing = 0.5;  // Define the spacing for the grid points (0.5 degrees in this case)

var latitudes = ee.List.sequence(latMin, latMax, spacing);
var longitudes = ee.List.sequence(lonMin, lonMax, spacing);

// Create grid points (combinations of lat and lon) over the Kenya region
var pointGrid = latitudes.map(function(lat) {
  return longitudes.map(function(lon) {
    return ee.Feature(ee.Geometry.Point([lon, lat]), {'latitude': lat, 'longitude': lon});
  });
}).flatten();

// Define a function to reduce the temperature data to monthly averages at each point
var monthlyAverage = function(start, end) {
  var monthStart = ee.Date(start);
  var monthEnd = ee.Date(end);
  
  // Filter temperature data for the specific month
  var monthlyData = temperature.filterDate(monthStart, monthEnd);
  
  // Reduce the image collection to monthly mean
  var monthlyMean = monthlyData.mean();
  
  // Sample temperature at each grid point and add time information
  var sampledPoints = monthlyMean.reduceRegions({
    collection: ee.FeatureCollection(pointGrid),
    reducer: ee.Reducer.mean(),
    scale: 1000
  });
  
  // Add the date to each feature
  return sampledPoints.map(function(feature) {
    return feature.set('month', monthStart.format('YYYY-MM'));
  });
};

// List of months to iterate over (April 2002 to December 2002)
var months = ee.List([
  ['2002-04-01', '2002-04-30'],
  ['2002-05-01', '2002-05-31'],
  ['2002-06-01', '2002-06-30'],
  ['2002-07-01', '2002-07-31'],
  ['2002-08-01', '2002-08-31'],
  ['2002-09-01', '2002-09-30'],
  ['2002-10-01', '2002-10-31'],
  ['2002-11-01', '2002-11-30'],
  ['2002-12-01', '2002-12-31']
]);

// Apply the monthly average function over the list of months
var monthlyResults = months.map(function(dateRange) {
  var start = ee.Date(ee.List(dateRange).get(0));
  var end = ee.Date(ee.List(dateRange).get(1));
  
  return monthlyAverage(start, end);
});

// Flatten the results into a single FeatureCollection
var finalResults = ee.FeatureCollection(monthlyResults).flatten();

// Export the data to a CSV file
Export.table.toDrive({
  collection: finalResults,
  description: 'Kenya_Temperature_Data_Monthly_2002',
  fileFormat: 'CSV',
  selectors: ['month', 'latitude', 'longitude', 'mean'], // Date, temperature, latitude, longitude
  fileNamePrefix: 'kenya_temperature_data_2002'
});

print('Monthly temperature data download initiated. Check the Tasks tab to run the export.');





###################################################################################################################2003 temp: repeat and modify years 2004-2024


// Initialize Earth Engine
// Define the time range for the data collection (Jan 1, 2003 - Dec 31, 2003)
var startDate = ee.Date('2003-01-01');
var endDate = ee.Date('2003-12-31');

// Define the Kenya bounding box using the maximum and minimum latitude and longitude values
var kenyaBounds = ee.Geometry.Rectangle([34.0, -5.0, 42.0, 5.0]); // [West, South, East, North]

// Define the dataset (temperature)
var dataset = ee.ImageCollection('ECMWF/ERA5_LAND/HOURLY')
                .filterBounds(kenyaBounds)
                .filterDate(startDate, endDate)
                .select('temperature_2m');

// Convert temperature from Kelvin to Celsius
var kelvinToCelsius = function(image) {
  return image.subtract(273.15).copyProperties(image, ["system:time_start"]);
};

// Apply temperature conversion
var temperature = dataset.map(kelvinToCelsius);

// Generate a grid of points over Kenya within the latitude and longitude bounds
// The spacing controls how many points will be sampled (smaller spacing = more points)
var latMin = -5.0;
var latMax = 5.0;
var lonMin = 34.0;
var lonMax = 42.0;
var spacing = 0.5;  // Define the spacing for the grid points (0.5 degrees in this case)

var latitudes = ee.List.sequence(latMin, latMax, spacing);
var longitudes = ee.List.sequence(lonMin, lonMax, spacing);

// Create grid points (combinations of lat and lon) over the Kenya region
var pointGrid = latitudes.map(function(lat) {
  return longitudes.map(function(lon) {
    return ee.Feature(ee.Geometry.Point([lon, lat]), {'latitude': lat, 'longitude': lon});
  });
}).flatten();

// Define a function to reduce the temperature data to monthly averages at each point
var monthlyAverage = function(start, end) {
  var monthStart = ee.Date(start);
  var monthEnd = ee.Date(end);
  
  // Filter temperature data for the specific month
  var monthlyData = temperature.filterDate(monthStart, monthEnd);
  
  // Reduce the image collection to monthly mean
  var monthlyMean = monthlyData.mean();
  
  // Sample temperature at each grid point and add time information
  var sampledPoints = monthlyMean.reduceRegions({
    collection: ee.FeatureCollection(pointGrid),
    reducer: ee.Reducer.mean(),
    scale: 1000
  });
  
  // Add the date to each feature
  return sampledPoints.map(function(feature) {
    return feature.set('month', monthStart.format('YYYY-MM'));
  });
};

// List of months to iterate over (January 2003 to December 2003)
var months = ee.List([
  ['2003-01-01', '2003-01-31'],
  ['2003-02-01', '2003-02-28'],
  ['2003-03-01', '2003-03-31'],
  ['2003-04-01', '2003-04-30'],
  ['2003-05-01', '2003-05-31'],
  ['2003-06-01', '2003-06-30'],
  ['2003-07-01', '2003-07-31'],
  ['2003-08-01', '2003-08-31'],
  ['2003-09-01', '2003-09-30'],
  ['2003-10-01', '2003-10-31'],
  ['2003-11-01', '2003-11-30'],
  ['2003-12-01', '2003-12-31']
]);

// Apply the monthly average function over the list of months
var monthlyResults = months.map(function(dateRange) {
  var start = ee.Date(ee.List(dateRange).get(0));
  var end = ee.Date(ee.List(dateRange).get(1));
  
  return monthlyAverage(start, end);
});

// Flatten the results into a single FeatureCollection
var finalResults = ee.FeatureCollection(monthlyResults).flatten();

// Export the data to a CSV file
Export.table.toDrive({
  collection: finalResults,
  description: 'Kenya_Temperature_Data_Monthly_2003',
  fileFormat: 'CSV',
  selectors: ['month', 'latitude', 'longitude', 'mean'], // Date, temperature, latitude, longitude
  fileNamePrefix: 'kenya_temperature_data_2003'
});

print('Monthly temperature data download initiated. Check the Tasks tab to run the export.');



################################################################################################################## interpolating temp_data(rename years)



# Load necessary libraries
library(sp)          # For spatial data handling
library(gstat)       # For Kriging interpolation
library(dplyr)       # For data manipulation
library(parallel)    # For parallel processing
library(zoo)         # For interpolation functions

# Function to perform Kriging interpolation
kriging_interpolation <- function(data) {
  # Remove rows where mean_temp is NA to create a spatial model
  data_non_na <- data[!is.na(data$mean_temp), ]
  
  if (nrow(data_non_na) < 3) {
    return(data)  # If not enough data points, return as is
  }
  
  # Convert data into a SpatialPointsDataFrame
  coordinates(data_non_na) <- ~ lon + lat
  
  # Build the variogram model
  variogram_model <- variogram(mean_temp ~ 1, data_non_na)
  
  if (nrow(variogram_model) < 2) {
    warning("Not enough data to create a valid variogram. Returning original data.")
    return(data)  # Return original data if variogram cannot be created
  }
  
  # Fit the variogram using a model
  variogram_fit <- tryCatch(
    fit.variogram(variogram_model, model = vgm("Sph")),
    error = function(e) {
      message("Error fitting variogram: ", e$message)
      return(NULL)
    }
  )
  
  if (is.null(variogram_fit)) {
    return(data)  # Return original data if variogram fitting fails
  }

  # Convert entire data to SpatialPointsDataFrame for interpolation
  coordinates(data) <- ~ lon + lat
  
  # Perform Kriging
  data_kriged <- krige(mean_temp ~ 1, data_non_na, newdata = data, model = variogram_fit)
  
  # Replace missing values with kriged predictions
  data$mean_temp[is.na(data$mean_temp)] <- data_kriged$var1.pred[is.na(data$mean_temp)]
  
  return(data)
}

# Function to process the temperature data
process_temperature_data <- function(file) {
  data <- read.csv(file)
  
  # Check for missing values in the mean_temp column
  if (any(is.na(data$mean_temp))) {
    # Apply Kriging interpolation to fill missing values
    data <- kriging_interpolation(data)
  }
  
  # Interpolate remaining NA values using linear interpolation
  data$mean_temp <- na.approx(data$mean_temp, na.rm = FALSE)
  
  # Alternatively, use spline interpolation
  data$mean_temp <- na.spline(data$mean_temp, na.rm = FALSE)
  
  return(data)
}

# Main script to load data and apply interpolation
file_name <- "kenya_temperature_data_2002.csv"

# Use parallel processing to read and process the file
n_cores <- detectCores() - 1  # Use one less than available cores for safety
cl <- makeCluster(n_cores)

# Export necessary functions and libraries to the workers
clusterEvalQ(cl, {
  library(sp)
  library(gstat)
  library(dplyr)
  library(zoo)  # For interpolation functions
})

# Export the data processing function and the kriging function to the cluster
clusterExport(cl, c("process_temperature_data", "kriging_interpolation"))

# Process the file in parallel (though here we are processing one file)
result <- parLapply(cl, list(file_name), process_temperature_data)

# Stop the parallel cluster
stopCluster(cl)

# Combine results into a single data frame
final_data <- bind_rows(result)

# Select the relevant columns: date, lat, lon, and mean_temp
final_data <- final_data %>%
  select(date, lat, lon, mean_temp)

# Save the final data to a CSV file
write.csv(final_data, "kenya_temperature_data_2002_interpolated.csv", row.names = FALSE)

# Print a message indicating successful saving
cat("Data successfully saved to 'kenya_temperature_data_2002_interpolated.csv'.\n")





###################################################################################################################precipitation 2003(repeat for all years)


// Define the time range for the data collection (January 2003 to December 2003)
var startDate = '2003-01-01';
var endDate = '2003-12-31';

// Define Kenya's geographical bounds using latitude and longitude
var kenyaBounds = ee.Geometry.Rectangle([34.0, -5.0, 42.0, 5.0]);

// Define the precipitation dataset (CHIRPS is used for precipitation data)
var dataset = ee.ImageCollection('UCSB-CHG/CHIRPS/PENTAD')
    .filterBounds(kenyaBounds)
    .filterDate(startDate, endDate)
    .select('precipitation');

// Define a function to sample points and extract precipitation data
function samplePrecipitation(image) {
    // Generate a grid of points with spacing of 0.5 degrees over the Kenya bounds
    var latitudes = ee.List.sequence(-5.0, 5.0, 0.5);
    var longitudes = ee.List.sequence(34.0, 42.0, 0.5);

    // Create a list of points
    var points = latitudes.map(function(lat) {
        return longitudes.map(function(lon) {
            return ee.Feature(ee.Geometry.Point([lon, lat]))
                .set('latitude', lat)
                .set('longitude', lon);
        });
    }).flatten();

    // Create a FeatureCollection from the points
    var pointsFC = ee.FeatureCollection(points);

    // Reduce the regions to get mean precipitation values at the specified scale
    var precipValues = image.reduceRegions({
        collection: pointsFC,
        reducer: ee.Reducer.mean(),
        scale: 1000 // 1000 meters scale
    });

    return precipValues.map(function(f) {
        return f.set('precipitation', f.get('mean'))
                 .set('date', image.date().format('YYYY-MM-dd'))
                 .set('latitude', f.get('latitude')) // include latitude
                 .set('longitude', f.get('longitude')); // include longitude
    });
}

// Apply the sampling function to each image in the dataset
var sampledPrecip = dataset.map(samplePrecipitation).flatten();

// Export the results as a CSV file
Export.table.toDrive({
    collection: sampledPrecip,
    description: 'kenya_precipitation_data_2003',
    fileFormat: 'CSV',
    selectors: ['date', 'precipitation', 'latitude', 'longitude'] // include latitude and longitude
});

// Notify the user
print('Precipitation data export initiated. Check your Google Drive for the CSV file.');








################################################################################################################### interpolating precipitation values Kriging & spline


# Load necessary libraries
library(sp)          # For spatial data handling
library(gstat)       # For Kriging interpolation
library(dplyr)       # For data manipulation
library(parallel)    # For parallel processing
library(zoo)         # For interpolation functions

# Function to perform Kriging interpolation
kriging_interpolation <- function(data) {
  # Remove rows where precipitation is NA to create a spatial model
  data_non_na <- data[!is.na(data$precipitation), ]
  
  if (nrow(data_non_na) < 3) {
    return(data)  # If not enough data points, return as is
  }
  
  # Convert data into a SpatialPointsDataFrame
  coordinates(data_non_na) <- ~ longitude + latitude
  
  # Build the variogram model
  variogram_model <- variogram(precipitation ~ 1, data_non_na)
  
  if (nrow(variogram_model) < 2) {
    warning("Not enough data to create a valid variogram. Returning original data.")
    return(data)  # Return original data if variogram cannot be created
  }
  
  # Fit the variogram using a model
  variogram_fit <- tryCatch(
    fit.variogram(variogram_model, model = vgm("Sph")),
    error = function(e) {
      message("Error fitting variogram: ", e$message)
      return(NULL)
    }
  )
  
  if (is.null(variogram_fit)) {
    return(data)  # Return original data if variogram fitting fails
  }

  # Convert entire data to SpatialPointsDataFrame for interpolation
  coordinates(data) <- ~ longitude + latitude
  
  # Perform Kriging
  data_kriged <- krige(precipitation ~ 1, data_non_na, newdata = data, model = variogram_fit)
  
  # Replace missing values with kriged predictions
  data$precipitation[is.na(data$precipitation)] <- data_kriged$var1.pred[is.na(data$precipitation)]
  
  return(data)
}

# Function to process the precipitation data
process_precipitation_data <- function(file) {
  data <- read.csv(file)
  
  # Check for missing values in the precipitation column
  if (any(is.na(data$precipitation))) {
    # Apply Kriging interpolation to fill missing values
    data <- kriging_interpolation(data)
  }
  
  # Interpolate remaining NA values using linear interpolation
  data$precipitation <- na.approx(data$precipitation, na.rm = FALSE)
  
  # Alternatively, use spline interpolation
   data$precipitation <- na.spline(data$precipitation, na.rm = FALSE)
  
  return(data)
}

# Main script to load data and apply interpolation
file_name <- "kenya_precipitation_data_2003.csv"

# Use parallel processing to read and process the file
n_cores <- detectCores() - 1  # Use one less than available cores for safety
cl <- makeCluster(n_cores)

# Export necessary functions and libraries to the workers
clusterEvalQ(cl, {
  library(sp)
  library(gstat)
  library(dplyr)
  library(zoo)  # For interpolation functions
})

# Export the data processing function and the kriging function to the cluster
clusterExport(cl, c("process_precipitation_data", "kriging_interpolation"))

# Process the file in parallel (though here we are processing one file)
result <- parLapply(cl, list(file_name), process_precipitation_data)

# Stop the parallel cluster
stopCluster(cl)

# Combine results into a single data frame
final_data <- bind_rows(result)

# Select the relevant columns: date, latitude, longitude, and precipitation
final_data <- final_data %>%
  select(date, latitude, longitude, precipitation)

# Save the final data to a CSV file
write.csv(final_data, "kenya_precipitation_data_2003_interpolated.csv", row.names = FALSE)

# Print a message indicating successful saving
cat("Data successfully saved to 'kenya_precipitation_data_2003_interpolated.csv'.\n")







##################################################################################################################local soil moisture data processing (CDS)USE api token 


import cdsapi

# Define the dataset and request parameters
dataset = "satellite-soil-moisture"
request = {
    "variable": ["volumetric_surface_soil_moisture"],
    "type_of_sensor": ["combined_passive_and_active"],
    "time_aggregation": ["month_average"],
    "year": [
        "2002", "2003", "2004",
        "2005", "2006", "2007",
        "2008", "2009", "2010",
        "2011", "2012", "2013",
        "2014", "2015", "2016",
        "2017", "2018", "2019",
        "2020", "2021", "2022",
        "2023", "2024"
    ],
    "month": [
        "01", "02", "03",
        "04", "05", "06",
        "07", "08", "09",
        "10", "11", "12"
    ],
    "day": ["01"],
    "type_of_record": ["cdr"],
    "version": ["v202212"]
}

# Initialize the CDS API client
client = cdsapi.Client()

# Download the data
client.retrieve(dataset, request).download()




################################################################################################################# interpolating NA values volumetric soil_moisture data


# Function to process the soil moisture data
process_soil_moisture_data <- function(file) {
  # Load the necessary columns and specify correct data types
  data <- read.csv(file, colClasses = c("numeric", "numeric", "character", "character", "numeric"))
  
  # Convert date column to Date type if needed
  data$date <- as.Date(data$date)
  
  # Check for missing values in the soil_moisture column
  if (any(is.na(data$soil_moisture))) {
    # Apply Kriging interpolation to fill missing values
    data <- kriging_interpolation(data)
  }
  
  # Interpolate remaining NA values using linear interpolation
 # data$soil_moisture <- na.approx(data$soil_moisture, na.rm = FALSE)
  
  # Alternatively, use spline interpolation
  data$soil_moisture <- na.spline(data$soil_moisture, na.rm = FALSE)
  
  return(data)
}



########################

# Main script to load soil moisture data and apply interpolation
file_name <- "kenya_soil_moisture_2003.csv"

# Use parallel processing to read and process the file
n_cores <- detectCores() - 1  # Use one less than available cores for safety
cl <- makeCluster(n_cores)

# Export necessary functions and libraries to the workers
clusterEvalQ(cl, {
  library(sp)
  library(gstat)
  library(dplyr)
  library(zoo)  # For interpolation functions
})

# Export the data processing function and the kriging function to the cluster
clusterExport(cl, c("process_soil_moisture_data", "kriging_interpolation"))

# Process the file in parallel (though here we are processing one file)
result <- parLapply(cl, list(file_name), process_soil_moisture_data)

# Stop the parallel cluster after processing
stopCluster(cl)

# Combine results into a single data frame
final_data <- bind_rows(result)

# Select the relevant columns: date, latitude, longitude, and soil_moisture
final_data <- final_data %>%
  select(date, lat, lon, soil_moisture)

# Save the final interpolated data to a CSV file
write.csv(final_data, "kenya_soil_moisture_2003_interpolated.csv", row.names = FALSE)

# Print a message indicating successful saving
cat("Data successfully saved to 'kenya_soil_moisture_2003_interpolated.csv'.\n")
