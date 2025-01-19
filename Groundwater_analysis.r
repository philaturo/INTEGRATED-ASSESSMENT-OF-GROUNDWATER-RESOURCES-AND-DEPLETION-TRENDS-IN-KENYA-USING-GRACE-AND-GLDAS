#################### groundwater_analysis--- R code to process the groundwater data 


# Load required libraries
library(data.table)

# Function to process and calculate groundwater for a given year
process_groundwater_for_year <- function(year) {
  
  # Read in the soil moisture and GRACE LWE data for the given year
  soil_file <- paste0("predicted_soil_moisture_", year, ".csv")
  grace_file <- paste0("kenya_lwe_", year, ".csv")
  
  tryCatch({
    # Load soil moisture data (volumetric soil moisture)
    soil_data <- fread(soil_file)
    
    # Load GRACE LWE thickness data
    grace_data <- fread(grace_file)
    
    # Merge datasets by lat and lon (ignoring date and set_type)
    merged_data <- merge(soil_data[, .(lat, lon, predicted_soil_moisture)], 
                         grace_data[, .(lat, lon, lwe_thickness)], 
                         by = c("lat", "lon"))
    
    # Calculate groundwater as the difference between LWE and soil moisture
    merged_data[, groundwater := lwe_thickness - predicted_soil_moisture]
    
    # Save the results into a new CSV file with the pattern 'kenya_groundwater_year.csv'
    output_file <- paste0("kenya_groundwater_", year, ".csv")
    fwrite(merged_data[, .(lat, lon, groundwater)], output_file)
    
    cat("Processed and saved groundwater data for year:", year, "\n")
    
  }, error = function(e) {
    cat("Error processing data for year:", year, " - ", e$message, "\n")
  })
}

# List of years to process (excluding 2005 and 2019)
years_to_process <- c(2003:2024)[!c(2003:2024) %in% c(2005, 2019)]

# Loop through the years and process each one
for (year in years_to_process) {
  process_groundwater_for_year(year)
}





#################################################################################### Visualizations(osm)


import geopandas as gpd
import pandas as pd
import matplotlib.pyplot as plt
import contextily as ctx
import os

# List of years for each interval
intervals = {
    "2003-2006": list(range(2003, 2007)),
    "2007-2011": list(range(2007, 2012)),
    "2012-2016": list(range(2012, 2017)),
    "2017-2021": list(range(2017, 2022)),
    "2022-2024": list(range(2022, 2025)),
}

# Function to load groundwater data for a specific year
def load_groundwater_data(year):
    filename = f"kenya_groundwater_{year}.csv"
    if os.path.exists(filename):
        data = pd.read_csv(filename)
        return gpd.GeoDataFrame(data, geometry=gpd.points_from_xy(data.lon, data.lat))
    else:
        print(f"File {filename} not found, skipping...")
        return None

# Function to load and concatenate groundwater data for a given range of years
def load_groundwater_data_for_years(years):
    gdfs = []
    for year in years:
        gdf = load_groundwater_data(year)
        if gdf is not None:
            gdfs.append(gdf)
    if gdfs:
        return pd.concat(gdfs, ignore_index=True)
    else:
        return None

# Function to plot groundwater map for a given interval
def plot_groundwater_map(gdf_groundwater, title, output_filename):
    fig, ax = plt.subplots(figsize=(10, 10))

    # Plot the groundwater data
    gdf_groundwater.plot(column='groundwater', ax=ax, legend=True, cmap='Blues', markersize=5)

    # Add basemap using OpenStreetMap
    try:
        ctx.add_basemap(ax, crs=gdf_groundwater.crs.to_string(), source=ctx.providers.OpenStreetMap.Mapnik)
    except Exception as e:
        print(f"Basemap fetching failed: {e}")

    # Add map elements: title, scale, compass, and legend
    ax.set_title(title, fontsize=15)
    ax.set_axis_off()  # Turn off axis

    # Add scale
    scale_text = "Scale: 0 50 100 km"
    ax.text(0.5, 0.02, scale_text, fontsize=10, transform=ax.transAxes, ha="center")

    # Add compass at the top-right
    ax.annotate("N", xy=(0.97, 0.85), xycoords='axes fraction', fontsize=12, ha='center', va='center')
    ax.annotate("↑", xy=(0.97, 0.78), xycoords='axes fraction', fontsize=15, ha='center', va='center')

    # Save the figure
    plt.savefig(output_filename, dpi=300)
    plt.close()

# Load the Kenya counties shapefile
kenya_shapefile = 'ken_adm.shp'
kenya_boundaries = gpd.read_file(kenya_shapefile)

# Ensure the shapefile has a CRS for plotting (if not, set it)
if kenya_boundaries.crs is None:
    kenya_boundaries.set_crs(epsg=4326, inplace=True)

# Iterate over the intervals, load the data, and plot the maps
for interval, years in intervals.items():
    # Load groundwater data for this interval
    groundwater_data = load_groundwater_data_for_years(years)
    
    # If there is no data, skip to the next interval
    if groundwater_data is None:
        print(f"No data available for {interval}, skipping...")
        continue

    # Ensure the CRS is set to match the boundaries (EPSG 4326)
    groundwater_data.set_crs(epsg=4326, inplace=True)

    # Merge the groundwater data with the Kenya boundaries
    gdf = gpd.sjoin(groundwater_data, kenya_boundaries, how="inner", predicate='within')

    # Plot the map for this interval
    title = f"KENYA GROUNDWATER {interval}"
    output_filename = f"kenya_groundwater_{interval}.png"
    plot_groundwater_map(gdf, title, output_filename)

print("Groundwater maps generated successfully.")





############################################################################################## heat map


import geopandas as gpd
import pandas as pd
import matplotlib.pyplot as plt
import contextily as ctx
import seaborn as sns
import numpy as np
import os

# List of years for each interval
intervals = {
    "2003-2006": list(range(2003, 2007)),
    "2007-2011": list(range(2007, 2012)),
    "2012-2016": list(range(2012, 2017)),
    "2017-2021": list(range(2017, 2022)),
    "2022-2024": list(range(2022, 2025)),
}

# Function to load groundwater data for a specific year
def load_groundwater_data(year):
    filename = f"kenya_groundwater_{year}.csv"
    if os.path.exists(filename):
        data = pd.read_csv(filename)
        return gpd.GeoDataFrame(data, geometry=gpd.points_from_xy(data.lon, data.lat))
    else:
        print(f"File {filename} not found, skipping...")
        return None

# Function to load and concatenate groundwater data for a given range of years
def load_groundwater_data_for_years(years):
    gdfs = []
    for year in years:
        gdf = load_groundwater_data(year)
        if gdf is not None:
            gdfs.append(gdf)
    if gdfs:
        return pd.concat(gdfs, ignore_index=True)
    else:
        return None

# Function to plot groundwater heatmap for a given interval
def plot_groundwater_heatmap(gdf_groundwater, title, output_filename):
    fig, ax = plt.subplots(figsize=(10, 10))

    # Create a density heatmap using seaborn
    if not gdf_groundwater.empty:
        x = gdf_groundwater.geometry.x
        y = gdf_groundwater.geometry.y

        # Create a 2D histogram to compute density
        heatmap, xedges, yedges = np.histogram2d(x, y, bins=30, density=True)

        # Create the heatmap using imshow
        ax.imshow(heatmap.T, origin='lower', cmap='Blues', extent=[xedges[0], xedges[-1], yedges[0], yedges[-1]], alpha=0.6)

    # Add basemap using OpenStreetMap
    try:
        ctx.add_basemap(ax, crs=gdf_groundwater.crs.to_string(), source=ctx.providers.OpenStreetMap.Mapnik)
    except Exception as e:
        print(f"Basemap fetching failed: {e}")

    # Add map elements: title, scale, compass, and legend
    ax.set_title(title, fontsize=15)
    ax.set_axis_off()  # Turn off axis

    # Add scale
    scale_text = "Scale: 0 50 100 km"
    ax.text(0.5, 0.02, scale_text, fontsize=10, transform=ax.transAxes, ha="center")

    # Add compass at the top-right
    ax.annotate("N", xy=(0.97, 0.85), xycoords='axes fraction', fontsize=12, ha='center', va='center')
    ax.annotate("↑", xy=(0.97, 0.78), xycoords='axes fraction', fontsize=15, ha='center', va='center')

    # Save the figure
    plt.savefig(output_filename, dpi=300)
    plt.close()

# Load the Kenya counties shapefile
kenya_shapefile = 'ken_adm.shp'
kenya_boundaries = gpd.read_file(kenya_shapefile)

# Ensure the shapefile has a CRS for plotting (if not, set it)
if kenya_boundaries.crs is None:
    kenya_boundaries.set_crs(epsg=4326, inplace=True)

# Iterate over the intervals, load the data, and plot the maps
for interval, years in intervals.items():
    # Load groundwater data for this interval
    groundwater_data = load_groundwater_data_for_years(years)
    
    # If there is no data, skip to the next interval
    if groundwater_data is None:
        print(f"No data available for {interval}, skipping...")
        continue

    # Ensure the CRS is set to match the boundaries (EPSG 4326)
    groundwater_data.set_crs(epsg=4326, inplace=True)

    # Merge the groundwater data with the Kenya boundaries
    gdf = gpd.sjoin(groundwater_data, kenya_boundaries, how="inner", predicate='within')

    # Plot the heatmap for this interval
    title = f"KENYA GROUNDWATER {interval} (Heatmap)"
    output_filename = f"kenya_groundwater_heatmap_{interval}.png"
    plot_groundwater_heatmap(gdf, title, output_filename)

print("Groundwater heatmaps generated successfully.")




##################################################################################








