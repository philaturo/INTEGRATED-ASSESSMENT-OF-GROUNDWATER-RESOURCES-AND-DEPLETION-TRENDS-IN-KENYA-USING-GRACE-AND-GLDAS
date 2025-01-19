#########################################validation code

# Define the years for analysis, excluding 2005 and 2019
years <- 2003:2024
years <- years[!years %in% c(2005, 2019)]  # Exclude 2005 and 2019

# Initialize the list to store groundwater data
groundwater_data_list <- list()

# Loop through each year to read groundwater data
for (year in years) {
  tryCatch({
    # Construct the file name for the year
    filename <- paste0("kenya_groundwater_", year, ".csv")
    
    # Check if the file exists before attempting to read it
    if (!file.exists(filename)) {
      warning(paste("File not found for year", year))
    } else {
      # Read the data
      year_data <- read.csv(filename)
      
      # Check if the expected columns exist (lat, lon, groundwater)
      if (!all(c("lat", "lon", "groundwater") %in% colnames(year_data))) {
        warning(paste("Missing expected columns in file:", filename))
      } else {
        # Store the data for the current year in the list
        groundwater_data_list[[as.character(year)]] <- year_data
      }
    }
  }, error = function(e) {
    warning(paste("Error reading file for year", year, ":", e$message))
  })
}

# Check if the data is loaded correctly (for one year as a sample)
if (length(groundwater_data_list) > 0) {
  str(groundwater_data_list[[1]])  # Check structure of the first year's data
} else {
  print("No groundwater data was loaded.")
}

# Proceed with further analysis (e.g., calculating mean groundwater for each year)
groundwater_trends <- sapply(groundwater_data_list, function(data) {
  if (is.null(data)) {
    return(NA)  # Skip years with no data
  } else {
    # Calculate mean groundwater for the year
    return(mean(data$groundwater, na.rm = TRUE))  # Calculate mean groundwater
  }
})

# Print the calculated groundwater trends
print("Groundwater Trends:")
print(groundwater_trends)

# Install and load required packages for trend analysis
if (!require(trend)) install.packages("trend", dependencies = TRUE)
library(trend)

# Sen's Slope estimation (using the "trend" package for Sen's slope estimation)
sen_slope_result <- tryCatch({
  sens.slope(groundwater_trends, x = as.numeric(names(groundwater_trends)))  # Use sens.slope from the 'trend' package
}, error = function(e) {
  print(paste("Error with sens.slope:", e$message))
  return(NULL)
})

# Display Mann-Kendall Test Results
print("Mann-Kendall Test Results:")
print(mk_result)

# Display Sen's Slope Results
print("Sen's Slope Results:")
print(sen_slope_result)

# Install and load required packages for visualization
if (!require(ggplot2)) install.packages("ggplot2", dependencies = TRUE)
library(ggplot2)

# Prepare a data frame for visualization
trend_data <- data.frame(
  year = as.numeric(names(groundwater_trends)),  # Extract years
  mean_groundwater = groundwater_trends
)

# Create a plot of groundwater trends over the years
plot_groundwater_trends <- ggplot(trend_data, aes(x = year, y = mean_groundwater)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 3) +
  theme_minimal() +
  labs(
    title = "Groundwater Trends (2003-2024)",
    x = "Year",
    y = "Mean Groundwater Level",
    caption = paste("Mann-Kendall Tau: ", round(mk_result$tau, 2), 
                    ", p-value: ", round(mk_result$sl, 5))
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save the plot as a PNG file
ggsave("groundwater_trends_plot.png", plot = plot_groundwater_trends, width = 10, height = 6, dpi = 300)

# Display the plot
print(plot_groundwater_trends)

# Initialize GRACE data list
grace_data_list <- list()

# Loop through each year to read GRACE LWE data
for (year in years) {
  tryCatch({
    # Construct the file name for GRACE LWE data
    filename <- paste0("kenya_lwe_", year, ".csv")
    
    # Check if the file exists before attempting to read it
    if (!file.exists(filename)) {
      warning(paste("File not found for GRACE LWE in year", year))
    } else {
      # Read the data
      year_grace_data <- read.csv(filename)
      
      # Check if the expected columns exist (lat, lon, lwe_thickness)
      if (!all(c("lat", "lon", "lwe_thickness") %in% colnames(year_grace_data))) {
        warning(paste("Missing expected columns in file:", filename))
      } else {
        # Store the GRACE LWE data for the current year in the list
        grace_data_list[[as.character(year)]] <- year_grace_data
      }
    }
  }, error = function(e) {
    warning(paste("Error reading GRACE data for year", year, ":", e$message))
  })
}

# Calculate mean LWE thickness for each year
grace_trends <- sapply(grace_data_list, function(data) {
  if (is.null(data)) {
    return(NA)  # Skip years with no data
  } else {
    # Calculate mean LWE thickness for the year
    return(mean(data$lwe_thickness, na.rm = TRUE))  # Calculate mean LWE thickness
  }
})

# Display the calculated GRACE trends
print("GRACE LWE Trends:")
print(grace_trends)

# Prepare a data frame for GRACE trends visualization
grace_trend_data <- data.frame(
  year = as.numeric(names(grace_trends)),
  mean_lwe_thickness = grace_trends
)

# Create a plot of GRACE LWE thickness trends over the years
plot_grace_trends <- ggplot(grace_trend_data, aes(x = year, y = mean_lwe_thickness)) +
  geom_line(color = "green", size = 1) +
  geom_point(color = "orange", size = 3) +
  theme_minimal() +
  labs(
    title = "GRACE LWE Thickness Trends (2003-2024)",
    x = "Year",
    y = "Mean LWE Thickness",
    caption = "Data from GRACE"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save the GRACE plot as a PNG file
ggsave("grace_trends_plot.png", plot = plot_grace_trends, width = 10, height = 6, dpi = 300)

# Display the plot
print(plot_grace_trends)

# Install and load cowplot for combining plots
if (!require(cowplot)) install.packages("cowplot", dependencies = TRUE)
library(cowplot)

# Combine the two plots for comparison
combined_plot <- plot_grid(plot_groundwater_trends, plot_grace_trends, ncol = 1)

# Save the combined plot as a PNG file
ggsave("combined_trends_plot.png", plot = combined_plot, width = 10, height = 12, dpi = 300)

# Display the combined plot
print(combined_plot)







#######################################residual analysis component



#code on residual and correlation analysis


# Install necessary libraries if not already installed
if (!require(corrplot)) install.packages("corrplot", dependencies = TRUE)
if (!require(ggplot2)) install.packages("ggplot2", dependencies = TRUE)

library(corrplot)
library(ggplot2)

# ----------------------------------------
# 1. Residual Analysis for Groundwater Trends
# ----------------------------------------

# Predicted values based on Sen's slope model (using the Sen's slope estimate and intercept)
# We will use a simple linear regression model based on Sen's slope (derived earlier)
sen_slope <- 1.069048
intercept <- mean(groundwater_trends) - sen_slope * mean(as.numeric(names(groundwater_trends)))  # estimated intercept

# Predicted groundwater levels
predicted_groundwater <- sen_slope * as.numeric(names(groundwater_trends)) + intercept

# Residuals for the groundwater trend
residuals_groundwater <- groundwater_trends - predicted_groundwater

# Create a residual plot to visually inspect the residuals
ggplot(data.frame(year = as.numeric(names(groundwater_trends)), residuals = residuals_groundwater), aes(x = year, y = residuals)) +
  geom_point(color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals for Groundwater Trends", x = "Year", y = "Residuals") +
  theme_minimal()

# Perform a normality test for residuals (e.g., Shapiro-Wilk test)
shapiro_test_groundwater <- shapiro.test(residuals_groundwater)
print(paste("Shapiro-Wilk test for normality of residuals (p-value):", round(shapiro_test_groundwater$p.value, 4)))

# ----------------------------------------------------
# 2. Residual Analysis for GRACE LWE Trends
# ----------------------------------------------------

# Predicted values based on Sen's slope model for GRACE LWE
sen_slope_grace <- 1.069048  # Assuming the same slope for simplicity (you could model separately)
intercept_grace <- mean(grace_trends) - sen_slope_grace * mean(as.numeric(names(grace_trends)))  # estimated intercept

# Predicted GRACE LWE levels
predicted_grace <- sen_slope_grace * as.numeric(names(grace_trends)) + intercept_grace

# Residuals for the GRACE LWE trend
residuals_grace <- grace_trends - predicted_grace

# Create a residual plot to visually inspect the residuals
ggplot(data.frame(year = as.numeric(names(grace_trends)), residuals = residuals_grace), aes(x = year, y = residuals)) +
  geom_point(color = "green") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals for GRACE LWE Trends", x = "Year", y = "Residuals") +
  theme_minimal()

# Perform a normality test for residuals (e.g., Shapiro-Wilk test)
shapiro_test_grace <- shapiro.test(residuals_grace)
print(paste("Shapiro-Wilk test for normality of residuals (p-value):", round(shapiro_test_grace$p.value, 4)))

# ----------------------------------------
# 3. Correlation Analysis between Groundwater and GRACE LWE
# ----------------------------------------

# Compute the Pearson correlation coefficient between groundwater trends and GRACE LWE trends
correlation_result <- cor(groundwater_trends, grace_trends, method = "pearson")
print(paste("Pearson Correlation Coefficient between Groundwater Trends and GRACE LWE Trends:", round(correlation_result, 4)))

# Visualize the relationship between groundwater and GRACE trends
correlation_plot <- ggplot(data.frame(groundwater = groundwater_trends, grace_lwe = grace_trends), aes(x = groundwater, y = grace_lwe)) +
  geom_point(color = "purple") +
  geom_smooth(method = "lm", color = "black", se = FALSE) +
  labs(title = "Correlation between Groundwater and GRACE LWE Trends", x = "Groundwater Trends", y = "GRACE LWE Trends") +
  theme_minimal()

# Display the correlation plot
print(correlation_plot)

# ----------------------------------------
# 4. Combine Plots for Final Output (Optional)
# ----------------------------------------

# You can also save the plots to files if needed
ggsave("residuals_groundwater.png", width = 8, height = 6)
ggsave("residuals_grace.png", width = 8, height = 6)
ggsave("correlation_plot.png", width = 8, height = 6)

# Save the combined plot of groundwater trends and GRACE LWE trends
ggsave("combined_groundwater_grace_trends.png", combined_plot, width = 10, height = 8)




