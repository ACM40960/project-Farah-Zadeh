#Load required libraries
library(data.table)
library(readr)
library(dplyr)
library(purrr)
library(ggplot2)
library(sf)
library(stringr)
library(dplyr)
library(readr)
library(raster)
library(ncdf4)
library(chron)
library(lattice)
library(RColorBrewer)
library(svglite)
library(viridis)

################################################################################
########################### Custom theme ######################################
################################################################################

# Create a function for plot themes
custom_theme <- function() {
  theme(
    # Background
    plot.background = element_rect(fill = "gray100"), # border for the whole plot area
    
    # Title and subtitle
    plot.title = element_text(color = "navyblue", face = "bold", size = 16),
    plot.subtitle = element_text(color = "navyblue", size = 12),
    
    # Axis titles and text
    axis.title.x = element_text(color = "navyblue", size = 14),
    axis.title.y = element_text(color = "navyblue", size = 14),
    axis.text.x = element_text(color = "navyblue"),
    axis.text.y = element_text(color = "navyblue"),
    
    # Panel background and grid lines
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "darkorange", linewidth = 0.1),
    panel.grid.minor = element_line(color = "darkorange", linewidth = 0.05),
    
    # Border for the panel
    panel.border = element_rect(color = "navyblue", fill = NA, linewidth = 0.8) # border for the plotting panel
  )
}


################################################################################
######################         Data Curation            ########################
################################################################################

# Read the saved RDS file back into a variable
sst_obs <- readRDS("data/ocean_data2.rds")

# Remove records with missing values eg: -1000 and -1.8
sst_obs <- sst_obs %>%
  filter(sst_vec_long != -1000)

# Filter data with tolerance for removing values ~-1.8
tolerance <- 0.00001
sst_obs <- sst_obs %>%
  filter(!(sst_vec_long > -1.8 - tolerance & sst_vec_long < -1.8 + tolerance))

# Rename columns and transform Date
sst_obs <- sst_obs %>%
  rename(
    Longitude = Var1,
    Latitude = Var2,
    DateValue = Var3,
    SST = sst_vec_long
  )

# Convert to date field 
sst_obs$Date <- as.Date(sst_obs$DateValue, origin = "1870-01-01")
sst_obs$DateValue <- NULL

################################################################################
##################        Reducing data points.       ##########################
################################################################################
# Filter out the data points on longitude and latitude values

# Determine latitude and longitude ranges
lat_range <- range(sst_obs$Latitude)
lon_range <- range(sst_obs$Longitude)

# Create sequences of latitude and longitude
lat_seq <- seq(lat_range[1], lat_range[2], by = 15)
lon_seq <- seq(lon_range[1], lon_range[2], by = 10)

# Filter the data based on latitude and longitude sequences
filtered_ocean_data <- sst_obs %>% 
  filter(Latitude %in% lat_seq & Longitude %in% lon_seq)

# Create a unique station identifier
filtered_ocean_data <- filtered_ocean_data %>%
  mutate(STATION = paste(Latitude, Longitude, sep = "_"))

# Extract year from Date
filtered_ocean_data$Year <- as.numeric(format(filtered_ocean_data$Date, "%Y"))

# Filterrecords based on year
filtered_ocean_data <- filtered_ocean_data %>%
  filter(Year>1940 & Year<2023)

length(unique(filtered_ocean_data$STATION))

# Calculate the count of stations for each year
#loc_counts <- filtered_ocean_data %>%
#  group_by(Year) %>%
#  summarise(loc_count = n_distinct(STATION))

# Plot the number of stations vs year count
# ggplot(loc_counts, aes(x = Year, y = loc_count)) +
#  geom_line() +
#  labs(
#    title = "Number of Stations vs Year",
#    x = "Year",
#    y = "Number of Stations"
#  ) +
#  theme_bw()

################################################################################

# Taking evenly distributed 100 points from the ocean
#Set seed
set.seed(123)
# Get the list of 200 evenly distributed station IDs
evenly_distributed_loc <- sample(unique(filtered_ocean_data$STATION), 100)

# Filter consistent_stations for the selected stations
selected_oceans_loc <- filtered_ocean_data %>%
  filter(STATION %in% evenly_distributed_loc)

# View the dimensions of the selected data
dim(selected_oceans_loc) # dimension 85740 X  6

# Save the data frame as an RDS file
saveRDS(selected_oceans_loc, file = "data/ocean_subset.rds")

################################################################################
####################    Global Ocean SST trends             ####################
################################################################################

selected_oceans_loc <- readRDS("data/ocean_subset.rds")

length(unique(selected_oceans_loc$STATION))

# Calculate yearly average SST for each lat-lon combination
global_ocean_data_avg <- selected_oceans_loc %>%
  group_by(Year) %>%
  summarise(
    SSTAVG = mean(SST, na.rm = TRUE),
    SSTSD = sd(SST, na.rm = TRUE) / sqrt(n())  # Calculate standard error
  )

# Plot the SST average with error bars
p <- ggplot(global_ocean_data_avg, aes(x = Year, y = SSTAVG)) +
  geom_line(aes(color = SSTAVG), size = .8) +
  geom_errorbar(aes(ymin = SSTAVG - SSTSD, ymax = SSTAVG + SSTSD), 
                width = 0.2, color = "grey60", alpha = 0.5) +
  
  # Moving average
  geom_smooth(aes(group = 1), method = "loess", color = "red", 
              linetype = "dashed", se = FALSE, size = .8) +
  scale_color_viridis(option = "B", guide = guide_colorbar(title = "Sea Surface Temp")) +
  custom_theme() +
  labs(
    title = "Yearly Average Sea Surface Temperature",
    subtitle = "Based on yearly temperature averages",
    x = "Year",
    y = "Average SST"
  )
# Save the plot using ggsave
ggsave(filename="images/SSTYear.svg", plot=p, width=7, height=5)

# Display plot
print(p)

#finding decadal average
global_ocean_dec_avg <- global_ocean_data_avg %>%
  mutate(Decade = floor(Year / 10) * 10) %>%  # Extracting the decade
  group_by(Decade) %>%
  summarise(decadal_avg_SSTAVG = mean(SSTAVG, na.rm = TRUE))

#Decadal Average of Global Ocean-Surface Temperature
ggplot(global_ocean_dec_avg, aes(x = Decade, y = decadal_avg_SSTAVG)) +
  geom_line(group=1, color = "darkorange", size=1.2) +  # Ensure the lines connect across all points
  geom_point() +
  custom_theme() +
  labs(title = "Decadal Average of Global Land-Surface Temperature",
       x = "Decade",
       y = "Decadal Average Temperature (°C)")

# Baseline for gloabl ocean anomaly calculations 
reference_avg <- global_ocean_data_avg %>%
  filter(Year >= 1951 & Year <= 1980) %>%
  summarise(reference_SSTAVG = mean(SSTAVG))

# Calculate anomaly
global_ocean_data_avg <- global_ocean_data_avg %>%
  mutate(anomaly = SSTAVG - reference_avg$reference_SSTAVG)

# Fit a linear model to the anomaly data
model <- lm(anomaly ~ Year, data = global_ocean_data_avg)

# Compute the standard deviation of the residuals
residual_sd <- sd(resid(model))

# Plot anomaly
p <- ggplot(global_ocean_data_avg, aes(x = Year, y = anomaly)) +
  # Error band using the standard deviation of the residuals
  geom_ribbon(aes(ymin = anomaly - residual_sd, ymax = anomaly + residual_sd), 
              fill="grey60", alpha=0.6) +
  geom_smooth(aes(group = 1), method = "loess", color = "red", 
              linetype = "dashed", se = FALSE, size=.8 ) +
  geom_line(aes(color = anomaly), size=1.2) +  # Color by anomaly
  geom_hline(yintercept = 0, color = "darkgreen") +
  scale_color_viridis(option = "B") +
  custom_theme() +
  labs(title = "SST Anomaly Over Time",
       x = "Year",
       y = "Temperature Anomaly (°C)")

# Save the plot using ggsave
ggsave(filename="images/SSTAnomaly.svg", plot=p, width=7, height=5)

# Display plot
print(p)

################################################################################
####################        Anomaly on world map            ####################
################################################################################

# Display head and dimensions of the averaged data
head(filtered_ocean_data)

# Count unique stations
length(unique(filtered_ocean_data$STATION))

# Write the filtered data to CSV
# write_csv(filtered_ocean_data_avg, "oceanFiltered.csv")

# Calculate yearly average SST for each lat-lon combination
filtered_ocean_avg <- filtered_ocean_data %>%
  group_by(Year,  Latitude, Longitude, STATION) %>%
  summarise(SSTAVG = mean(SST, na.rm = TRUE))

# Calculate baseline SST values for each lat-lon combination
baselines <- filtered_ocean_avg %>%
  filter(Year > 1951 & Year <= 1980) %>%
  group_by(STATION) %>%
  summarize(baseline = mean(SSTAVG, na.rm = TRUE))

# Join baseline data to the main dataset
ocean_with_baseline <- left_join(baselines, 
                                 filtered_ocean_avg, by = "STATION")

# Calculate anomalies
ocean_with_baseline <- ocean_with_baseline %>%
  mutate(anomaly = SSTAVG - baseline)

# Filter to the latest year for each station
latest_data <- ocean_with_baseline %>%
  group_by(STATION) %>%
  filter(Year == max(Year))

# Save the data frame as an RDS file
saveRDS(latest_data, file = "data/ocean_latest.rds")

# Plotting on a map
world_map <- st_as_sf(maps::map("world", plot = FALSE, fill = TRUE))

# Plot the latest anomaly values
#ggplot() +
#  geom_sf(data = world_map, fill = "lightgray") +
#  geom_point(data = latest_data, aes(x = Longitude, y = Latitude, color = anomaly), 
#             size = 2) +
#  scale_color_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
#  theme_minimal() +
#  labs(title = "Temperature Anomaly (Observed - Baseline)", color = "Anomaly (°C)")

# Plot the temperature anomalies
p <- ggplot() +
  # Base map
  geom_sf(data = world_map, fill = "lightslategrey", color = "black", size = 0.2) +
  # Anomalies plotted as points, with size reflecting the absolute anomaly value
  geom_point(data = latest_data, 
             aes(x = Longitude, y = Latitude, color = anomaly, size = abs(anomaly))) +
  # Use viridis color palette for anomalies
  scale_color_viridis(option = "B", guide = "colorbar", name = "Anomaly (°C)") +
  # Adjust the size scale
  scale_size_continuous(range = c(1, 4), breaks = seq(0, 5, by = 1)) +
  # Set the theme
  custom_theme() +
  # Add labels
  labs(
    title = "Sea Surface Temperature Anomaly",
    color = "Anomaly (°C)",
    size = "|Anomaly| (°C)"
  ) 


# Save the plot using ggsave
ggsave(filename="images/WorldSSTAnomaly.svg", plot=p, width=7, height=5)

# Display plot
print(p)

################################################################################

