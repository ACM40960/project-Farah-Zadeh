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
gsoy_data2 <- readRDS("data/gsoy_data2.rds")
head(gsoy_data2)

# Remove records with NA values in the TAVG column
gsoy_data_upd <- gsoy_data2 %>%
  filter(!is.na(TAVG))

# Print the dimensions of the updated data frame
dim(gsoy_data_upd) # Dimension 858172 X  7

# Print the minimum and maximum DATE values in the updated data frame
min(gsoy_data_upd$DATE) # 1763
max(gsoy_data_upd$DATE) # 2022

# Calculate the count of stations for each year
station_counts <- gsoy_data_upd %>%
  group_by(DATE) %>%
  summarise(station_count = n_distinct(STATION))

# Find year and maximum station count
max_station_year <- station_counts$DATE[which.max(station_counts$station_count)]
max_station_count <- max(station_counts$station_count)

# Plot the number of stations vs year count
p <- ggplot(station_counts, aes(x = DATE, y = station_count)) +
  geom_line(color = "darkblue", size = 1) +
  geom_point(aes(y = station_count), color = "darkorange", size = 1.5) +
  geom_vline(aes(xintercept = max_station_year), color="darkblue", linetype="dashed", size = 0.75) +
  annotate("text", x = 1900, y = 11000 , label = paste("Max Number of Stations:", max_station_count, "\n Year:",max_station_year ), color = "darkorange2", hjust = "left") + 
  labs(
    title = "Number of Stations vs Year",
    subtitle = "Stations with over 40 years of coverage",
    x = "Year",
    y = "Number of Stations"
  ) +
  custom_theme()

# Save the plot using ggsave
ggsave(filename="images/StationCount.svg", plot=p, width=7, height=5)

# Display plot
print(p)

# Summarize the range of years for each station
temporal_coverage <- gsoy_data_upd %>% 
  group_by(STATION) %>% 
  summarise(start_year = min(DATE, na.rm = TRUE),
            end_year = max(DATE, na.rm = TRUE),
            total_years = n_distinct(DATE))

# Print the dimensions of the summarized data frame
dim(temporal_coverage) # Dimension 32325 X 4
head(temporal_coverage)

# Visualize Data Coverage using ggplot2
p <- ggplot(temporal_coverage, aes(x = start_year, y = end_year)) +
  geom_point(aes(color = total_years), alpha = 0.7, size = 2.5) +
  scale_color_viridis(name = "Total Years Recorded", direction = -1, option = "B") +
  labs(
    title = "Temporal Coverage of Weather Stations",
    subtitle = "Colored by total number of recorded years",
    x = "Start Year",
    y = "End Year"
  ) +
  custom_theme()

# Save the plot using ggsave
ggsave(filename="images/TemporalCoverage.svg", plot=p, width=7, height=5)

# Display plot
print(p)

# We have more data starting from 1880 on wards, before that data is scaterred
# indicating only few stations having data for the time period

# Taking the average of total years
avg_years <- mean(temporal_coverage$total_years, na.rm = TRUE)

# Distribution of Total Years of Coverage Across Stations
p <- ggplot(temporal_coverage, aes(x=total_years)) + 
  geom_histogram(aes(y = after_stat(density)), binwidth=1, fill="darkorange", color="tan1", alpha=0.8) + 
  geom_density(color="navyblue", size=1) + 
  geom_vline(aes(xintercept = avg_years), size = 0.7, color="navyblue", linetype="dashed") +  
  annotate("text", x = avg_years + 5, y= 0.046, label = paste("Mean Total Years:", round(avg_years, 1)), color="navyblue", hjust="left") +
  labs(title="Distribution of Total Years of Data Coverage Across Stations", 
       x="Total Years of Coverage", 
       y="Density") +
  custom_theme()

# Save the plot using ggsave
ggsave(filename="images/TemporalCoverageDist.svg", plot=p, width=7, height=5)

# Display plot
print(p)

#Filter out Stations with Insufficient Data
#For instance, to keep stations with at least 40 years of data:
sufficient_data_stations <- temporal_coverage %>% 
  filter(total_years >= 40) %>%
  pull(STATION)

# Number of stations having more than 40 years of data
length(sufficient_data_stations) #7499 

#filter based on sufficient data points
filtered_data <- gsoy_data_upd %>% 
  filter(STATION %in% sufficient_data_stations)

# Taking data which is above 1940
filtered_data <- filtered_data %>%
  filter(DATE > 1940)

# Number of stations
length(unique(filtered_data$STATION)) #7479

################################################################################

# Weather station metadata from NASA merged with dataset to get the station name.
# https://data.giss.nasa.gov/gistemp/station_data_v4_globe/v4.temperature.inv.txt

# Read the metadata text file - fixed-width file
station_metadata <- read_fwf("data/gov_gistemp_station_data_v4_globe_v4.txt", 
                             fwf_widths(c(12, 9, 9, 8, 31, 6), 
          c("STATION", "Lat", "Lon", "Elev_m", "Station_Name", "BI")), skip = 1)

# Select only STATION and Station_Name columns
station_names <- dplyr::select(station_metadata, STATION, Station_Name)

# Merge the station name with the filtered data based on the STATION column
filtered_data <- left_join(filtered_data, station_names, by = "STATION")

# Check the first few rows of the combined data
head(filtered_data)

#write_csv(filtered_data, "filtered_gsoy.csv")

# Filter the dataset based on station name starting with EI for Ireland
filtered_gsoy_data_EI <- filtered_data %>%
  filter(str_detect(STATION, "^EIE")) %>%
  filter(STATION == "EIE00107808")

# Plotting temperature vs time for BALLYSHANNON STATION
p<-ggplot(filtered_gsoy_data_EI, aes(x = DATE, y = TAVG)) +
  geom_line(color="darkorange1", size=.8) +
  labs(title = "Average Temperature for BallyShannon Station",
       x = "Year",
       y = "Average Temperature",
       color = "Station") +
  custom_theme()

# Save the plot using ggsave
ggsave(filename="images/BallyShannon.svg", plot=p, width=7, height=5)

# Display plot
print(p)

################################################################################
###################     Temporal Continuity.     ###############################
################################################################################

# For the temporal continuity, only the stations which have data from 1951 to 
# 2019 are selected.

# Define the range of years
 year_range <- 1951:2020

# Filter stations with data for all years in the range
consistent_stations <- filtered_data %>%
  group_by(STATION) %>%
  filter(all(year_range %in% DATE)) %>%
  ungroup()

# Filter stations with data for all years in the range
#consistent_stations <- filtered_data %>%
#  group_by(STATION) %>%
  #filter(any(DATE == 1950) & any(DATE == 2022)) %>%
#  filter(all(year_range %in% DATE)) %>%
#  ungroup()

length(unique(consistent_stations$STATION))

#Set seed
set.seed(1234)
# Get the list of 200 evenly distributed station IDs
evenly_distributed_stations <- sample(unique(consistent_stations$STATION), 200)

# Filter consistent_stations for the selected stations
selected_stations_data <- consistent_stations %>%
  filter(STATION %in% evenly_distributed_stations)

# View the dimensions of the selected data
dim(selected_stations_data) # dimension 13512 X 8

# Save the data frame as an RDS file
saveRDS(selected_stations_data, file = "data/land_subset.rds")

################################################################################
####################    Global land temperature trends      ####################
################################################################################

# Calculate global average temperature for each year
global_avg_temp <- selected_stations_data %>%
  group_by(DATE) %>%
  summarise(global_avg_TAVG = mean(TAVG, na.rm = TRUE),
            sd_TAVG =  sd(TAVG, na.rm = TRUE) / sqrt(n())) 

# Plot the global average temperature over time with errorbar (ribbon)
p <- ggplot(global_avg_temp, aes(x = DATE, y = global_avg_TAVG)) +
  geom_ribbon(aes(ymin = global_avg_TAVG - sd_TAVG, ymax = global_avg_TAVG + sd_TAVG), 
              fill = "grey60", alpha = 0.5) +
  geom_line(aes(color = global_avg_TAVG), size = 1.5) +
  scale_color_viridis(option = "B", guide = guide_colorbar(title = "Temperature")) +
  geom_smooth(aes(group = 1), method = "loess", color = "red", 
              linetype = "dashed", se = FALSE, size = .8) +
  custom_theme() +
  labs(
    title = "Global Land-Surface Average Temperature Over Time",
    subtitle = "Based on yearly temperature averages",
    x = "Year",
    y = "Global Average Temperature"
  )

# Save the plot using ggsave
ggsave(filename="images/LandTavg.svg", plot=p, width=7, height=5)

# Display plot
print(p)

# Baseline for gloabl land anomaly calculations 
reference_avg <- global_avg_temp %>%
  filter(DATE >= 1951 & DATE <= 1980) %>%
  summarise(reference_TAVG = mean(global_avg_TAVG))

# Calculate anomaly
global_avg_temp <- global_avg_temp %>%
  mutate(anomaly = global_avg_TAVG - reference_avg$reference_TAVG)

# Fit a linear model to the anomaly data
model <- lm(anomaly ~ DATE, data = global_avg_temp)

# Compute the standard deviation of the residuals
residual_sd <- sd(resid(model))

# Plot anomaly
p <- ggplot(global_avg_temp, aes(x = DATE, y = anomaly)) +
  # Error band using the standard deviation of the residuals
  geom_ribbon(aes(ymin = anomaly - residual_sd, ymax = anomaly + residual_sd), 
              fill="grey60", alpha=0.6) +
  geom_smooth(aes(group = 1), method = "loess", color = "red", 
              linetype = "dashed", se = FALSE, size=.8 ) +
  geom_line(aes(color = anomaly), size=1.2) +  # Color by anomaly
  geom_hline(yintercept = 0, color = "darkgreen") +
  scale_color_viridis(option = "B") +
  custom_theme() +
  labs(title = "Global Land-Surface Temperature Anomaly",
       subtitle = "Based on yearly temperature averages",
       x = "Year",
       y = "Temperature Anomaly (°C)")


# Save the plot using ggsave
ggsave(filename="images/LandAnomaly.svg", plot=p, width=7, height=5)

# Display plot
print(p)

#finding decadal average
decadal_avg_temp <- global_avg_temp %>%
  mutate(Decade = floor(DATE / 10) * 10) %>%  # Extracting the decade
  group_by(Decade) %>%
  summarise(decadal_avg_TAVG = mean(global_avg_TAVG, na.rm = TRUE))

#Decadal Average of Global Land-Surface Temperature
ggplot(decadal_avg_temp, aes(x = Decade, y = decadal_avg_TAVG)) +
  geom_line(group=1,color = "darkorange", size=1.2) +  # Ensure the lines connect across all points
  geom_point() +
  custom_theme() +
  labs(title = "Decadal Average of Land-Surface Temperature",
       x = "Decade",
       y = "Decadal Average Temperature (°C)")



################################################################################
####################       Anomaly on world map             ####################
################################################################################

# Convert the latitude/longitude to a spatial object
stations <- st_as_sf(filtered_data, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)

# Create a 5-degree latitude/longitude grid and give each cell a unique ID
grid <- st_make_grid(stations, cellsize = c(10, 10))
grid <- st_sf(id = 1:length(grid), geometry = grid, crs = st_crs(stations))

# Intersect the stations with the grid
stations_grid <- st_join(stations, grid, left = FALSE)

# Sample one station from each grid cell (without taking the entire data)
sampled_station_ids <- stations_grid %>%
  group_by(id) %>%
  sample_n(1) %>%
  pull(STATION)

#length(sampled_station_ids)

# Filter the original dataset for only the sampled stations
filtered_gsoy_data <- filtered_data %>%
  filter(STATION %in% sampled_station_ids)

# Establishing a Baseline for each selected station (1951-1980 as an example)
baselines<- filtered_gsoy_data %>%
  filter(DATE >= 1951 & DATE <= 1980) %>%
  group_by(STATION) %>%
  summarize(baseline = mean(TAVG, na.rm = TRUE))

# Joining the baseline to the main data
gsoy_with_baseline <- left_join(filtered_gsoy_data, baselines, by = "STATION")

# Filter out rows where TAVG is NA
gsoy_filtered <- gsoy_with_baseline %>%
  filter(!is.na(baseline))

# Calculate anomalies
gsoy_filtered <- gsoy_filtered %>%
  mutate(anomaly = TAVG - baseline)

# Remove the anomaly record
gsoy_filtered  <- gsoy_filtered %>%
filter(STATION!='SF000068858')

# Filter to the latest year for each station
land_latest_data <- gsoy_filtered %>%
  group_by(STATION) %>%
  filter(DATE == max(DATE)) %>%
  filter(DATE > 2015)

# Save the data frame as an RDS file
saveRDS(land_latest_data, file = "data/land_latest.rds")

# Plotting on a map
world_map <- st_as_sf(maps::map("world", plot = FALSE, fill = TRUE))

# Plot the latest anomaly values
p <- ggplot() +
  # Base map
  geom_sf(data = world_map, fill = "lightslategrey", color = "black", size = 0.2) +
  geom_point(data = land_latest_data, 
             aes(x = LONGITUDE, y = LATITUDE, color = anomaly, size = abs(anomaly))) +
  scale_color_viridis(option = "B", guide = "colorbar", name = "Anomaly (°C)") +
  scale_size_continuous(range = c(1, 4), breaks = seq(0, 5, by = 1)) +
  custom_theme() +
  labs(
    title = "Sea Surface Temperature Anomaly",
    color = "Anomaly (°C)",
    size = "|Anomaly| (°C)"
  ) +
  # Remove the size legend
  guides(size = "none")



# Save the plot using ggsave
ggsave(filename="images/WorldLandAnomaly.svg", plot=p, width=7, height=5)

# Display plot
print(p)
################################################################################


