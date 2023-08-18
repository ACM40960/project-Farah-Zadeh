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
library(rnaturalearth)
library(boot)
library(forecast)
library(urca)
library(svglite)
library(lmtest)
library(gganimate)
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
    panel.border = element_rect(color = "navyblue", fill = NA, linewidth = 0.8), # border for the plotting panel
    
    # Legend position
    legend.position = "bottom"
  )
}

################################################################################
################     Retrieve Data for Land and Ocean     ######################
################################################################################

# Read the saved RDS file back into a variable
land_subset <- readRDS("data/land_subset.rds")
ocean_subset <- readRDS("data/ocean_subset.rds")

# Rename SSTAVG to TAVG in ocean_subset
ocean_subset <- ocean_subset %>%
  rename(TAVG = SST)

# Rename columns in land_subset to match those in ocean_subset
land_subset <- land_subset %>%
  rename(Year = DATE,
         Latitude = LATITUDE,
         Longitude = LONGITUDE)

# Merge datasets and select specific columns
global_data <- full_join(ocean_subset, land_subset, 
                         by = c("Year", "Latitude", "Longitude","TAVG" )) %>%
  dplyr::select(Year, Latitude, Longitude, TAVG)

# View the merged data
head(global_data)
dim(global_data) # dimension 185798 X 4

# Removing year 2023 as it is not in land dataset 
# Also records below year 1950 removed for consistancy
global_data_filtered <- global_data %>%
  filter(Year < 2023 & Year > 1950)

# View few rows of the data
head(global_data_filtered)

# Write the combined data frame to CSV file
#write_csv(land_subset, "land_subset.csv")
#write_csv(ocean_subset, "ocean_subset.csv")

################################################################################
##########.        Global Land - Ocean Temperature trends           ############
################################################################################

# Calculate yearly average Temperature for each lat-lon combination
global_data_avg <-global_data_filtered %>%
  group_by(Year) %>%
  summarise(
    global_avg_TAVG = mean(TAVG, na.rm = TRUE),
    TAVG_SE = sd(TAVG, na.rm = TRUE) / sqrt(n()),  # Calculate standard error
    TAVG_SD = sd(TAVG, na.rm = TRUE)  # Calculate standard deviation
  )

# Save the data frame as an RDS file
saveRDS(global_data_avg, file = "data/global_data_avg.rds")

# Plot the Temperature average with error bars
p <- ggplot(global_data_avg, aes(x = Year, y = global_avg_TAVG)) +
  geom_ribbon(aes(ymin = global_avg_TAVG - TAVG_SE, ymax = global_avg_TAVG + TAVG_SE), 
              fill = "grey60", alpha = 0.5) +
  geom_line(aes(color = global_avg_TAVG), size = 1.5) +
  geom_smooth(aes(group = 1), method = "loess", color = "red", 
              linetype = "dashed", se = FALSE, size = .8) +
  scale_color_viridis(option = "B", guide = guide_colorbar(title = "Temperature")) +
  custom_theme() +
  labs(
    title = "Global Average Temperature over Time",
    subtitle = "Based on yearly temperature averages",
    x = "Year",
    y = "Global Average Temperature (°C)"
  )


# Save the plot using ggsave
ggsave(filename="images/AvgTempYear.svg", plot=p, width=7, height=5)

# Display plot
print(p)

#finding decadal average
global_dec_avg <- global_data_avg %>%
  mutate(Decade = floor(Year / 10) * 10) %>%  # Extracting the decade
  group_by(Decade) %>%
  summarise(decadal_avg_TAVG = mean(global_avg_TAVG, na.rm = TRUE))

#Decadal Average of Global Land-Ocean Temperature
ggplot(global_dec_avg, aes(x = Decade, y = decadal_avg_TAVG)) +
  geom_line(group=1,color = "darkorange", size=1.2) +  # Ensure the lines connect across all points
  geom_point() +
  custom_theme() +
  labs(title = "Decadal Average of Global Temperature",
       x = "Decade",
       y = "Decadal Average Temperature (°C)")


# Baseline for gloabl land ocean anomaly calculations 
reference_avg <- global_data_avg %>%
  filter(Year >= 1951 & Year <= 1980) %>%
  summarise(reference_TAVG = mean(global_avg_TAVG))

# Calculate anomaly
global_data_avg <- global_data_avg %>%
  mutate(anomaly = global_avg_TAVG - reference_avg$reference_TAVG)


# Fit a linear model to the anomaly data
model <- lm(anomaly ~ Year, data = global_data_avg)

# Compute the standard deviation of the residuals
residual_sd <- sd(resid(model))

# Plot temperature anomaly
p <- ggplot(global_data_avg, aes(x = Year, y = anomaly)) +
  geom_ribbon(aes(ymin = anomaly - residual_sd, ymax = anomaly + residual_sd), 
              fill="grey60", alpha=0.6) +
  geom_line(aes(color = anomaly), size=1.2) +
  geom_smooth(aes(group = 1), method = "loess", color = "red", 
              linetype = "dashed", se = FALSE, size=.8 ) +
  geom_hline(yintercept = 0, color = "darkgreen", alpha=.7) +
  scale_color_viridis(option = "B", guide = guide_colorbar(title = "Temperature Anomaly")) +
  custom_theme() +
  labs(title = "Global Temperature Anomaly",
       subtitle = "Based on yearly temperature averages",
       x = "Year",
       y = "Temperature Anomaly (°C)")

# Save the plot using ggsave
ggsave(filename="images/AnomalyYear3.svg", plot=p, width=7, height=5)

# Display plot
print(p)

# Plot temperature anomaly as bars
p<-ggplot(global_data_avg, aes(x = Year, y = anomaly)) +
  geom_bar(aes(fill = ifelse(anomaly > 0, "Above 0", "Below 0")), 
           stat = "identity") +
  scale_fill_manual(values = c("Above 0" = "#e56d54", "Below 0" = "#005cbb")) +
  geom_hline(yintercept = 0, color = "darkgreen", alpha=.7) +
  guides(fill=FALSE) +
  custom_theme() +
  labs(title = "Global Temperature Anomaly",
       subtitle = "Yearly anomalies based on merged land and ocean data",
       x = "Year",
       y = "Temperature Anomaly (°C)") 

# Save the plot using ggsave
ggsave(filename="images/AnomalyYear2.svg", plot=p, width=7, height=5)

# Display plot
print(p)

# Interactive plot of global temperature anomaly
anim <- ggplot(global_data_avg, aes(x = Year, y = anomaly)) +
  geom_line(aes(group = 1), color="red", alpha = .7) +
  geom_hline(yintercept = 0, color = "darkgreen", alpha=.7) +
  custom_theme() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
    axis.title = element_text(size = 12, face = "bold"),
    aspect.ratio = .5
  )+
  labs(title = "Global Land-Surface Temperature Anomaly",
       x = "Year",
       y = "Temperature Anomaly (°C)") +
  transition_reveal(Year) +
  shadow_mark()

#anim
# Save as gif
anim_save("images/animated_line.gif", anim, fps = 25)

################################################################################
################## Glabal Temperature Anomaly Prediction.     ##################
################################################################################

# Fit an ARIMA model to the anomaly data
fit <- auto.arima(global_data_avg$anomaly)

# Forecast the next 5 years
forecast_result <- forecast(fit, h=8)

# Extract the years and bind with original data
years <- c(global_data_avg$Year, seq(max(global_data_avg$Year) + 1, by = 1, 
                                     length.out = 8))
data_to_plot <- data.frame(Year = years, 
                           Anomaly = c(global_data_avg$anomaly, 
                                       forecast_result$mean))

# Temperature Anomaly for year 2030
anomaly_2030 <- data_to_plot[data_to_plot$Year == 2030, "Anomaly"]
anomaly_2030

# Plot Global Temperature Anomaly vs. Year
p<-ggplot(data_to_plot, aes(x = Year, y = Anomaly)) +
  # Historical data
  geom_line(aes(group = 1), color = "#e56d54", size =1.2) +
  # Moving average
  geom_smooth(aes(group = 1), method = "loess", color = "steelblue4", 
              linetype = "dotted", se = FALSE) +
  # Vertical dotted line at 2022
  geom_vline(aes(xintercept=2022), linetype="dotted", color="steelblue4", size=1) +
  # Forecasted data points
  geom_point(data = data_to_plot[length(global_data_avg$anomaly):nrow(data_to_plot),], 
             aes(x = Year, y = Anomaly ), size = 1.8, color ="darkblue") +
  labs(
    title = "Historical and Forecasted Temperature Anomalies Over Time",
    subtitle = "Based on temperature data with a moving average and projections for future years.",
    x = "Year", 
    y = "Temperature Anomaly (°C)"
  ) +
  custom_theme()

# Save the plot using ggsave
ggsave(filename="images/Anomalytear.svg", plot=p, width=7, height=5)

# Display plot
print(p)

################################################################################
####################        Anomalies on world map          ####################
################################################################################

# Read the saved RDS file back into a variable
land_latest <- readRDS("data/land_latest.rds")
ocean_latest <- readRDS("data/ocean_latest.rds")

# Rename columns in land_latest to match those in ocean_latest
land_latest <- land_latest %>%
  rename(Year = DATE,
         Latitude = LATITUDE,
         Longitude = LONGITUDE)

# Merge datasets and select specific columns
Combined_data <- dplyr::full_join(ocean_latest, land_latest, 
                                by = c("Year", "Latitude", "Longitude","anomaly" )) %>%
  dplyr::select(Year, Latitude, Longitude, anomaly)

# View the merged data
#head(Combined_data)

# Get a detailed world map
world_map <- ne_countries(scale = "medium", returnclass = "sf")

# Plot the temperature anomalies
p<-ggplot() +
  # Base map
  geom_sf(data = world_map, fill = "lightslategrey", color = "black", size = 0.2) +
  # Anomalies plotted as points, with size reflecting the absolute anomaly value
  geom_point(data = Combined_data, 
             aes(x = Longitude, y = Latitude, color = anomaly, size = abs(anomaly))) +
  # Define color gradient for anomalies
  scale_color_viridis(option = "B", guide = "colorbar", name = "Anomaly (°C)") +
  # Adjust the size scale
  scale_size_continuous(range = c(1, 5), breaks = seq(0, 5, by = 1)) +
  # Set the theme
  custom_theme() +
  # Add labels
  labs(
    title = "Global Temperature Anomaly",
    color = "Anomaly (°C)",
    size = "|Anomaly| (°C)"
  )

# Save the plot using ggsave
ggsave(filename="images/WorldAnomaly2.svg", plot=p, width=7, height=5)

# Display plot
print(p)

################################################################################
################        Temperature - CO2 Correlation          #################
################################################################################

# Read the saved RDS file back into a variable
co2_atm <- readRDS("data/co2_atm.rds")

# Merge global temperature data both with atmospheric Co2 content
Gloabl_temp_co2 <- merge(global_data_avg, co2_atm, by.x="Year", by.y="year")

head(Gloabl_temp_co2)

# Rename column mean to Co2_lvl
Gloabl_temp_co2 <- Gloabl_temp_co2 %>%
  rename(Co2_lvl = mean)

# Compute Pearson correlations
pearson_corr <- cor(Gloabl_temp_co2$global_avg_TAVG, 
                    Gloabl_temp_co2$Co2_lvl, method = "pearson")

# Print correlations
pearson_corr

#The Pearson correlation coefficient between atmospheric CO2 content and the
# combined global average temperature (land + ocean) is approximately 0.8580254
# indicating a strong positive correlation. In other words, as CO2 concentrations 
# in the atmosphere increase, global average temperatures (accounting for both 
# land and ocean) also tend to rise. This strong correlation further supports 
# the relationship between increased atmospheric CO2 and global warming.

# Bootstrap confidence intervals
set.seed(123)  # For reproducibility
bootstrap_corr <- function(data, indices) {
  sample_data <- data[indices, ]
  return(cor(sample_data$global_avg_TAVG, sample_data$Co2_lvl, method = "pearson"))
}

results <- boot(data=Gloabl_temp_co2, statistic=bootstrap_corr, R=1000)
boot.ci(results, conf=0.95, type="perc")

# With 95% confidence, the true Pearson correlation coefficient between global 
# temperature and CO2 concentration lies between 0.7763 and 0.9163 Since this 
# range is entirely positive and relatively close to 1, it indicates a strong 
# positive correlation between the two variables. The fact that the entire range 
# is well above zero further suggests that this isn't a spurious result; there 
# is indeed a strong relationship between CO2 concentration and global temperature.

#### Granger Causality Test

# Perform the Granger Causality Test
granger_test <- grangertest(global_avg_TAVG ~ Co2_lvl, order = 1, data = Gloabl_temp_co2)
print(granger_test)

#F: The F-statistic value of 29.148 indicates the difference in fit between the 
# two models.Pr(>F): This is the p-value associated with the F-statistic. A very 
# small p-value (1.207e-06, which is close to 0) suggests that the inclusion of 
# lagged values of Co2_lvl significantly improves the model's fit.

#### Cointegration Test:

# Perform the Johansen Cointegration Test
johansen_test <- ca.jo(Gloabl_temp_co2[, c("global_avg_TAVG", "Co2_lvl")], 
                       type = "eigen", K = 2)
summary(johansen_test)

# The test statistic for  r=0 is highly significant, there's evidence of at least 
# one cointegrating relationship between global average temperature and CO2 levels
# this test provides further evidence that changes in CO2 levels and global average 
# temperatures are related in the long run. Even if they deviate from each other 
# in the short term, they tend to move back towards an equilibrium relationship 
# over time.

################################################################################

##### Regression Analysis.

# Perform linear regression
model <- lm(anomaly ~ Co2_lvl, data=Gloabl_temp_co2)

summary(model)

# The p-value associated with the CO2 coefficient is extremely small (less than 
# 2.2e-16), indicating strong evidence against the null hypothesis. This means 
# the relationship between atmospheric CO2 concentration and global average 
# temperature anomaly is statistically significant.
# R-squared (0.7362): About 73.6% of the variability in the global average 
# temperature anomaly can be explained by the atmospheric CO2 concentration.


# Extract the coefficients (slope and intercept) from the model
slope <- coef(model)[2]
intercept <- coef(model)[1]

# Create equation string suitable for parsing
equation_text <- sprintf("y == %.2f * x + %.2f", slope, intercept)

# Plot the observed data and the regression line
p <- ggplot(Gloabl_temp_co2, aes(x=Co2_lvl, y=anomaly)) +
  geom_point(color="#e56d54", alpha=0.9) +
  geom_smooth(method="lm", se=TRUE, color="#005cbb", fill="skyblue") + 
  # Textbox background
  geom_rect(aes(xmin=380, xmax=415, ymin=-0.14, ymax=-0.02), fill="lightsteelblue1", alpha=0.7) +
  # Add model equation to the graph
  annotate("text", x = 420, y = 0, label = equation_text, 
           vjust = 2, hjust = 1.2, color = "navyblue", size = 5, parse = TRUE) +
  labs(title="Temperature Anomaly vs CO2", 
       x="CO2 Concentration (ppm)", 
       y="Temperature Anomaly (°C)",
       color="Legend") +
  custom_theme()

# Save the plot
ggsave(filename="images/temp_co2.svg", plot=p, width=7, height=5)

# Display plot
print(p)


################################################################################
################ Correlation between CO2 and Volcano eruptions #################
################################################################################

# Read the saved RDS file back into a variable
volcanic_data <- readRDS("data/volcanic_data.rds")

# Aggregate volcanic data by year
volcanic_aggregated <- volcanic_data %>%
  group_by(Year) %>%
  summarise(num_eruptions = n()) %>%
  filter(!is.na(num_eruptions))

# Display the head of the aggregated volcanic data
head(volcanic_aggregated)

# Merge global temperature, CO2, and volcanic eruption data
Gloabl_temp_co2_volcano <- merge(Gloabl_temp_co2, volcanic_aggregated, 
                                 by.x = "Year", by.y = "Year", all.x = TRUE)

###################   Correlation Analysis. ####################################

# Remove rows with NA values
Gloabl_temp_co2_volcano <- na.omit(Gloabl_temp_co2_volcano)

# Perform correlation test
cor_test <- cor.test(Gloabl_temp_co2_volcano$Co2_lvl, 
                     Gloabl_temp_co2_volcano$num_eruptions)

# Display the correlation estimate and p-value
cor_test$estimate
cor_test$p.value

# The correlation coefficient suggests a weak positive linear relationship 
# between the number of volcanic eruptions and atmospheric CO2 levels in a 
# given year. The p-value is approximately 0.0071, indicating statistical 
# significance at the commonly used threshold of 0.05.

################   Granger Causality Test  #####################################

# Perform Granger causality test
gc_test <- grangertest(Co2_lvl ~ num_eruptions, order = 1, 
                       data = Gloabl_temp_co2_volcano)

# Display the Granger causality test results
gc_test

# The p-value of 0.7439 is much greater than the significance threshold of 0.05.
# According to the Granger causality test, the number of volcanic eruptions does
# not "Granger-cause" changes in atmospheric CO2 levels.

##################    Lagged Analysis       ####################################

# Initialize arrays to store lagged correlations and p-values
lag_correlations <- c()
lag_p_values <- c()

# Loop through lag values
for (lag in 0:5) {
  # Create a lagged column
  Gloabl_temp_co2_volcano <- Gloabl_temp_co2_volcano %>% 
    mutate(lagged_eruptions = lag(num_eruptions, lag))
  
  # Perform correlation test
  cor_test <- cor.test(Gloabl_temp_co2_volcano$Co2_lvl, 
                       Gloabl_temp_co2_volcano$lagged_eruptions, 
                       use = "complete.obs")
  
  # Store correlation estimate and p-value
  lag_correlations <- c(lag_correlations, cor_test$estimate)
  lag_p_values <- c(lag_p_values, cor_test$p.value)
}

# Create a data frame with lag results
lag_analysis_results <- data.frame(lag = 0:5, correlation = lag_correlations, 
                                   p_value = lag_p_values)

# Display lag analysis results
lag_analysis_results

# The highest correlation is observed at a lag of 1 year (0.399), suggesting 
# that the impact of volcanic eruptions on atmospheric CO2 levels might be most
# pronounced one year after the eruption. All the p-values are below the 0.05 
# significance level, indicating that these correlations are statistically significant.
# This implies that while volcanic eruptions might influence CO2 levels, they are 
# likely not the dominant factor driving changes in atmospheric CO2 concentrations.

################################################################################
############# Correlation between temperature and Solar Irradiance #############
################################################################################

# Read the saved RDS file back into a variable
solar_data <- readRDS("data/solar_data.rds")

# Merge global temperature, CO2, and solar irradiance data
Gloabl_temp_co2_solar <- merge(Gloabl_temp_co2, solar_data, 
                                 by.x = "Year", by.y = "Year", all.x = TRUE)

head(Gloabl_temp_co2_solar)

###################   Correlation Analysis. ####################################

# Remove rows with NA values
Gloabl_temp_co2_solar <- na.omit(Gloabl_temp_co2_solar)

# Perform correlation test for TSI and mean CO2 levels
cor_test1 <- cor.test(Gloabl_temp_co2_solar$Co2_lvl, Gloabl_temp_co2_solar$TSI)

# Display the correlation estimate and p-value
cor_test1$estimate
cor_test1$p.value

# -0.1227257 correlation coefficient indicates a weak negative correlation between 
# solar activity (as measured by TSI) and CO2 levels. In other words, as TSI values 
# slightly decrease (as we observed earlier), CO2 levels have been increasing.
# However, the relationship is not strong.

# Perform correlation test for mean TVG and TSI
cor_test2 <- cor.test(Gloabl_temp_co2_solar$global_avg_TAVG, Gloabl_temp_co2_solar$TSI)

# Display the correlation estimate and p-value
cor_test2$estimate
cor_test2$p.value

# -0.001612236 correlation coefficient is close to 0, indicating no correlation 
# between solar activity (as measured by TSI) and global average temperature levels. 

################################################################################

