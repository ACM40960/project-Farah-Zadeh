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
library(lubridate)
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
##################    Solar Irradiance Data Collection      ####################
################################################################################

# https://www.ncei.noaa.gov/products/climate-data-records/total-solar-irradiance

# The Total Solar Irradiance (TSI) Climate Data Record (CDR) measures the spectrally 
# integrated energy input to the top of the Earth's atmosphere at a base mean 
# distance from the Sun (i.e., one Astronomical Unit), and its units are Wm-2. 
# This CDR is constructed using models that identify and quantify irradiance 
# changes relative to quiet Sun conditions at daily, monthly, and yearly intervals. 
# It applies a linear regression to the proxy Mg II index and sunspot areas, and 
# compares the results with solar irradiance measurements from the SOlar 
# Radiation and Climate Experiment (SORCE) satellite. The daily and monthly data 
# records span from 1882 to present, and the yearly data record spans from 1610 
# to present.

# Open the NetCDF file
netcdf_file <- "data/tsi_v02r01_yearly_s1610_e2022_c20230120.nc"
ncfile <- nc_open(netcdf_file)

# Print File informations
print(ncfile)

# Extract TSI and time values in arrays
TSI_values <- ncvar_get(ncfile, "TSI")
TSI_unc_values <- ncvar_get(ncfile, "TSI_UNC")
time_values <- ncvar_get(ncfile, "time")

# Convert time values to dates (assuming they are in days since 1610-01-01)
start_date <- as.Date("1610-01-01")
time_dates <- start_date + time_values

# Create a dataframe fro solar Irradiance
tsi_df <- data.frame(Time = time_dates, 
                     TSI = TSI_values, 
                     TSI_unc = TSI_unc_values)

# View the first few rows of the dataframe
head(tsi_df)

# Close the NetCDF file
nc_close(ncfile)

# Extract the year from Time
tsi_df$Year <- year(tsi_df$Time)
tsi_df$Time <- NULL  # Remove the Time column

# Filter the data from 1950 on wards
solar_data <- tsi_df[tsi_df$Year >= 1950, ]

# Save the combined data frame as an RDS file
saveRDS(solar_data, file = "data/solar_data.rds")

# Write the combined data frame to a CSV file
#write_csv(solar_data, "solar_data.csv")

################################################################################

# Plot TSI vs Year
p<-ggplot(solar_data, aes(x = Year, y = TSI)) +
  geom_line(aes(color = "TSI values"), size=.8) +
  geom_ribbon(aes(ymin = TSI - TSI_unc, ymax = TSI + TSI_unc), alpha = 0.2) +
  geom_smooth(method = "lm", se = FALSE, aes(color = "Trend line"), size=1) +
  scale_color_manual(values=c("TSI values"="#005cbb", "Trend line"="#e56d54")) + 
  labs(title = "Total Solar Irradiance (TSI) Over the Years", 
       y = "TSI (W/m^2)",
       color = "Legend") +
  custom_theme()

# Save the plot using ggsave
ggsave(filename="images/Solar.svg", plot=p, width=7, height=5)

# Display plot
print(p)

# There are cyclical variations in the TSI values over the years, which is expected
# due to the solar cycle. The red trend line suggests a very slight decrease in 
# the TSI values over the observed period.

################################################################################