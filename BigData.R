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

################################################################################
########################     Land Data Collection      #########################
################################################################################

# https://www.ncei.noaa.gov/cdo-web/datasets
# National Oceanic and Atmospheric Administration: Global Summary of the Year
# Global Summary of the Year (GSOY) data provides a global summary of meteorological 
# elements (max temp, snow, etc) from 1763 to present with updates weekly on a 
# yearly resolution. The data are computed from input data using the Global 
# Historical Climatology Network (GHCN)-Daily dataset.
# Documentation: https://www.ncei.noaa.gov/pub/data/metadata/documents/GSOYReadme.txt

# Interested fields
# "STATION" = GHCN ID
# "DATE" = YYYY-MM where YYYY is 4-digit year and MM is 2-digit month
# LONGITUDE
# LATITUDE
# "TAVG" = Average Annual Temperature. Computed by adding the unrounded monthly 
# average temperatures and dividing by 12.
# "TMAX" = Average Annual Maximum Temperature. 
# "TMIN" = Average Annual Minimum Temperature.

# Function to read and select fields
# Files which do not have the required fields will not be processed
read_and_select_fields <- function(file_path) {
  data <- read_csv(file_path, col_types = cols())
  
  # List of necessary fields that should be present in the data
  necessary_fields <- c("STATION", "DATE", "LONGITUDE", "LATITUDE", "TAVG", "TMIN", "TMAX")
  
  # Check if all necessary fields are present in the data
  if (all(necessary_fields %in% names(data))) {
    # Select only the necessary fields from the data
    data <- data %>% 
      select(all_of(necessary_fields))
    
    return(data)  # Return the filtered data with necessary fields
  } else {
    return(NULL)  # Return NULL if necessary fields are not present
  }
}

# Global Summary of the Year dataset folder on the computer
data_dir <- "gsoy-latest"
# Get all the files in the path as a list
csv_files <- list.files(path = data_dir, pattern = "\\.csv$", full.names = TRUE)

# Define the batch size for processing a large set of files
batch_size <- 1000

# Calculate the number of batches needed to process all CSV files
num_batches <- ceiling(length(csv_files) / batch_size)

# Create an empty list to store processed data for each batch
gsoy_data_list <- vector("list", num_batches)

# Loop through each batch and process CSV files
for(i in seq_len(num_batches)) {
  start_index <- (i-1) * batch_size + 1
  end_index <- min(i * batch_size, length(csv_files))
  
  batch_files <- csv_files[start_index:end_index]
  
  # Process each file in the current batch and bind the results together
  gsoy_data_list[[i]] <- map_dfr(batch_files, read_and_select_fields)
  
  cat(sprintf("Processed batch %d of %d\n", i, num_batches))
}

# Combine all batches into one data.table
# Print the dimensions of the combined data frame
dim(gsoy_data2) # dimension 1243693 X 7
# Print the first few rows of the combined data frame
head(gsoy_data2)

# Save the combined data frame as an RDS file
saveRDS(gsoy_data2, file = "gsoy_data2.rds")

# Write the combined data frame to a CSV file
write_csv(gsoy_data2, "gsoy_data2.csv")

################################################################################
########################     Ocean Data Collection      ########################
################################################################################

# https://www.metoffice.gov.uk/hadobs/hadisst/index.html
# https://www.metoffice.gov.uk/hadobs/hadisst/data/download.html
# The SST data are taken from the Met Office Marine Data Bank (MDB), which from 
# 1982 onwards also includes data received through the Global Telecommunications 
# System (GTS). In order to enhance data coverage, monthly median SSTs for 
# 1871-1995 from the Comprehensive Ocean-Atmosphere Data Set (COADS) (now ICOADS) 
# were also used where there were no MDB data. The sea ice data are taken from a 
# variety of sources including digitized sea ice charts and passive microwave 
# retrievals.

# Open the NetCDF file
ncfile <- nc_open("HadISST_sst.nc")
print(ncfile)

# Extract values of variables
var_name <- "sst"
longitude <- ncvar_get(ncfile, "longitude")
n_longitude <- dim(longitude)
head(longitude)

latitude <- ncvar_get(ncfile, "latitude")
n_latitude <- dim(latitude)
head(latitude)

time_values <- ncvar_get(ncfile, "time")
head(time_values)
time_units <- ncatt_get(ncfile, "time", "units")
n_time <- dim(time_values)
n_time

sst_values <- ncvar_get(ncfile, var_name)
long_name <- ncatt_get(ncfile, var_name, "long_name")
units <- ncatt_get(ncfile, var_name, "units")
fill_value <- ncatt_get(ncfile, var_name, "_FillValue")
dim(sst_values)

# Replace netCDF fill values with NA
sst_values[sst_values == fill_value$value] <- NA

# Extract a single slice (January)
month_index <- 1
sst_slice <- sst_values[, , month_index]
dim(sst_slice)

# Plot the slice using levelplot
grid <- expand.grid(lon = longitude, lat = latitude)
cut_points <- c(-50, -40, -30, -20, -10, 0, 10, 20, 30, 40, 50)
levelplot(sst_slice ~ lon * lat, data = grid, at = cut_points, cuts = 11, 
          pretty = TRUE, 
          col.regions = rev(brewer.pal(10, "RdBu")))

# Create a 2D matrix of longitude, latitude, and time
lon_lat_time <- as.matrix(expand.grid(longitude, latitude, time_values))

# Reshape the SST array
sst_vector_long <- as.vector(sst_values)
length(sst_vector_long)  # It should be 1447344

# Create a data frame
sst_data <- data.frame(cbind(lon_lat_time, sst_vector_long))

# Save the data frame as an RDS file
saveRDS(sst_data, file = "ocean_data2.rds")

# Close the NetCDF file
nc_close(ncfile)

################################################################################

#### Raster plot of 1900 and 2023 SST

# Load the NetCDF file as a raster brick
sst_brick <- brick("HadISST_sst.nc")  # Replace with your file path
sst_brick

# Extract and plot a specific layer (364) 
layer_index_364 <- 364  #  for April 1900
raster_364 <- sst_brick[[layer_index_364]]
raster_364[raster_364 < -200] <- NA  # Set extreme values to NA
color_scheme <- rev(brewer.pal(10, "RdBu"))  # Reverse color scheme
plot(raster_364, col = color_scheme, main = "April 1900 Ocean SST")

# Extract and plot another specific layer (1840) 
layer_index_1840 <- 1840
raster_1840 <- sst_brick[[layer_index_1840]]
raster_1840[raster_1840 < -200] <- NA
plot(raster_1840, col = color_scheme, main = "April 2023 Ocean SST")

################################################################################
