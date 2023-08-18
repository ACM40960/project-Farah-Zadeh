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
library(readxl)
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
################     Volcano Eruption Data Collection     ######################
################################################################################

# https://public.opendatasoft.com/explore/dataset/significant-volcanic-eruption-database/table/
# The Significant Volcanic Eruption Database is a global listing of over 500
# significant eruptions which includes information on the latitude, longitude, 
# elevation, type of volcano, and last known eruption. A significant eruption is 
# classified as one that meets at least one of the following criteria: caused 
# fatalities, caused moderate damage (approximately $1 million or more), with a 
# Volcanic Explosivity Index (VEI) of 6 or larger, caused a tsunami, or was 
# associated with a major earthquake

# Read the volcanic eruption data
volcanic_data <- read_xlsx("data/significant-volcanic-eruption-database.xlsx")

# Filter data for years from 1951 onwards
volcanic_data_filtered <- subset(volcanic_data, Year >= 1951)

# Save the combined data frame as an RDS file
saveRDS(volcanic_data_filtered, file = "data/volcanic_data.rds")

# Bar plot for number of eruptions by year
p<-ggplot(volcanic_data_filtered, aes(x=Year)) +
  # Bar plot with color gradient based on the count of eruptions
  geom_bar(aes(fill = ..count..)) +
  # Using a color gradient from blue (few eruptions) to red (many eruptions)
  scale_fill_gradient("Number of Eruptions", low = "#005cbb", high = "#e56d54")  +
  guides(fill="none") +
  custom_theme() +
  labs(title="Number of Volcanic Eruptions by Year",
       x="Year", y="Number of Eruptions") 

# Save the plot using ggsave
ggsave(filename="images/VolcanoCount.svg", plot=p, width=7, height=5)

# Display plot
print(p)

# Stable Magnitude: Despite variability from year to year, the average VEI doesn't
# show a consistent increasing or decreasing trend since 1940. The magnitude of 
# eruptions, on average, has remained relatively stable.


# Extract every 5th year from the dataset
breaks_to_show <- seq(from=1951, to=2021, by=3)

# Boxplot for VEI distribution by year
p<-ggplot(volcanic_data_filtered, aes(x=as.factor(Year), y=`Volcanic Explosivity Index`)) +
  geom_boxplot(fill="darkorange3", show.legend = FALSE,  alpha=.8) +  
  custom_theme() +
  labs(title="Distribution of Volcanic Explosivity Index (VEI)",
       x="Year", y="VEI") +
  theme(plot.title = element_text(hjust = 0.5, size=20),   
        axis.title.x = element_text( size=18),  
        axis.title.y = element_text( size=18),
        axis.text.y = element_text(size=14),  
        legend.position = "bottom",
        legend.text = element_text(size=13),
        axis.text.x=element_text(angle=45, hjust=1, size=12))+
  scale_x_discrete(breaks = breaks_to_show) 

# Save the plot using ggsave
ggsave(filename="images/VolcanoVEI.svg", plot=p, width=7, height=5)

# Display plot
print(p)

# Some years experience higher average VEIs, suggesting years with more powerful 
# eruptions, while others have lower averages. However, there isn't a clear pattern 
# suggesting a significant increase in explosiveness over the years.

################################################################################

