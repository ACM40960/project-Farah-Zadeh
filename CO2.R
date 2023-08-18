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
library(readxl)
library(ggpmisc)
library(svglite)
library(gridExtra)
library(viridis)

################################################################################
########################     Setting Custom Theme     #########################
################################################################################

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
    panel.grid.major = element_line(color = "darkorange", size = 0.1),
    panel.grid.minor = element_line(color = "darkorange", size = 0.05),
    
    # Border for the panel
    panel.border = element_rect(color = "navyblue", fill = NA, size = 0.8) # border for the plotting panel
  )
}


################################################################################
####################         CO2 Data Collection.           ####################
################################################################################

#https://gml.noaa.gov/ccgg/trends/data.html
#https://gml.noaa.gov/webdata/ccgg/trends/co2/co2_annmean_mlo.csv

# Read the CSV file, skipping the first 59 rows
co2_atm <- read.csv("data/co2_annmean_mlo.csv", skip = 59)

# View the top few rows of the data
head(co2_atm)

# https://www.icos-cp.eu/science-and-impact/global-carbon-budget/2022

# Read the Excel file
co2_emission <- read_excel("data/Global_Carbon_Budget_2022v1.0.xlsx", 
                   sheet = "Global Carbon Budget", 
                   skip = 20)

# View the top few rows of the data
head(co2_emission)

# Save the combined data frame as an RDS file
saveRDS(co2_atm, file = "data/co2_atm.rds")

# Write the processed data to CSV files
#write_csv(co2_atm, "CO2_atm.csv")
#write_csv(co2_emission, "CO2_emission.csv")

################################################################################
####################         CO2 - fossil trends       ################
################################################################################


# Plot atomospheric CO2 content trends
p1 <- ggplot(co2_atm, aes(x = year, y = mean)) +
  geom_line(color = "#005cbb", size = 1) +
  geom_point(color = "#e56d54", size = 2) +
  geom_ribbon(aes(ymin = mean - unc, ymax = mean + unc), fill = "#005cbb", 
              alpha = 0.9) +
  labs(
    title = "Atmospheric CO2 Concentration Over Time",
    x = "Year",
    y = "CO2 Concentration (ppm)"
  ) +
  custom_theme() 

# Plot Year vs Fossil Emissions
p2 <- ggplot(co2_emission, aes(x = Year, y = `fossil emissions excluding carbonation`)) +
  geom_line(color = "#e56d54", size=1) +
  labs(
    title = "Fossil Emissions Over Time",
    x = "Year",
    y = "Fossil Emissions (GtCO2)"
  ) +
  custom_theme()

# Combine plots using grid.arrange from the gridExtra package
p <- grid.arrange(p1, p2, ncol=1)


# Save the plot using ggsave
ggsave(filename="images/Co2_fossil.svg", plot=p, width=7, height=5)


################################################################################
################      CO2 - Fossil emission Correlation          ################
################################################################################

# Correlation between atmospheric co2 concentation and fossil emissions
# Merge both data togather
merged_data <- merge(co2_atm, co2_emission, by.x="year", by.y="Year")

# Compute Pearson and Spearman correlations
pearson_corr <- cor(merged_data$mean, 
                    merged_data$`fossil emissions excluding carbonation`, 
                    method = "pearson")

# Print correlations
pearson_corr


# The correlation coefficient between atmospheric CO2 concentration and fossil
# emissions is approximately 0.987 This is very close to  1, indicating a strong 
# positive linear relationship between the two variables.


################################################################################
################                 Regression Analysis.           ################ 
################################################################################

# Perform linear regression
model <- lm(mean ~ `fossil emissions excluding carbonation`, data=merged_data)

# Display the summary of the regression model
summary(model)

# Extract coefficients
intercept <- coef(model)[1]
slope <- coef(model)[2]

# Create equation string
equation_text <- sprintf("y == %.2f * x + %.2f", slope, intercept)

p <- ggplot(merged_data, aes(x=`fossil emissions excluding carbonation`, y=mean)) +
  geom_point(color="#e56d54", alpha=0.9) +  
  geom_smooth(method="lm", se=TRUE, color="#005cbb", fill="skyblue") + 
  labs(title="Fossil Emissions vs CO2 Level", 
       x="Fossil Emissions (GtCO2)", 
       y="CO2 Concentration (ppm)") +
  # Add model equation to the graph
  annotate("text", x = 10, y = Inf, label = equation_text, 
           vjust = 2, hjust = 1.2, color = "navyblue", size = 5, parse = TRUE) +
  custom_theme()

# Save the plot
ggsave(filename="images/Co2vsFossil.svg", plot=p, width=7, height=5)


# Display plot
print(p)

# Extract the coefficients (slope and intercept) from the model
slope <- coef(model)[2]
intercept <- coef(model)[1]

# Create equation string suitable for parsing
equation_text <- sprintf("y == %.2f * x + %.2f", slope, intercept)


p <- ggplot(merged_data, aes(x=`fossil emissions excluding carbonation`, y=mean)) +
  geom_point(color="#e56d54", alpha=0.9) +  
  geom_smooth(method="lm", se=TRUE, color="#005cbb", fill="skyblue") + 
  # Textbox background
  geom_rect(aes(xmin=7.4, xmax=10, ymin=305, ymax=315), fill="lightsteelblue1", alpha=0.7) +
  # Add model equation to the graph
  annotate("text", x = 8.7, y = 310, label = equation_text, color = "navyblue", size = 5, parse = TRUE) +
  labs(title="Fossil Emissions vs CO2 Level", 
       x="Fossil Emissions (GtCO2)", 
       y="CO2 Concentration (ppm)") +
  custom_theme()

# Save the plot
ggsave(filename="images/Co2vsFossil.svg", plot=p, width=7, height=5)

# Display plot
print(p)




# Coefficient of Determination R^2 is 0.974, which indicates that approximately
# 97.4% of the variance in CO2 concentration is explained by the fossil emissions. 
# This is a high 

# Significance of the Variable: The p-value for the fossil emissions variable
# is extremely close to zero (< 0.05), indicating that fossil emissions are a 
# statistically significant predictor of CO2 concentration.

# The regression analysis confirms a strong linear relationship between fossil 
# emissions and atmospheric CO2 concentration, with fossil emissions being a 
# significant predictor of CO2 concentration based on this dataset.

################################################################################