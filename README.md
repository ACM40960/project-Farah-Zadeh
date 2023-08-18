
# Climate Change Attribution: Analysis of the Effects of CO2, Solar Irradiance, and Volcanoes on Global Temperature Trends



![image](https://github.com/ACM40960/project-SudeepDamodaranUcd/assets/117033852/2f7f3943-42db-4564-8282-5e69e714c399)


## Table of Contents

- [Introduction](#introduction)
- [Getting Started](#getting-started)
  - [Prerequisites](#prerequisites)
  - [Installation](#installation)
- [Data Sources](#data-sources)
- [File Structure](#file-structure)
- [Analysis Workflow](#analysis-workflow)
- [Key Results](#key-results)
- [Visualization](#visualization)
- [Conclusion](#conclusion)
- [License](#license)
- [References](#references)
- [Authors](#authors)

## Introduction

This project's primary objective is to build a case for or against human-made global warming. By curating and analyzing global surface temperature data from various locations, combined with atmospheric CO2 content, volcanic eruption data, and solar irradiance over time, this project provides a comprehensive perspective on the factors influencing our planet's climate.

## Getting Started

### Prerequisites

Ensure you have R and RStudio installed on your machine. The following libraries are essential for executing the provided scripts:

- `data.table`, `readr`, `dplyr`, `purrr`, `ggplot2`, `sf`, `stringr`, `svglite`, `raster`, `ncdf4`, `chron`, `lattice`, `RColorBrewer`, `rnaturalearth`, `boot`, `forecast`, `urca`, `lmtest`, `gganimate`, `readxl`, `ggpmisc`.

### Installation

1. Clone the repo or download the R scripts and data directory.
2. Open RStudio and set the working directory to the project folder.
``` r
  # To set the working directory:
  setwd("/path/to/your/project")
```
3. Install the required libraries using the `install.packages()` function.
4. The `BigData.R` script manages big data processing. The raw files were not included data folder due to the size concern. To run this R program, land and ocean temperature files have to be downloaded from the url metioned in the data source section and corresponding filepaths have to be updated in `BigData.R`. 

``` r
# Example
# Global Summary of the Year files path on the computer (unzipped csv files)
data_dir <- "gsoy-latest"

# Open the NetCDF file
ncfile <- nc_open("HadISST_sst.nc")
```

5. Refer to `Main.R` for temperature and correlation analyses. This program can be ran directly as it uses the processed data available in `.rds` format in the data folder.

## Data Sources

- **Global Surface Temperature**: Data points are sourced from both terrestrial and maritime regions.
  - Land: Extracted from [NOAA's Global Summary of the Year (GSOY)](https://www.ncei.noaa.gov/cdo-web/datasets). 
  - Ocean: Sourced from the [MET UK Comprehensive Ocean-Atmosphere Data Set (COADS)](https://www.metoffice.gov.uk/hadobs/hadisst/data/download.html).
- **Atmospheric CO2**: Aggregated data from [NOAA's Trends in Atmospheric Carbon Dioxide](https://gml.noaa.gov/ccgg/trends/data.html) and fossil emission data from [ICOS-CP](https://www.icos-cp.eu/science-and-impact/global-carbon-budget/2022).
- **Solar Irradiance**: Data sourced from [NOAA's Total Solar Irradiance (TSI) Climate Data Record (CDR)](https://www.ncei.noaa.gov/products/climate-data-records/total-solar-irradiance).
- **Volcanic Eruptions**: Data sourced from [Opendatasofts's Significant Volcanic Eruption Database](https://public.opendatasoft.com/explore/dataset/significant-volcanic-eruption-database/table/).

## File Structure

- **Main.R**: The core script responsible for most of the analysis. It integrates data, processes it, and Conducts various statistical tests and models, such as ARIMA, Granger Causality Test, and regression analysis, to analyze relationships between variables.
- **BigData.R**: Deals with terrestrial and ccean temperature big data processing.
- **Land.R**: Manages land temperature data. The script filters stations based on activity years and calculates global land temperature anomalies and trends, using the period 1951-1980 as a reference.
- **Ocean.R**: Handles ocean temperature data. The script forms a latitude-longitude grid, samples data points for even distribution, and calculates global ocean SST anomalies and trends, using 1951-1980 as a reference.
- **CO2.R**: Dedicated to collecting and refining atmospheric CO2 data. The script showcases an upward trend in CO2 levels, conducts correlation and regression analysis, emphasizing CO2's role in global warming.
- **Solar.R**: Focuses on solar irradiance data. The script visualizes TSI values, highlighting cyclical solar variations and their minor role in recent climate changes.
- **Volcano.R**: Gathers and processes volcanic activity data post-1950. Visualizes the number and intensity of eruptions, suggesting stability in eruption magnitudes over the years.



## Analysis Workflow

1. **Data Collection**: Extract and refine data from the mentioned sources.
2. **Data Integration**: Combine datasets to obtain a holistic view of global data.
3. **Temperature Trend Analysis**: Computation of global average temperature drift over time, including decadal averages and anomalies. The period 1951-1980 is used as the reference for anomaly calculation.
4. **Predictive Analysis**: An ARIMA model predicts global temperature anomalies up to 2030.
5. **Correlation Analysis**: Correlation computations between CO2 and temperature trends. Rigorous methods, including Pearson correlation, bootstrap confidence intervals, Granger Causality Test, and Johansen Cointegration Test, are employed.
6. **Volcanic Activity Analysis**: Investigates the correlation between volcanic eruptions and CO2 levels.
7. **Solar Irradiance Analysis**: Examines the correlation between solar activity and global temperature/CO2 levels.


## Key Results

- Observed a global average temperature increase, and an upward trend for temperature anomalies.
- Forecasts derived from the Time series ARIMA model also predict a rise in temperatures in coming years.
- Strong positive correlation (Pearson Correlation Coefficient 0.9867) between atmospheric CO2 concentration and fossil emission.
- Strong positive correlation (Pearson Correlation Coefficient 0.858) between global temperature anomalies and CO2. Bootstrap confidence intervals, Johansen Cointegration Test, Granger Causality Test Granger also indicates the strong correlation.
- Regression analysis also indicates significant impact of CO2 on temperature (R-squared: 0.736).
- Volcanic activity shows a weak correlation (0.341) with CO2 levels.
- Solar irradiance analysis indicates no strong correlation with global temperature.

## Visualization

![image](https://github.com/ACM40960/project-SudeepDamodaranUcd/assets/117033852/7f6ea11b-6026-4360-83b7-dda4bac0f9e3)
![image](https://github.com/ACM40960/project-SudeepDamodaranUcd/assets/117033852/3769fc37-b056-4cf4-909b-745444be4b83)
![image](https://github.com/ACM40960/project-SudeepDamodaranUcd/assets/117033852/eede99e7-ee82-4ee5-bbfe-6b2a2f29a756)
![image](https://github.com/ACM40960/project-SudeepDamodaranUcd/assets/117033852/0e15b834-9f0f-45ab-9338-121542a2a7e3)



## Conclusion

The analysis strongly suggests that rising CO2 levels are a significant factor contributing to global warming. While other factors like volcanic activity and solar irradiance play a role in climate variability, they do not show a strong correlation with the observed temperature increases. Robust statistical tests employed further solidify the assertion that CO2 emissions, especially from human activities, remain a dominant driver behind the escalating global temperatures. It's imperative to recognize and address this challenge to mitigate future climatic adversities.

## License

This project is licensed under the MIT License. See the `LICENSE` file for details.

## References

1. [NOAA's Global Summary of the Year (GSOY)](https://www.ncei.noaa.gov/cdo-web/datasets)
2. [MET UK Comprehensive Ocean-Atmosphere Data Set (COADS)](https://www.metoffice.gov.uk/hadobs/hadisst/data/download.html)
3. [NOAA's Trends in Atmospheric Carbon Dioxide](https://gml.noaa.gov/ccgg/trends/data.html)
4. [ICOS-CP Global Carbon Budget 2022](https://www.icos-cp.eu/science-and-impact/global-carbon-budget/2022)
5. [Significant Volcanic Eruption Database](https://public.opendatasoft.com/explore/dataset/significant-volcanic-eruption-database/table/)
6. [Total Solar Irradiance Climate Data Record (CDR)](https://www.ncei.noaa.gov/products/climate-data-records/total-solar-irradiance)

## Authors 

1. Sudeep Damodaran
2. Farah Abed Zadeh 
