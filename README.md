# ERA5 Climate Data Download Tutorial with KrigR

A practical guide for downloading and processing ERA5 climate data using the KrigR package in R based on my tested workflow (up to July 2025). Package versions I used:
- KrigR version 0.9.6.2.
- tidyverse version 2.0.0
- lubridate version 1.9.4
- sf version 1.0-21
- terra version 1.8-50

## Overview

This tutorial provides step-by-step instructions for:
- Setting up ERA5 data downloads from Copernicus Climate Data Store
- Downloading temperature, precipitation, and dewpoint data
- Calculating relative humidity from temperature variables
- Performing population-weighted spatial aggregation
- Creating monthly climate summaries

## Getting Started

### Prerequisites
- R and RStudio (or your favourite IDE) 
- Account on [Copernicus Climate Data Store](https://cds.climate.copernicus.eu/)
- Required R packages: `tidyverse`, `KrigR`, `lubridate`, `sf`, `terra`

### Tutorial

Follow the complete tutorial by opening `era5_download_tutorial_KrigR.html` in your browser and copy pasting the code to your R script.

## Key Features

- **Handles common KrigR issues** - Includes workarounds for metadata writing errors
- **Population weighting** - Aggregation using population density/count data
- **Multiple climate variables** - Temperature, precipitation, and relative humidity
- **Spatial processing** - From raster downloads to administrative boundary summaries
- **Performance tips** - Best practices for large dataset downloads based on my experience

## Example Use Case

The tutorial uses Jakarta, Indonesia as an example, downloading 2 months of daily climate data and aggregating to monthly summaries at province and district levels.

## Data Sources

- **Climate Data**: ERA5-Land reanalysis from Copernicus Climate Data Store
- **Population Data**: LandScan Global Population Database
- **Administrative Boundaries**: GeoBoundaries dataset

## Contributing

Issues and improvements welcome! This tutorial complements the official [KrigR documentation](https://www.erikkusch.com/courses/krigr/) with practical examples and updated workflows. Many other sources were also mentioned within the tutorial file as I went through all of them to come up with this workflow that worked for my research needs.
