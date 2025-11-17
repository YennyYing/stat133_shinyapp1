# California Crash Explorer (2021–2023)

## Description
An interactive Shiny app that visualizes California traffic crash data (2021–2023) through crash maps, county-level choropleths, and exploratory trend graphics. Users can explore crashes by location, violation type, severity, and more.  

**Link to project:** https://yuanying.shinyapps.io/app3/

![alt tag](http://placecorgi.com/1200/650)

## Author
Yuan Ying  

## Date
05/02/2025  

---

## How It's Made:

**Tech used:** R, Shiny, tidyverse, leaflet, plotly, sf

This project was built as an interactive Shiny web application using R. I used **tidyverse** for data wrangling and visualization, **sf** and **maps** packages to handle California county polygons for choropleth mapping, **leaflet** for interactive web maps, and **plotly** for dynamic exploratory plots.  

The crash data (2021–2023) was cleaned and preprocessed in R to allow filtering by year, location, county, and violation category. Interactive features include clickable crash points, choropleth maps showing county-level crash counts, and faceted trend plots by collision type and severity.  

This project showcases my ability to combine data cleaning, geospatial mapping, and interactive visualization to make complex datasets accessible and insightful.

## Features
- Interactive map showing crash locations in California
- County-level choropleth maps of crash counts
- Trend analysis and exploratory plots by year, county, type of collision, and violation category
- User-friendly filtering options

---

## Packages
```r
The app uses the following R packages (install via `install.packages()` if needed):
library(shiny)
library(tidyverse)
library(maps)
library(leaflet)
library(plotly)
library(sf)

---


