# Time series analysis of COVID19 data using R. 

![GitHub Logo](/figures/prediction_total.png)

This project is a self learning experiment with time series forecasting models in R. The long term goal of this project is to build a Rshiny dashboard to forecast the new daily cases of COVID 19. 

DISCLAIMER: I am by no means an epidemiologist. This work in no way claims to account for easing of stay-at-home mandates, social distancing, mask usage, and other factors. 

## Contents 

### /owid_covid
This project uses the Our World In Data COVID19 data found on Github: (https://github.com/owid/covid-19-data) or the official website (https://ourworldindata.org/coronavirus-source-data). 

This folder includes a bash script to download the most recent data release. This data includes 212 unique locations, so it may be easily adapted to other countries or continents. 

### /figures
Two sample figures of TBATS forecasting of COVID19 daily new cases data in the US. The spike in new cases per day in late June diverges from the predictable periodic behavior seen beginning clearly in May through the middle of June. 

### timeseriesPrediction.R
Loads owid_covid.csv data and performs forecasting for the US. One can specify the time series start and end points (i.e to study the periodic behavior observed in daily new cases during May and early June), and the forecast interval. 

## To do
- [x] Showcase divergence from periodic behavior
- [x] Create bash script to download data directly from OWID website. 
- [ ] Easily swap between country and forecasting models
- [ ] Use predictions to calculate total cases 
- [ ] Build shiny dashboard

