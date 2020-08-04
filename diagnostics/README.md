# Forecast Diagnostics 

This project features what I call an "augmented forecast". The "augmented forecast is built as follows:

1. Choose the cumulative variable you would like to forecast (e.g `total_cases`). 
2. Build a forecast of the corresponding daily variable, (eg. `new_cases`) with `myForecast`. 
3. Initialize a time series in which all values are the last value of the cumulative variable at the time in which the forecast begins. 
4. Add the daily forecast mean to the first value of the time series created above. 
5. Iterate over the range of the forecast horizon. 

In psuedo code, step 5 looks something like:

```
augmented.mean <- ts(last value of cumulative variable, 
                     start=end of timeseries forecast is built on)

for (i in seq(1:forecast horizon){
  augmented.mean[i+1] <- augmented.mean[i] + dailyforecast.mean[i]
 }
```
A complete example using helper functions looks like:

```
source("appHelpers.R")
mc <- myCountry(dataIn, "Brazil")
mdt <- myTimeseries(mc, "new_cases", "2020-07-01")
mtt <- myTimeseries(mc, "total_cases", "2020-07-01")

mdf <- myForecast(mdt, "auto.arima", 30)
mtf <- myForecast(mtt, "auto.arima", 30)

maf <- myAugmentedforecast(mdf, mtt)

plot(maf$mean, type='l')
lines(maf$lower, lty=2)
lines(maf$upper, lty=2)
```

The augmented forecast was made to provide a more accurate forecast of cumulative variables. To test this, a forecast of a cumulative variable and an "augmented forecast" of the same variable is generated at some point in the past. The forecast errors are then calculated to find the RMSE of the two methods. In the top 10 countries by total number of cases, the "augmented forecast" outperforms the simple forecast on the cumulative variable. 

The performance of the augmented forecast against the cumulative forecast is done with the following parameters: 

Locations: 
* Iran 
* Colombia 
* Chile 
* Peru 
* Mexico 
* South Africa 
* Russia 
* India 
* Brazil 
* United States

Forecast horizon: 10 days
Forecast start: "2020-07-01"
Forecast variable: Total cases

**Legend**:

* red: cumulative forecast

* blue: augmented forecast

* shaded blue: augmented forecast 95% confidence interval

### ARIMA: 
In some cases, the daily forecast ARIMA parameters are similar to the cumulative forecast parameters, and the augmented forecast shows little improvement. 

The mean % change in RMSE was found to be -25.0%.
![arima_errors](https://github.com/johnfmaner/covid19/blob/master/diagnostics/arima_error.png)
![arima errors distribution](https://github.com/johnfmaner/covid19/blob/master/diagnostics/arima_error_kde.png)

### ETS
The mean % change in RMSE was found to be -16.9%.  
![ets_errors](https://github.com/johnfmaner/covid19/blob/master/diagnostics/ets_error.png)
![ets errors distribution](https://github.com/johnfmaner/covid19/blob/master/diagnostics/ets_error_kde.png)

### TBATS
The mean % change in RMSE was found to be -26.4%. 
![tbats_errors](https://github.com/johnfmaner/covid19/blob/master/diagnostics/tbats_error.png)
![tbats_errors distribution](https://github.com/johnfmaner/covid19/blob/master/diagnostics/tbats_error_kde.png)
