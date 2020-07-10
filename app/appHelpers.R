library(forecast) #moving average, forecasting
library(lubridate) #working with dates

#DATA -----------------------------------------------------------
myLocations <- source("~/Documents/projects/covid19/data/myLocations.R")

#available variables and names 
myVars<- c("total_cases","new_cases","total_deaths","new_deaths",
           "total_cases_per_million","new_cases_per_million",
           "total_deaths_per_million","new_deaths_per_million",
           "total_tests","new_tests","total_tests_per_thousand",
           "new_tests_per_thousand","new_tests_smoothed","new_tests_smoothed_per_thousand")

myNames <- c("Total cases","Daily new cases","Total deaths","Daily new deaths","Total case / million",
        "Daily new cases / million","Total deaths / million","Daily new deaths / million",
        "Total tests","Daily new tests","Total tests / thousand","Daily new test / thousand",
        "Daily new tests (smoothed)","Daily new tests (smoothed) / thousand")

#forecast types and names 
myTypes <- c("arima","tbats","ets","nnetar")
myFuns <-  c("auto.arima","tbats","ets","nnetar")

#month days and names for drawing better axis 
month.days = c(001,032,061,092,122,153,183,214,245,275,306,336)
month.names = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

# FUNCTIONS -----------------------------------------------------------

download.owid <- function(download.dir, file.name) {
  "
  RETURNS: () downloads most recent OWID COVID19
  (download.dir, string) Directory to download OWID COVID19 data
  (file.name, string) Desired file name
  "
  
  owid.url <- "https://covid.ourworldindata.org/data/owid-covid-data.csv"
  download.final <- paste(download.dir, file.name,sep="")
  download.file(owid.url, download.final)
}

date.day <- function(date) {
  "
  RETURNS: (,integer) integer day of year from corresponding to date (i.e December 31, 2020 = 366)
  (date, date) lubridate ymd('YYYY-MM-DD') object
  "
  
  return(as.integer(strftime(date, format="%j")))
}

myCountry <- function(source, loc) {
  "
  RETURNS: (,named list) Subset of input data (source) filtered by location
  (source, data.frame) input owid-covid data source 
  (loc, string), country name/location
  "
  
  return(subset(source, source$location == loc))
}

myContinent <- function(source, loc) {
  #TODO: aggregate values for each data for each country 
  
  "
  RETURNS: (,named list) Subset of input data (source) filtered by continent
  (source, data.frame) input owid-covid data.frame
  (loc, string), continent name
  "
  
}

myTimeseries <- function(source, var, start, stop) {
  "
  RETURNS: (,ts) Time series data of source$var from start:stop 
  (source, named list) input OWID covid data frame ideally input from myCountry or myContinent
  (var, string) variable to create time series of
  (start, date) ymd('YYYY-MM-DD)' lubridate object for starting point of time series
  (stop, date) ymd('YYYY-MM-DD') lubrdiate object for stopping point of time series
  "
  
  myStart = date.day(start)
  myEnd = date.day(stop)
  
  if (myStart > myEnd) {
    message <- paste("Error in stop arg:",start,"occured before",stop,sep=" ")
    stop(message)
  }
  
  return(ts(subset(source[var], source$date <= stop & source$date >= start), 
            start=myStart, 
            end=myEnd))
}

myForecast <- function(source, type, predInt) {
  "
  RETURNS: ( ,named list) time series forecast on a dataset using (type) forecasting method
  (source, ts) times series object to compute forecast values 
  (type, char) forecasting method as a string 
  (predInt, integer) days to forecast from end of time series
  "

  confLevels = c(80,95)
  myFitfunction <- get(type)
  return(forecast(myFitfunction(source), h=predInt, level=confLevels))
}

find.start<- function(source, var) {
  "
  RETURNS: (, date) date of first non zero value for a variable from owid-covid data (ideally from myCountry output)
  (source, named list) output of myCountry or myContinent containing var and date columns. 
  (var, string) variable to find first date of first non zero value. 
  "
  
  for (val in seq(1:length(source[[var]]))) {
    default <-  ymd("2020-01-01") #default start value 
    start.val<- default 
    if (source[[var]][val] != 0 & is.na(source[[var]][val]) == FALSE) {
      start.val <- ymd(source[["date"]][val])
      return(start.val)
      break 
    } # else {
    #   next
    # }
  }
  #check if start.val occurs before Jan1 to avoid first value being 365
  #this SHOULDN'T happen, it's just out of abundance of caution  
  if (year(start.val) != "2020") {
    print("Defaulted to 2020-01-01 because start did not occur in 2020")
    return(default)
  } else {
    return(start.val)
  }
}

recent.date <- function(source) {
  "RETURNS: (, int) returns most recent (assuming the last) date in owid-covid data
  (source, named list) owid-covid raw data"

  return(max(source$date))
}

var.name <- function(var) {
  "
  RETURNS: (,string) full name of variable, for use in generating titles and labels
  (var, string) valid time series variable from myVars
  "
  
  if(is.na(match(var, myVars)) == TRUE){
    stop("Not a valid time series variable")
  }
  
  return(myNames[match(var, myVars)])
}

myColors <- function(source, col1, col2) {
  "
  RETURNS: (,list) color code source by height from col1 (min) to col2 (max)
  (source, named list) univariate data to color code 
  (col1, string) valid color string to start color ramp
  (col2, string) valid color string to end color ramp
  "
  
  myPal <- colorRampPalette(c('green','red'))
  return(myPal(10)[as.numeric(cut(source,breaks = 10))])
}

myAugmentedforecast <- function(dailyforecast, totalsource) {
  "RETURNS: (, ts) new forecast built on daily forecast rather than predicting from total
  i.e forecast total_cases from new_cases 
  (dailyforecast, named list) result from myForecast, 
  (totalsource, named list) result from myTimeseries
  "
  
  total.end <- end(totalsource)[1] #last day in time series
  forecast.length <- length(dailyforecast$mean)
  
  total.augmented <- ts(totalsource[length(totalsource)], 
                        start=total.end, 
                        end=total.end + forecast.length)   
  
  for (i in seq(1:forecast.length)) {
    total.augmented[i+1] <- total.augmented[i] + daily.forecast$mean[i]  
  }
  
  #exclude first value 
  return(total.augmented) 
}


myForecast.plot <- function(source, loc, ts.var, ts.start, ts.end, fore.type, pred.int) {
  "RETURNS: (, figure) Generates forecast plot for one country (replicates timeseriesForecast.R)
  (loc, string) valid location (country)
  (ts.var, string) y-axis variable to plot
  (ts.start, ymd) lubridate ymd object 
  (ts.end, ymd) lubridate ymd object
  (fore.type, string) forecasting model 
  (pred.int, int) days in the future to forecast. 
  "

  loc.data <- myCountry(source, loc)
  ts.data <- myTimeseries(loc.data, ts.var, ts.start, ts.end)
  data0 <- myTimeseries(loc.data, ts.var,ts.start, recent) #all data to plot as points
  ts.forecast <- myForecast(ts.data, fore.type, pred.int)

  out.fname <- paste(loc, var.name(ts.var), pred.int, "Day Forecast", sep=" ")

  #forecast interval plot 
  plot(ts.forecast,
     main=out.fname,
     sub="(data: Our World in Data Coronavirus Source Data)",
     ylab=var.name(ts.var),
     xlim=c(date.day(ts.start), date.day(ts.end) + pred.int),
     shadecols = c("grey80", "grey70"), #color conf int.
     xaxt='n', #hide x axis to label with months
     fcol='black', #prediction line color
     type='p',
     col=NA,
     flty=2,
     showgap=FALSE
     )
  axis(1, labels=month.names,at=month.days) #create axis with months.
  abline(v=date.day(ts.end) + pred.int ,col='grey50',lty=2,lwd=2)
  abline(v=date.day(ts.start),col='red',lty=2,lwd=2)
  abline(v=date.day(ts.end),col='red',lty=2,lwd=2)
  abline(0,0)

  data0.colors <- myColors(data0, "red","green")
  points(data0,col=data0.colors,pch=19,cex=0.9)

  moav <- ma(data0, order=2) 
  lines(moav,col='black',lwd=2) #plot moving average

  confLevel1<-ts.forecast$level[1] #confidence levels for creating legend automatically
  confLevel2<-ts.forecast$level[2]
  confLegend <- c(paste(confLevel1, "% Confidence",sep=""),
                  paste(confLevel2, "% Confidence",sep=""))

  legend(date.day(ts.start) + 1, par("usr")[4] - par("usr")[4]*0.02, #plot in top left corner
        legend=c("Moving Average","Forecast", confLegend[1] ,confLegend[2]),
        col=c("black",'black','grey80','grey70'),
        lty=c(1,2,1,1),lwd=c(2,2,15,15))
}

