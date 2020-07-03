library(forecast) #moving average, forecasting
library(lubridate) #working with dates

# FUNCTIONS -----------------------------------------------------------

download.owid <- function(download.dir, file.name) {
  "
  RETURNS: () downloads most recent OWID COVID19
  (download.dir, string) Directory to download OWID COVID19 data
  (file.name, string) Desired file name
  "
  
  owid.url <- "https://covid.ourworldindata.org/data/owid-covid-data.csv"
  download.final <- paste(download.dir, file.name)
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
  
  myLocations <- unique(source$location)
  loc <- tolower(loc) #convert location to lowercase for ease of use
  
  if (is.na(match(loc, tolower(myLocations))) == FALSE){
    return(subset(source, tolower(location) == loc))
  }
  else {
    stop("Location is not a valid location. ")
  }
}

myContinent <- function(source, loc) {
  #TODO: aggregate values for each data for each country 
  
  "
  RETURNS: (,named list) Subset of input data (source) filtered by continent
  (source, data.frame) input owid-covid data.frame
  (loc, string), continent name
  "
  
  myLocations <- unique(source$continent)
  loc <- tolower(loc) #convert location to lowercase for ease of use
  
  if (is.na(match(loc, tolower(myLocations))) == FALSE){
    return(subset(source, tolower(continent) == loc))
  }
  else {
    stop("Continent is not a valid location. ")
  }
}

myTimeseries <- function(source, var, start, stop) {
  "
  RETURNS: (,ts) Time series data of source$var from start:stop 
  (source, named list) input OWID covid data frame ideally input from myCountry or myContinent
  (var, string) variable to create time series of
  (start, date) ymd('YYYY-MM-DD)' lubridate object for starting point of time series
  (stop, date) ymd('YYYY-MM-DD') lubrdiate object for stopping point of time series
  "
  
  #available variables for time series forecasting
  myVars<- c("total_cases","new_cases","total_deaths","new_deaths",
             "total_cases_per_million","new_cases_per_million",
             "total_deaths_per_million","new_deaths_per_million",
             "total_tests","new_tests","total_tests_per_thousand",
             "new_tests_per_thousand","new_tests_smoothed","new_tests_smoothed_per_thousand")

  if(is.na(match(var, myVars)) == TRUE){
    stop("Not a valid time series variable")
  }
  
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

myForecast <- function(source, type, predInt, confLevels) {
  "
  RETURNS: ( ,named list) time series forecast on a dataset using (type) forecasting method
  (source, ts) times series object to compute forecast values 
  (type, char) forecasting method as a string 
  (predInt, integer) days to forecast from end of time series
  (confLevels, numeric vector) vector specifying confidence intervals as percentage
  "
  
  #available forecasting models -- will add more in future! 
  myTypes <- c("arima","tbats","ets","nnetar")
  myFuns <-  c("auto.arima","tbats","ets","nnetar")
  
  type <- tolower(type) #convert type to lowercase for ease of use
  
  #if conf intervals not specified, default to 95% and 80%
  if(missing(confLevels)) {
    confLevels = c(80,95)
    print("Using default 95% and 80% confidence intervals")
  } else {
    confLevels = confLevels
  }
  
  #perform forecasting type if available in myTypes
  if (is.na(match(type, myTypes)) == FALSE ){
    funIndex <- match(type, myTypes)
    myFitfunction <- get(myFuns[funIndex])
    #output:
    return(forecast(myFitfunction(source), h=predInt, level=confLevels))
  }
  else{
    stop("Forecast type is not available. ")
  }
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
  #this SHOULDN'T happen, it's just here as a contigency 
  if (year(start.val) != "2020") {
    print("Defaulted to 2020-01-01 because start did not occur in 2020")
    return(default)
  } else {
    return(start.val)
  }
}

var.name <- function(var) {
  "
  RETURNS: (,string) full name of variable, for use in generating titles and labels
  (var, string) valid time series variable from myVars
  "
  
  myVars<- c("total_cases","new_cases","total_deaths","new_deaths",
             "total_cases_per_million","new_cases_per_million",
             "total_deaths_per_million","new_deaths_per_million",
             "total_tests","new_tests","total_tests_per_thousand",
             "new_tests_per_thousand","new_tests_smoothed","new_tests_smoothed_per_thousand")
  
  myNames<- c("Total cases","Daily new cases","Total deaths","Daily new deaths","Total case / million",
              "Daily new cases / million","Total deaths / million","Daily new deaths / million",
              "Total tests","Daily new tests","Total tests / thousand","Daily new test / thousand",
              "Daily new tests (smoothed)","Daily new tests (smoothed) / thousand")
  
  if(is.na(match(var, myVars)) == TRUE){
    stop("Not a valid time series variable")
  }
  
  return(myNames[match(var, myVars)])
}
