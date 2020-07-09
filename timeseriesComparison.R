# WORKING DIR -----------------------------------------------------------
myDir <- "~/Documents/projects/covid19/" #specify repository directory
setwd(myDir)

# DEPENDENCIES -----------------------------------------------------------
library(forecast) #moving average, forecasting
library(lubridate) #working with dates
source("timeseriesHelpers.R") #load helper functions

# LOAD DATA -----------------------------------------------------------

## **Downloading Data ===========================================================
#download most recent owid-covid data - can also do this with download-owid.sh

myData <- paste(myDir, "data/",sep="")
myFile <- "owid-covid.csv"
#download.owid(myData, myFile) 


## **Read CSV ============================================
dataIn <- read.csv(paste(myData, myFile, sep=""), header=TRUE)

# DATA -----------------------------------------------------------

## **Country Data ===========================================================
#load data and create subset of data for a country
loc <- "Mexico"         #Eventually add ability to choose from unique(dataIn$location). 
loc.data <- myCountry(dataIn, loc)

## **Time Series ===========================================================
#create subsets of time series data
#variable with which to create time series 
daily.var <- "new_cases"
total.var <- "total_cases"

#useful dates 
jan1 <- ymd("2020-01-01") 
start0 <- find.start(loc.data, daily.var)    #find date for first non zero value of daily.var    
recent <- max(loc.data$date)      #Most recent day from owid-covid data

ts.start <- start0
ts.end <- recent

#TODO: add ts.start, ts.end as selectable fields (ideally from calendar or drop down in Shiny)
#create time series of new_cases from ts.start to ts.end days. 

daily.ts<- myTimeseries(loc.data, "new_cases", ts.start, ts.end)
total.ts<- myTimeseries(loc.data, "total_cases", ts.start, ts.end)

#create time series from jan1 to plot points 
data0 <- myTimeseries(loc.data, total.var ,jan1, recent)

#corresponding day of year for start of each month in 2020. 
month.days = c(001,032,061,092,122,153,183,214,245,275,306,336)
month.names = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

# FORECASTING -----------------------------------------------------------

pred.int = 30 #used for drawing plot windows and vertical lines
fore.type = "arima" #WARNING: TBATS not recommended for non periodic data (i.e ts.var <- total_)

daily.forecast <- myForecast(daily.ts, fore.type, pred.int)
total.forecast <- myForecast(total.ts, fore.type, pred.int)

total.augm <- myAugmentedforecast(daily.forecast, total.ts) #augmented forecast to compare total_ against new_

# PLOT -----------------------------------------------------------
# ## **Image Device ===========================================================
# out.dir <- paste(myDir,"figures/",sep="")
out.fname <- paste(loc, var.name(total.var), pred.int, "Day", toupper(fore.type), "Forecast", sep=" ")
# out.ftype <- "png"
# out.final<- paste(out.dir, out.fname,".", out.ftype, sep="")
# png(filename=out.final,
#     width=800,height=800,
#     bg='white',pointsize = 20)

## **Forecast Plot ===========================================================
plot(total.forecast,
     main=out.fname,
     sub="(data: Our World in Data Coronavirus Source Data)",
     ylab=var.name(total.var),
     xlim=c(date.day(ts.start), date.day(ts.end) + pred.int),
     shadecols = c("grey80", "grey70"), #color conf int.
     xaxt='n', #hide x axis to label with months
     fcol='black', #prediction line color
     type='p',
     flty=2,
     col=NA, #hide points
     showgap=FALSE
)

axis(1, labels=month.names,at=month.days) #create axis with months.
lines(total.augm, col="red")

## Vertical Lines  ===========================================================
#vertical line end of prediction interval
abline(v=date.day(ts.end) + pred.int ,col='grey50',lty=2,lwd=2)

#vertical lines to mark forecast range
abline(v=date.day(ts.start),col='red',lty=2,lwd=2)
abline(v=date.day(ts.end),col='red',lty=2,lwd=2)
abline(0,0)

## **Points ===========================================================
#plot all new cases/days points color coded by height for reference
data0.colors <- myColors(data0, "red","green")
points(data0,col=data0.colors,pch=19,cex=0.8)

## **Moving Average ===========================================================
#calculate and plot moving average smoothed over moav_length.
moav_length= 2 #days. choose 7 or 14 to smooth over weekly fluctuations
moav <- ma(data0, order=moav_length)
lines(moav,col='black',lwd=2) #plot moving average

## **Legend ===========================================================
confLevel1<-total.forecast$level[1] #confidence levels for creating legend automatically
confLevel2<-total.forecast$level[2]
confLegend <- c(paste(confLevel1, "% Confidence",sep=""),
                paste(confLevel2, "% Confidence",sep=""))

legend(date.day(ts.start) + 1, par("usr")[4] - par("usr")[4]*0.02, #plot in top left corner
       legend=c("Moving Average","Forecast","Augmented Forecast", confLegend[1],confLegend[2]),
       col=c("black",'black','red','grey80','grey70'),
       lty=c(1,2,1,1,1),lwd=c(2,2,2,15,15),
       bg='transparent')

# ## **Close Device  ===========================================================
#dev.off ()