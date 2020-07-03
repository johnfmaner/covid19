# WORKING DIR -----------------------------------------------------------
myDir <- "~/Documents/projects/covid/" #specify your directory
setwd(myDir)

# DEPENDENCIES -----------------------------------------------------------
library(forecast) #moving average, forecasting
library(lubridate) #working with dates
source("timeseriesHelpers.R") #load helper functions

# LOAD DATA -----------------------------------------------------------

## **Downloading Data ===========================================================
#download most recent owid-covid data - can also do this with bash script

myData <- paste(myDir, "data/",sep="")
myFile <- "owid-covid.csv"
#download.owid(myData, myFile)

## **Read CSV ============================================
dataIn <- read.csv(paste(myData, myFile, sep=""), header=TRUE)

# DATA -----------------------------------------------------------

## **Country Data ===========================================================
#load data and create subset data.frame of data for a country
loc <- "Brazil"         #Eventually add ability to choose from unique(covid$location). 
covid <- myCountry(dataIn, loc)

## **Time Series ===========================================================
#create subsets of time series data
jan1 <- ymd("2020-01-01")     #Jan 1
recent <- max(covid$date)      #Most recent day from owid-covid data

start1 <- ymd("2020-05-05")     
end1 <- ymd("2020-06-16")        

ts.start <- jan1
ts.end <- recent 
#TODO: add ts.start, ts.end as selectable fields (ideally from calendar or drop down in Shiny)
#create time series of new_cases from ts.start to ts.end days. 
data <- myTimeseries(covid, "new_cases", ts.start, ts.end)

#create time series from jan1 to plot points 
data0 <- myTimeseries(covid, "new_cases", jan1, recent)

#corresponding day of year for start of each month in 2020. 
month.days = c(001,032,061,092,122,153,183,214,245,275,306,336)
month.names = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

# FORECASTING -----------------------------------------------------------

pred.int = 30 #used for drawing plot windows and vertical lines 
fore.type = "arima"
Fcast <- myForecast(source=data, type=fore.type, predInt=pred.int, confLevels=c(80,95))

# PLOT -----------------------------------------------------------
## **Image Device ===========================================================
out.dir <- paste(myDir,"figures/",sep="")
out.fname <- paste(loc, fore.type, pred.int, ts.start, ts.end, sep="-")
out.ftype <- "png"
out.final<- paste(out.dir, out.fname,".", out.ftype, sep="")
png(filename=out.final,
    width=800,height=800,
    bg='white',pointsize = 20)

## **Forecast Plot ===========================================================
plotTitle <- paste(loc, pred.int, "Day", toupper(fore.type), "Forecast", sep=" ")

plot(Fcast,
     main=plotTitle,
     sub="(data: Our World in Data Coronavirus Source Data)",
#     xlab="Time",
     ylab="Daily new cases",
     xlim=c(date.day(ts.start), date.day(ts.end) + pred.int),
     shadecols = c("grey80", "grey70"), #color conf int. 
     xaxt='n', #hide x axis to label with months 
#     yaxt='n', #hide y axis to label with thousands
     fcol='black', #prediction line color
     flty=2,
     type="p",
     col=NA, #hide points 
     showgap=FALSE)

axis(1, labels=month.names,at=month.days) #create axis with months. 
#axis(2, labels=c(0,10,20,30,40,50,60,70,80,90,100),
#     at=c(0,10e3,20e3,30e3,40e3,50e3,60e3,70e3,80e3,90e3,100e3))

## Vertical Lines  ===========================================================
#vertical line end of prediction interval  
abline(v=date.day(ts.end) + pred.int ,col='grey50',lty=2,lwd=2)

#vertical lines to mark forecast range 
abline(v=date.day(ts.start),col='red',lty=2,lwd=2)
abline(v=date.day(ts.end),col='red',lty=2,lwd=2)
abline(0,0)

## **Points ===========================================================
#plot all new cases/days points color coded by height for reference 
grPal <- colorRampPalette(c('green','red'))
myColors <- grPal(10)[as.numeric(cut(data0,breaks = 10))]
points(data0,col=myColors,pch=19)

## **Moving Average ===========================================================
#calculate and plot moving average smoothed over moav_length.
moav_length= 2 #days. choose 7 or 14 to smooth over weekly fluctuations
moav <- ma(data0, order=moav_length)
lines(moav,col='black',lwd=2) #plot moving average

## **Legend ===========================================================
confLevel1<-Fcast$level[1]
confLevel2<-Fcast$level[2]
confLegend <- c(paste(confLevel1, "% Confidence",sep=""), 
                paste(confLevel2, "% Confidence",sep=""))

legend(date.day(ts.start) + 1, par("usr")[4] - par("usr")[4]*0.02, #plot in top left corner
       legend=c("Moving Average","Forecast", confLegend[1],confLegend[2]),
       col=c("black",'black','grey80','grey70'),
       lty=c(1,2,1,1),lwd=c(2,2,15,15))


## **Close Device  ===========================================================
dev.off ()

