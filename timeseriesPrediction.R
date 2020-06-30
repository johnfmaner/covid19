library(forecast) #for moving average, forecasting


# LOAD DATA #########################################

## **Downloading Data ####
#set working directory to download data 
working_dir <- "~/Documents/projects/covid/owid_data"  
setwd(working_dir) 

#download most recent owid-covid data - can also do this with bash script
# owid_url <- "https://covid.ourworldindata.org/data/owid-covid-data.csv" 
# download.file(owid_url, "owid-covid.csv") 



## **Convert ymd to day ####
#convert ymd object to day of year for time series analysis
date.day <- function(date) {
  as.numeric(strftime(date, format="%j"))
}

#corresponding day of year for start of each month in 2020. 
month.days = c(001,032,061,092,122,153,183,214,245,275,306,336)
month.names = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")



# DATA #########################################


## **US Data ####
#load data and create subset data.frame of data for US
covid <- read.csv("~/Documents/projects/covid/owid_data/owid-covid.csv", header=TRUE)
covid.us <- subset(covid,location=="United States")



## **Time Series subsets ####
#create subsets of time series data
start0 <- ymd("2020-01-01")     #Jan 1
last <- max(covid.us$date)      #Most recent day from owid-covid data

start1 <- ymd("2020-04-29")     
end1 <- ymd("2020-06-16")        

#specify start and end times from above as ymd objects 
ts.start <- start0
ts.end <- last

#create time series from ts_start to ts_end days. 
data <- ts(subset(covid.us$new_cases, covid.us$date <= ts.end & covid.us$date >= ts.start), 
           start=date.day(ts.start), 
           end=date.day(ts.end))

data0 <- ts(covid.us$new_cases, start=date.day(start0), end=date.day(last))



#FORECASTING ###################################
#parameters you need to change- data source, prediction interval. 
pred_int = month.days[8] - date.day(ts.end) #from today until August. 



## **TBATS ####
tbatsFit <- tbats(data, use.parallel=TRUE, num.cores = 4) # fit tbats model
tbatsFcast <- forecast(tbatsFit, 
                       h=pred_int,
                       level=c(80,95))

# ## **ARIMA ####
# arimaFit <- auto.arima(data)
# arimaFcast <- forecast(arimaFit, 
#                        h=pred_int,
#                        level=c(80,95))
# 
# 
# ## **ETS ####
# etsFit <- ets(data)
# etsFcast <- forecast(etsFit, 
#                      h=pred_int,
#                      level=c(80,95))


#PLOT #############################################
#Main plot with forecast intervals 
plot(tbatsFcast,
     main="",
     xlab="Time",
     ylab="Thousands of daily new cases",
     shadecols = c("grey80", "grey70"),
     xaxt='n',                  #hide x axis to label with months 
     yaxt='n',                  #hide y axis to label with thousands
     fcol='black',              #prediction line color
     flty=2,
     type="p",
     col=NA, #hide points 
     xlim=c(date.day(ts.start), date.day(ts.end) + pred_int),
     ylim=c(0,60e3),
     showgap=FALSE)

axis(1, labels=month.names,at=month.days) #create axis with months. 
axis(2, labels=c(0,10,20,30,40,50,60,70,80,90,100),
     at=c(0,10e3,20e3,30e3,40e3,50e3,60e3,70e3,80e3,90e3,100e3))

#vertical line end of prediction interval  
abline(v=date.day(ts.end) + pred_int ,col='grey50',lty=2)

#vertical lines to mark forecast range 
abline(v=date.day(ts.start),col='red',lty=2)
abline(v=date.day(ts.end),col='red',lty=2)
abline(0,0)

## **Points ####
#plot new cases/days points color coded by height. 
grPal <- colorRampPalette(c('green','red'))
mycolors <- grPal(10)[as.numeric(cut(data0,breaks = 10))]
points(data0,col=mycolors,pch=19)

## **Moving Average ####
#calculate and plot moving average smoothed over moav_length.
moav_length= 2 #days. choose 7 or 14 to smooth over weekly fluctuations
moav <- ma(data0, order=moav_length)
lines(moav,col='black',lwd=2) #plot moving average

#create legend
legend(date.day(ts.start) + 1 ,60e3,
       legend=c("Moving Average","Forecast",'95% Confidence',"80% Confidence"),
       col=c("black",'black','grey80','grey70'),
       lty=c(1,2,1,1),lwd=c(2,2,8,8))
