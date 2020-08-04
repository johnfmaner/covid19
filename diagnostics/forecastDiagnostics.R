library(tidyverse) 
setwd("~/Documents/projects/covid19/app/")
source("appHelpers.R")

# TOP 10 COUNTRIES BY CASES
all.countries.cases <- select(as.data.frame(dataIn), 
                              location, date, total_cases)

countries.cases <- 
  arrange(
    subset(
      all.countries.cases,
      all.countries.cases$date == recent.date(all.countries.cases) & 
        (all.countries.cases$location != "World")),
    total_cases)

n.countries <- 10
top.countries <- countries.cases$location[
  (length(countries.cases$location) - n.countries + 1):
  length(countries.cases$location)]

length.top <-length(top.countries)
top.countries

# find forecast errors for each country with AUTO.ARIMA fore.type
results <- as.data.frame(list(location=rep(0,length.top), 
                         mtf.rmse = rep(0,length.top),
                         mtf.method = rep(0,length.top), 
                         maf.rmse = rep(0,length.top), 
                         maf.method = rep(0,length.top),
                         rmse.change = rep(0,length.top)))

par(mar=c(2,3,2,2),mfrow=c(2,5))
for (i in seq(1,length.top,by=1)) {
  #create data
  ts.loc <- top.countries[i]
  mc <- myCountry(dataIn, ts.loc)                         # country data for ts.loc 
  
  #define other parameters
  pred.int <- 10
  d.var <- "new_cases"
  t.var <- new2total(d.var)                               # convert d.var to total
  ts.end <- "2020-07-01" 
  fore.type <- myFuns[3]                                  # auto.arima, ets, tbats
  
  #timeseries
  mdt <- myTimeseries(mc, d.var, ts.end)                  # timeseries of daily_var
  mtt <- myTimeseries(mc, t.var, ts.end)                  # timeseries of total_var
  mt0 <- myTimeseries(mc, t.var, max(mc$date))            # timeseries of total_var for all available dates 
  
  #forecasts
  mdf <- myForecast(mdt, fore.type, pred.int)             # standard forecast of daily_var
  mtf <- myForecast(mtt, fore.type, pred.int)             # standard forecast of total_var 
  maf <- myAugmentedforecast(mdf, mtt)                    # augmented forecast of total_var

  #temporarily store results 
  mtf.error <- forecast.errors(mt0, mtf$mean)             # standard forecast of total_var rmse
  maf.error <- forecast.errors(mt0, maf$mean)             # augmented forecast of total_var rmse
  maf.rmse <- sqrt(mean(maf.error^2))                     
  mtf.rmse <- sqrt(mean(mtf.error^2))
  mtf.method <- mtf$method                                # standard forecast of total_var method
  maf.method <- maf$method                                # augmented forecast of total_var method

  #plot forecast errors 
  conf.x<-c(seq(date.day(ts.end),date.day(ts.end) + pred.int),
            seq(date.day(ts.end) + pred.int,date.day(ts.end)))
  conf.y<-c(maf$upper - maf$mean, rev(-maf$mean + maf$lower))
  
  plot(mtf.error,type='l',ylim=c(min(mtf.error,maf.error,conf.y),max(mtf.error,maf.error,conf.y)),
       main=ts.loc,xlab='Days Since Jan 1 2020',ylab='Forecast Error, Total Cases',col='red')

  polygon(conf.x,conf.y,col=rgb(0.2,0.2,0.7,alpha=0.3),border=NA)  #draw confidence intervals 
  
  text(date.day(ts.end) + 4.5, 0.94*max(maf.error,mtf.error,conf.y),
       labels=paste0("Δ% RMSE: ",round(100*(maf.rmse - mtf.rmse)/mtf.rmse, 2)))
  lines(maf.error,lty=2,col='blue')
  abline(h=0)

  #populate results list 
  results$location[i] <- ts.loc 
  results$mtf.rmse[i] <- mtf.rmse
  results$maf.rmse[i] <- maf.rmse
  results$mtf.method[i] <- mtf.method
  results$maf.method[i] <- maf.method
  results$rmse.change[i] <- round(100*(maf.rmse - mtf.rmse)/mtf.rmse, 2)
}

par(mar=c(2,3,2,2),mfrow=c(2,5))
for (i in seq(1,length.top,by=1)) {
  #create data
  ts.loc <- top.countries[i]
  mc <- myCountry(dataIn, ts.loc)                         # country data for ts.loc 
  
  #define other parameters
  pred.int <- 10
  d.var <- "new_cases"
  t.var <- new2total(d.var)                               # convert d.var to total
  ts.end <- "2020-07-01" 
  fore.type <- myFuns[3]                                  # auto.arima, ets, tbats
  
  #timeseries
  mdt <- myTimeseries(mc, d.var, ts.end)                  # timeseries of daily_var
  mtt <- myTimeseries(mc, t.var, ts.end)                  # timeseries of total_var
  mt0 <- myTimeseries(mc, t.var, max(mc$date))            # timeseries of total_var for all available dates 
  
  #forecasts
  mdf <- myForecast(mdt, fore.type, pred.int)             # standard forecast of daily_var
  mtf <- myForecast(mtt, fore.type, pred.int)             # standard forecast of total_var 
  maf <- myAugmentedforecast(mdf, mtt)                    # augmented forecast of total_var
  
  #temporarily store results 
  mtf.error <- forecast.errors(mt0, mtf$mean)             # standard forecast of total_var rmse
  maf.error <- forecast.errors(mt0, maf$mean)             # augmented forecast of total_var rmse
  maf.rmse <- sqrt(mean(maf.error^2))                     
  mtf.rmse <- sqrt(mean(mtf.error^2))
  mtf.method <- mtf$method                                # standard forecast of total_var method
  maf.method <- maf$method                                # augmented forecast of total_var method
  
  #plot forecast errors 
  mtf.kde <- density(mtf.error)
  maf.kde <- density(maf.error)

  mtf.kde
  plot(maf.kde,
       type='l',
       col='blue',
       main=ts.loc,
       xlim=c(min(mtf.kde$x,maf.kde$x), max(mtf.kde$x,maf.kde$x)),
       ylim=c(0,max(maf.kde$y, mtf.kde$y)))
  lines(mtf.kde,type='l',col='red')
  rug(maf.error,col='blue',lwd=2)
  rug(mtf.error,col='red',lwd=2)
  
  #populate results list 
  results$location[i] <- ts.loc 
  results$mtf.rmse[i] <- mtf.rmse
  results$maf.rmse[i] <- maf.rmse
  results$mtf.method[i] <- mtf.method
  results$maf.method[i] <- maf.method
  results$rmse.change[i] <- round(100*(maf.rmse - mtf.rmse)/mtf.rmse, 2)
}

results

?auto.arima

mean(round(100*(results$maf.rmse - results$mtf.rmse)/results$mtf.rmse, 2))
  