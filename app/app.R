#setwd("~/Documents/projects/covid19/app/") #only for development use
source("appHelpers.R")
options(scipen=999) #(try to) disable scientific notation for prettier plots
recent <- recent.date(dataIn)
# 
# Define UI
ui <- fluidPage(theme = shinytheme("paper"),
                navbarPage("COVID-19 Forecasting", 
                           tabPanel("Forecasting",
                                    sidebarPanel(
                                      tags$head(tags$script('$(document).on("shiny:connected", function(e) {
                            Shiny.onInputChange("innerWidth", window.innerWidth);
                            });
                            $(window).resize(function(e) {
                            Shiny.onInputChange("innerWidth", window.innerWidth);
                            });
                            ')),
                                      tags$h3("Forecasting"),
                                      p("Build a forecast according to your parameters."),
                                      selectInput("loc", "Location", myLocations, selected="Brazil",multiple=FALSE),
                                      dateInput("ts.end", "Forecast Start (YYYY-MM-DD)",
                                                value="2020-07-01",
                                                min = "2020-01-15",
                                                max=recent),
                                      selectInput("ts.var", "Variable", myVars,selected="total_cases",multiple=FALSE),
                                      selectInput("fore.type", "Forecast Model", myFuns,multiple=FALSE),
                                      sliderInput("pred.int", "Forecast Horizon", min=1,max=30,value=21),
                                      
                                      width=3), # sidebarPanel
                                    mainPanel(
                                      plotOutput("myForecast.plot", hover='myForecast.hover', height='500'),
                                      verbatimTextOutput("myForecast.info"),
                                      
                                      width=9) # mainPanel
                                    
                           ), # Forecasting, tabPanel
                           tabPanel("Augmented Forecast",
                                    sidebarPanel(
                                      tags$head(tags$script('$(document).on("shiny:connected", function(e) {
                                                 Shiny.onInputChange("innerWidth", window.innerWidth);
                                                 });
                                                 $(window).resize(function(e) {
                                                 Shiny.onInputChange("innerWidth", window.innerWidth);
                                                 });
                                                 ')),
                                       tags$h3("Augmented Forecast"),
                                        p("(Experimental): This method builds a forecast of total cases, total deaths, etc. based on the daily forecast
                                          of the same variable. "),
                                       selectInput("aug.loc", "Location:", myLocations, selected="Brazil",multiple=FALSE),
                                       dateInput("aug.ts.end", "Forecast Start (YYYY-MM-DD)",
                                                    value="2020-07-01",
                                                     min = "2020-01-15",
                                                     max=recent),
                                       selectInput("aug.ts.var", "Variable", myVars.new,selected="new_cases",multiple=FALSE),
                                       selectInput("aug.fore.type", "Forecast Model", myFuns, multiple=FALSE),
                                       sliderInput("aug.pred.int", "Forecast Horizon", min=1,max=30,value=21),

                                    width=3), # sidebarPanel
                                    mainPanel(
                                                 plotOutput("myAugmentedforecast.plot", hover='myAugmentedforecast.hover', height='500'),
                                                 verbatimTextOutput("myAugmentedforecast.info"),

                                    width=9) # mainPanel

                           ), # Forecasting Performance, tabPanel
                           
                           tabPanel("X-Y Plotting", 
                                    sidebarPanel(
                                      tags$head(tags$script('$(document).on("shiny:connected", function(e) {
                            Shiny.onInputChange("innerWidth", window.innerWidth);
                            });
                            $(window).resize(function(e) {
                            Shiny.onInputChange("innerWidth", window.innerWidth);
                            });
                            ')),
                                      tags$h3("X-Y Plotting"),
                                      p("Generic X-Y plotting of any two variables."),
                                      selectInput("xy.loc", "Location", myLocations, selected='United States', multiple=FALSE),
                                      selectInput("x", "X", myVars, multiple = FALSE,selected = 'total_cases_per_million'),
                                      selectInput("y", "Y", myVars, multiple = FALSE,selected = 'total_deaths_per_million'),
                                      width=3), # sidebarPanel
                                    mainPanel(
                                      plotOutput("xy.plot", height="500", hover='xy.hover'),
                                      verbatimTextOutput("xy.info"),
                                      
                                      width=9) # mainPanel
                           ), # X-Y Plotting, tabPanel
                           
                           tabPanel(
                             "About",
                             sidebarPanel(
                               h3("The Author"),
                               p("I am a recent Texas A&M graduate currently residing in Orlando, FL. After graduating with a BSc. of Physics in May, 
                               I began self learning R through online courses and this project. This project is the culmination of
                               over 80+ hours of work and research, and aims to provide some insight into the complex problem that is COVID-19, 
                               while allowing the user to explore trends on their own. The Github repository for this project can be found", 
                                 a(href = 'https://github.com/johnfmaner/covid19', 'here'),"."),
                               
                               HTML('<script type="text/javascript" src="https://platform.linkedin.com/badges/js/profile.js" async defer></script>'),
                               HTML("<center><div class=\"LI-profile-badge\"  data-version=\"v1\" data-size=\"large\" data-locale=\"en_US\" data-type=\"horizontal\" 
          data-theme=\"light\" data-vanity=\"johnfmaner\"><a class=\"LI-simple-link\" 
          href=\'https://www.linkedin.com/in/johnfmaner?trk=profile-badge\'>John Maner</a></div></center><br><br><br>"),

                               HTML('<center><iframe src="https://githubbadge.appspot.com/johnfmaner" style="border: 0;
               height: 150px;width: 300px;overflow: hidden;" frameBorder="0"></iframe></center><br><br><br><br><br><br><br><br><br><br><br><br><br><br>'),

                               position="left", width=4), #About, sidebarPanel
                             
                             h5("A Disclaimer"),
                             p("I do not claim to be an epidemiologist in any capacity!  
      This work in no way claims to account for easing of stay-at-home mandates, social distancing, usage of face coverings, and other factors.
      This project is simply a self learning experiment with basic time series forecasting models in R, which know nothing about epidemiology. 
      Despite being derived from official data sources, any predictions produced by this work are NOT to be taken as official predictions."),
                             p("Additionally: OWID data includes corrections from official sources, which may appear as negative values when viewing new cases,
                             new tests, new deaths, etc. Currently, any negative value is set to 0 to improve forecast performance."),
                             
                             
                             h5("Data"), 
                             p("This project utilizes the Our World in Data (OWID) source data, which can be found directly at 
                              the", a(href = 'https://ourworldindata.org/coronavirus-source-data/', 'OWID Website'),".
                              OWID data is entirely open source and includes extensive documentation regarding their sources and methods. Below is an 
                              interactive courtesy of OWIDw hich displays how recent the testing data for each country is. Countries with old or no 
                              testing data are not guaranteed to work well in this application."),
                             HTML('<center><iframe src="https://ourworldindata.org/grapher/how-recent-is-the-latest-testing-data-for-each-country-last-update" 
           loading="lazy" style="width: 50%; height: 400px; border: 0px none;"></iframe></center>'),
                             
                             h5("Methods"),
                             p("Forecasted values are calculated using the", 
                             a(href = 'https://cran.r-project.org/web/packages/forecast/index.html', 'forecast'), "package in R. 
      A geographic subset of data is first created according to the specified country. This data is then formatted as a time series of one variable composed of
      the longest contigous portion of data, starting at the first non zero value for the variable, and ends at the user specified date. A forecast is then built according to 
      the user selected forecasting model, and visualized. "),
                             p("The augmented forecast is an experimental forecast of total values built on the daily forecast of the corresponding variable. 
                             Although far from perfect, this method can significantly narrow the prediction confidence intervals, and in some cases, produces 
                               more accurate results than directly forecasting the total counts. To reiterate, this method is completely experimental, and *may
                               be overfitting the data. I am continuing to learn about timeseries forecasting and forecasting diagnostics, and will continue to
                               update this project with some diagnostics until satisfied. ")

                           )# tabPanel About
                           
                           
                ) # navbarPage
) # fluidPage


# Define server function  
server <- function(input, output, session) {
  
  output$myForecast.plot <- renderPlot({
    myForecast.plot(source=dataIn, 
                    loc= input$loc, 
                    ts.var=input$ts.var,
                    ts.end=ymd(input$ts.end), 
                    fore.type=input$fore.type, 
                    pred.int=input$pred.int)
  })
  
  output$myForecast.info <- renderText({
    #https://shiny.rstudio.com/articles/plot-interaction.html   
    xy_str <- function(e) {
      if(is.null(e)) return("")
      paste("Date: ", day.date(e$x), "\n", var.name(input$ts.var),": ", floor(e$y), sep="")
    }
    
    xy_str(input$myForecast.hover)
  })
  
  output$myAugmentedforecast.plot <- renderPlot({
    myAugmentedforecast.plot(source=dataIn, 
                             loc= input$aug.loc, 
                             ts.var=input$aug.ts.var,
                             ts.end=ymd(input$aug.ts.end), 
                             fore.type=input$aug.fore.type, 
                             pred.int=input$aug.pred.int)
  })
  
  output$myAugmentedforecast.info <- renderText({
    #https://shiny.rstudio.com/articles/plot-interaction.html   
    xy_str <- function(e) {
      if(is.null(e)) return("")
      paste("Date: ", day.date(e$x), "\n", var.name(input$aug.ts.var),": ", floor(e$y), sep="")
    }
    
    xy_str(input$myAugmentedforecast.hover)
  })
  
  output$xy.plot <- renderPlot({
    mySource <- myCountry(dataIn, input$xy.loc)
    xy.plot(source = mySource, 
            x=input$x, y=input$y)
  })
  
  output$xy.info <- renderText({
    #https://shiny.rstudio.com/articles/plot-interaction.html   
    xy_str <- function(e) {
      if(is.null(e)) return("")
      paste(var.name(input$x),": ", floor(e$x), "\n", new2total(var.name(input$y)),": ", floor(e$y), sep="")
    }
    
    xy_str(input$xy.hover)
  })
  
} # server

# Create Shiny object
shinyApp(ui = ui, server = server)
# 
# test <- myTimeseries(myCountry(dataIn, "Brazil"), "total_cases", day.date(174))
# fcast<- myForecast(test, "auto.arima", 30)
# date.day(day.date(date.day(recent) - 30))
# xp<- c(seq(174 + 1, 174 + 30), seq(174 + 30, 174+1))
# yp<- c(fcast$upper, rev(fcast$lower))
# 
# length(xp)
# length(yp)
# 
# plot(fcast$mean,xlim=c(170,210),ylim=c(1000000,3000000))
# polygon(xp,yp,col='#AFD9FF',border=NA)
# lines(fcast$mean, lty=2)
# lines(myTimeseries(myCountry(dataIn, "Brazil"), "total_cases", recent),type='h')
# 
#       

# myAugmentedforecast.plot(dataIn, "Brazil", "new_cases", recent, "auto.arima", 30)

