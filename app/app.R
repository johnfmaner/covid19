# Load R packages
library(shiny)
library(shinythemes)
source("~/Documents/projects/covid19/app/appHelpers.R")

#hard coded for the time being. 
dataIn <- read.csv("~/Documents/projects/covid19/data/owid-covid.csv")
recent <- recent.date(dataIn)
options(scipen=999) #(try to) disable scientific notation for prettier plots

  # Define UI
  ui <- fluidPage(theme = shinytheme("paper"),
    navbarPage("COVID-19 Data Visualization", 
      tabPanel("Forecasting",
               sidebarPanel(
                 tags$head(tags$script('$(document).on("shiny:connected", function(e) {
                            Shiny.onInputChange("innerWidth", window.innerWidth);
                            });
                            $(window).resize(function(e) {
                            Shiny.onInputChange("innerWidth", window.innerWidth);
                            });
                            ')),
                  tags$h3("Inputs:"),
                  selectInput("loc", "Location:", myLocations,selected="Brazil",multiple=FALSE),
                  # dateRangeInput("ts.range", "Date range (YYYY-MM-DD):",
                  #         "inDateRange",        
                  #         format = "yyyy-mm-dd",
                  #         separator = "-"
                  #         ),
                  dateRangeInput("ts.range", "Date range (YYYY-MM-DD)",
                                start = "2020-01-22", #Days since 30 total cases 
                                min = "2020-01-22",
                                end=recent,
                                max=recent),
                  selectInput("ts.var", "Variable", myVars,selected="new_cases",multiple=FALSE),
                  selectInput("fore.type", "Forecast Model", myFuns,multiple=FALSE),
                  sliderInput("pred.int", "Prediction Interval", min=0,max=30,value=10),

                 
               width=3), # sidebarPanel
               mainPanel(
                            h4("Forecast Plot"),
                            plotOutput("myForecast.plot"),

               width=9) # mainPanel
               
      ), # COVID-19, tabPanel

      tabPanel("X-Y Plotting", 
      sidebarPanel(
                 tags$head(tags$script('$(document).on("shiny:connected", function(e) {
                            Shiny.onInputChange("innerWidth", window.innerWidth);
                            });
                            $(window).resize(function(e) {
                            Shiny.onInputChange("innerWidth", window.innerWidth);
                            });
                            ')),
                  tags$h3("X-Y Plotting:"),
                  selectInput("xy.loc", "Location", myLocations, selected='United States', multiple=FALSE),
                  selectInput("x", "X", myVars, ,multiple = FALSE,selected = 'new_cases'),
                  selectInput("y", "Y", myVars, multiple = FALSE,selected = 'new_tests'),
               width=3), # sidebarPanel
               mainPanel(
                         plotOutput("xy.plot"),
               width=9) # mainPanel
      ), # X-Y Plotting, tabPanel

      tabPanel(
          "About",
          sidebarPanel(
          HTML('<script type="text/javascript" src="https://platform.linkedin.com/badges/js/profile.js" async defer></script>'),
          HTML("<center><div class=\"LI-profile-badge\"  data-version=\"v1\" data-size=\"large\" data-locale=\"en_US\" data-type=\"horizontal\" 
          data-theme=\"light\" data-vanity=\"johnfmaner\"><a class=\"LI-simple-link\" 
          href=\'https://www.linkedin.com/in/johnfmaner?trk=profile-badge\'>John Maner</a></div></center>"),
          br(),
          br(),
          HTML('<center><iframe src="https://githubbadge.appspot.com/johnfmaner" style="border: 0;height: 150px;width: 300px;overflow: hidden;" frameBorder="0"></iframe></center>'),
          position="left", width=5), #About, sidebarPanel

      h5("DISCLAIMER"),
      p("I do not claim to be an epidemiologist in any capacity. 
      This work in no way claims to account for easing of stay-at-home mandates, social distancing, face covering usage, and other factors.
      This project is simply a self learning experiment with time series forecasting models in R, which know nothing about epidemiology. 
      Despite being derived from official data, any predictions produced by this work are not to be taken as official predictions."),

      h5("Data"), 
      p("This project utilizes the Our World in Data (OWID) source data, which can be found directly at 
      the", a(href = 'https://ourworldindata.org/coronavirus-source-data/', 'OWID Website'),".
      OWID data is entirely open source and provides extensive documentation regarding their sources and methods. "),

      h5("The Creator"),
      p("I am a recent Texas A&M graduate currently residing in Orlando, FL. After graduating with a BSc. in Physics, I began self learning
      the R language through online courses and this project. This project is the culmination of over 70+ hours of work and research, and 
      aims to provide some insight into the complex problem that is COVID-19, while allowing the user to explore trends on their own.
      The repository for this project can be found", 
      a(href = 'https://github.com/johnfmaner/covid19', 'here'),".")
      )# tabPanel About


    ) # navbarPage
  ) # fluidPage

  
  # Define server function  
  server <- function(input, output, session) {
    
    #trying to automate start date, however, may just have to settle with a fixed date.
    # observe({updateDateRangeInput(session, "inDateRange",
    #     start = find.start(myCountry(dataIn, input$loc), 
    #                       input$ts.var),
    #     end = recent,
    #     min = "2020-01-01",
    #     max = recent
    #     )
    # })

    output$myForecast.plot <- renderPlot({
      myForecast.plot(source=dataIn, 
                    loc= input$loc, 
                    ts.var=input$ts.var,
                    ts.start=ymd(input$ts.range[1]),
                    ts.end=ymd(input$ts.range[2]), 
                    fore.type=input$fore.type, 
                    pred.int=input$pred.int)
    })

    output$xy.plot <- renderPlot({
      mySource <- myCountry(dataIn, input$xy.loc)
      xy.plot(source = mySource, 
              x=input$x, y=input$y)
    })
  } # server

  # Create Shiny object
  shinyApp(ui = ui, server = server)
