# Load R packages
library(shiny)
library(shinythemes)
source("~/Documents/projects/covid19/app/appHelpers.R")

#hard coded for the time being. 
dataIn <- read.csv("~/Documents/projects/covid19/data/owid-covid.csv")
recent <- recent.date(dataIn)
scipen(999) #disable scientific notation for prettier plots

  # Define UI
  ui <- fluidPage(theme = shinytheme("paper"),
    navbarPage("COVID-19 Data Visualization", 
      tabPanel("Forecasting",
               sidebarPanel(
                  tags$h3("Inputs:"),
                  selectInput("loc", "Location:", myLocations,selected="Brazil",multiple=FALSE),
                  # dateRangeInput("ts.range", "Date range (YYYY-MM-DD):",
                  #         "inDateRange",        
                  #         format = "yyyy-mm-dd",
                  #         separator = "-"
                  #         ),
                  dateRangeInput("ts.range", "Date range (YYYY-MM-DD)",
                                start = "2020-01-15",
                                min = "2020-01-15",
                                end=recent,
                                max=recent),
                  selectInput("ts.var", "Variable", myVars,selected="new_cases",multiple=FALSE),
                  selectInput("fore.type", "Forecast Model", myFuns,multiple=FALSE),
                  sliderInput("pred.int", "Prediction Interval", min=0,max=30,value=10),

                 
               width=3), # sidebarPanel
               mainPanel(
                            # h1("Testing Outputs"),

                            # h4("loc"),
                            # verbatimTextOutput("loc"),

                            # h4("ts.start"),                            
                            # verbatimTextOutput("ts.start"),

                            # h4("ts.end"),
                            # verbatimTextOutput("ts.end"),

                            # h4("ts.var"),
                            # verbatimTextOutput("ts.var"),   

                            # h4("fore.type"),
                            # verbatimTextOutput("fore.type"),

                            # h4("pred.int"),
                            # verbatimTextOutput("pred.int")

                            h4("Forecast Plot"),
                            plotOutput("myForecast.plot"),

               width=9) # mainPanel
               
      ), # COVID-19, tabPanel
      tabPanel("Navbar 2", "This panel is intentionally left blank"),
      tabPanel("About",
      h5("DISCLAIMER"),
      p("I do not claim to be an epidemiologist in any capacity. 
      This work in no way claims to account for easing of stay-at-home mandates, social distancing, mask usage, and other factors.
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
      a(href = 'https://github.com/johnfmaner/covid19', 'here'),"."),
      )# tabPanel About


    ) # navbarPage
  ) # fluidPage

  
  # Define server function  
  server <- function(input, output, session) {
    
    output$loc <- renderText({
      input$loc 
    })

    output$ts.start <- renderText({
      format(input$ts.range[1])
    })

    output$ts.end <- renderText({
      format(input$ts.range[2])
    })

    output$ts.var <- renderText({
      input$ts.var
    })

    #trying to automate start date, however, may just have to settle with a fixed date.
    # observe({updateDateRangeInput(session, "inDateRange",
    #     start = find.start(myCountry(dataIn, input$loc), 
    #                       input$ts.var),
    #     end = recent,
    #     min = "2020-01-01",
    #     max = recent
    #     )
    # })

    output$fore.type <- renderText({
      input$fore.type
    })

    output$pred.int <- renderText({
      input$pred.int
    })

    output$myForecast.plot <- renderPlot({
      myForecast.plot(source=dataIn, 
                    loc= input$loc, 
                    ts.var=input$ts.var,
                    ts.start=ymd(input$ts.range[1]),
                    ts.end=ymd(input$ts.range[2]), 
                    fore.type=input$fore.type, 
                    pred.int=input$pred.int)
    })
  } # server
  
  # Create Shiny object
  shinyApp(ui = ui, server = server)
