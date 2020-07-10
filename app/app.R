# Load R packages
library(shiny)
library(shinythemes)
source("~/Documents/projects/covid19/app/appHelpers.R")

#hard coded for the time being. 
dataIn <- read.csv("~/Documents/projects/covid19/data/owid-covid.csv")
recent <- recent.date(dataIn)

  # Define UI
  ui <- fluidPage(theme = shinytheme("sandstone"),
    navbarPage("COVID-19 Forecasting", 
      tabPanel("Forecasting",
               sidebarPanel(
                  tags$h3("Inputs:"),
                  selectInput("loc", "Location:", myLocations),

                  dateRangeInput("ts.range", "Date range (YYYY-MM-DD):",
                          start  = "2020-01-01",
                          end    = recent,
                          min    = "2020-01-01",
                          max    = recent,
                          format = "yyyy-mm-dd",
                          separator = " - "),

                  selectInput("ts.var", "Variable", myVars),
                  selectInput("fore.type", "Forecast Model", myFuns),
                  sliderInput("pred.int", "Prediction Interval", min=0,max=100,value=30)

                 
               ), # sidebarPanel
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
                            plotOutput("myForecast.plot")

               ) # mainPanel
               
      ), # COVID-19, tabPanel
      tabPanel("Navbar 2", "This panel is intentionally left blank"),
      tabPanel("About", 
      "This project utilizes the Our World in Data source data, which can be found directly at the OWID website, or Github. ")

    ) # navbarPage
  ) # fluidPage

  
  # Define server function  
  server <- function(input, output) {
    
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
