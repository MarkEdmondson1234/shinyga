# shinyga (Shiny Google Authentication)
Easier Google Authentication Dashboards in Shiny

Pre-release version, please tell me any bugs in (issue tracker)[/issues]

Package focused on quick setup of Google Authentication APIs, with initial focus on Google Analytics.

## What it does

Utility functions to help easily setup up a Google API authentication flow.  Focus on Google Analytics initially, with macros to help easy download of GA data, but to be expanded into other Google APIs.

## Installation

    library(devtools)
    devtools::install_github("MarkEdmondson1234/shinyga")
    
    library(shiny)
    library(shinyapps)
    library(shinydashboard)
    library(shinyga)

## To Use (to be fleshed out)

Get your client secret, ID from the (Google API console)[https://ga-dev-tools.appspot.com/explorer/]

Your URL is either local (127.0.0.1:XXXX) or the URL of where your Shiny app is hosted on Shiny Server or on Shinyapps.io

For local the Shiny runApp() uses a random port, so specify using runApp(port=1234) and put that in the Google API console as your port number e.g. 127.0.0.1:1234

Read how to use (Shiny apps)[http://shiny.rstudio.com/]

Then (todo check example):
 
    ###### server.r
    
    ## client info taken from Google API console.
    CLIENT_ID      <-  "xxxxx.apps.googleusercontent.com"
    CLIENT_SECRET  <-  "xxxxxxxxxxxx"
    CLIENT_URL     <-  'https://your-account.shinyapps.io/your-app/'
    ## comment out for deployment, in for local testing via runApp(port=6423)
    CLIENT_URL     <-  'http://127.0.0.1:6423' 
    
    securityCode <- createCode()
    
    shinyServer(function(input, output, session)){
    
    ## returns list of token and profile.table
    auth <- doAuthMacro(input, output, session,
                        securityCode,
                        client.id     = CLIENT_ID,
                        client.secret = CLIENT_SECRET, 
                        client.uri    = CLIENT_URL)
    
    ga.token         <- auth$token
    profile.table    <- auth$table
    
    ## call the token for API calls
    
    gadata <- reactive({
    
        rollupGA(GAProfileTable = profile.table(),
                 dimensions     = 'ga:date',
                 start_date     = '2014-03-13',
                 end_date       = '2015-03-13'
                 metrics        = 'ga:sessions',
                 ga             = ga.token())
                 
                 }) 
        
     
     output$gaplot <- renderPlot(gadata)
     
    }
    
    ##### ui.r
    
    dashboardPage(dashboardHeader(
                     title = "shinyGA demo",
                     dropdownMenuOutput("messageMenu")
                     ),
                  dashboardSidebar(
                      sidebarMenu(
                          menuItem("Setup", tabName = "setup", icon = icon("gears")),
                          menuItem("Results", tabName = "dash", icon = icon("dashboard"))
                          ),
                  dashboardBody(
                      authDropdownRow(),
                      renderPlot("gaplot")
                      )
    )

