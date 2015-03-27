# shinyga (Shiny Google Authentication)
Easier Google Authentication Dashboards in Shiny.

The functions in the package were used to help create the [GA Effect dashboard](http://markedmondson.me/how-i-made-ga-effect-creating-an-online-statistics-dashboard-using-reais)

## Change history
Version 0.1.1 - move everything to httr, segment fetch backup if it fails to find your own.

Version 0.1.0 - Release

## Help 
Please tell me any bugs or problems with examples or documentation in the [issue tracker](https://github.com/MarkEdmondson1234/shinyga/issues)

Most functions have help files, start with ?shinyga

The package focused on quick setup of Google Authentication APIs, with initial focus on Google Analytics, so please keep that in mind when asking for features. 

## What shinyga does

Provides utility functions to help easily setup up a Google API authentication flow in Shiny. The focus is on Google Analytics initially, with macros to help easy download of GA data, but to be expanded later into other Google APIs such as Google Tag Manager, Gmail, BigQuery etc.

## Installation

    library(devtools)
    
    ## install shinydashboard dependency
    devtools::install_github("rstudio/shinydashboard")
    
    ## install shinyga
    devtools::install_github("MarkEdmondson1234/shinyga")
    
    library(shiny)
    library(shinydashboard)
    library(shinyga)
    
    ?shinyga

# To Use

## Get Google API credentials

Get your client secret, ID from the [Google API console](https://ga-dev-tools.appspot.com/explorer/)

Activate Analytics API

Fill in details, get the client ID, secret and URL.

### Client URL: Running Locally
For local the Shiny runApp() uses a random port, so specify using runApp(port=1234) and put that in the Google API console as your port number e.g. 127.0.0.1:1234 and use for the CLIENT_URL in the shinyga app.

### Client URL: Running on Shiny Server or Shinyapps.io
Use the URL where your app is published as your CLIENT_URL.  You can put both your local and live URL in the Google API console, and comment out the local one when you are ready to deploy.

## Run Shiny

Read how to use [Shiny apps](http://shiny.rstudio.com/) before using this package.

# Example Shiny App Code
 
    ###### server.r
    
    library(shiny)
    library(shinydashboard)
    library(shinyga)
    
    securityCode <- createCode()
    
    shinyServer(function(input, output, session){
  
      ## returns list of token and profile.table ----------------------------------
      auth <- doAuthMacro(input, output, session,
                          securityCode,
                          ## client info taken from Google API console.
                          client.id     = "xxxxx.apps.googleusercontent.com",
                          client.secret = "xxxxxxxxxxxx")
  
      ## auth returns auth$table() and auth$token() to be used in API calls.
  
      ## get the GA data ----------------------------------------------------------
      gadata <- reactive({
        validate(
          need(auth$token(), "Authentication needed"))
    
        df    <- auth$table()
        token <- auth$token()
        gaid <- as.character(input$view)
        
        profileRow <- df[df$id %in% gaid,] 
        
        data <- rollupGA(GAProfileTable = profileRow,
                         dimensions = 'ga:date',
                         start_date = '2014-09-01',
                         metrics = 'ga:sessions',
                         end_date = '2015-03-01',
                         ga = token)
                         
        data[,c('date','sessions')]
      }) 
  
  
      ## do a plot! ---------------------------------------------------------------
      output$gaplot <- renderPlot({
        validate(
          need(gadata(), "Authenticate to see"))
    
        plot(gadata(), type="l") 
    
        })
  
    })

    
    ##### ui.r
    
    library(shiny)
    library(shinydashboard)
    library(shinyga)

    dashboardPage(
      dashboardHeader(
        title = "Shiny Google Authentication demo",
        dropdownMenuOutput("messageMenu")
      ),
      dashboardSidebar(
        sidebarMenu(
          menuItem("Tab1", tabName = "setup", icon = icon("gears")),
          menuItem("Tab2", tabName = "dash", icon = icon("dashboard")))
        ),
      dashboardBody(
        uiOutput("AuthGAURL"),
        authDropdownRow(),
        plotOutput("gaplot")
      )
    )

