# shinyga (Shiny Google Authentication)
Easier Google Authentication Dashboards in Shiny.

The functions in the package were used to help create these demo apps:

* [GA Effect dashboard](http://markedmondson.me/how-i-made-ga-effect-creating-an-online-statistics-dashboard-using-reais)
* [GA Rollup dashboard](https://mark.shinyapps.io/ga-rollup/)

**Get in touch if you have any dashboards made with this package to get a link from here**

## Change history
### Version 0.1.2 - March 29th 2015
* Fixed bug where older GA accounts couldn't fetch segments or goals.
* Support for Google Sheets via Jennifer Bryan's [googlesheets](https://github.com/jennybc/googlesheets) package.  

Call via the 'type="googlesheets"' in doAuthMacro()

      auth <- doAuthMacro(input, output, session,
                          securityCode,
                          client.id     = "xxxxx.apps.googleusercontent.com",
                          client.secret = "xxxxx",
                          type = "googlesheets"
                          )

If you don't specify type then it defaults to Google Analytics. ("analytics"). Doing Analytics renders the GA account dropdowns etc.  This is not needed for Google Sheets.

### Version 0.1.1 - March 27th 2015
* Port everything to httr, get rid of RCurl, RJSONIO 
* Segments will default to default GA ones if it can't find or parse yours
* Removed need for redirect URL in Auth macro, Shiny will detect App URL from session info.

### Version 0.1.0 - March 18th 2015
* Initial Release

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

* Create a Project
* Go to APIs & Auth - Activate the appropriate Google API (only necessary for Analytics atm)
* Go to APIs & Auth - Credentials.
* Create new Client ID for web application
* Note your Client ID and Client secret
* Put the URL of your app in the Redirect URIs, one per line. See below.
* Fill in some details on your consent screen

### Client URL: Running Locally
For local the Shiny runApp() uses a random port, so specify using runApp(port=1234) and put that in the Google API console as your port number e.g. http://127.0.0.1:1234

### Client URL: Running on Shiny Server or Shinyapps.io
If you use the doAuthMacro() functions it will detect your app URL for you, otherwise you will need to specify it via the redirect.uri parameter in the underlying authentication functions. 

Use the URL where your app is published as your CLIENT_URL.  You can put both your local and live URL in the Google API console. 

TIP: Comment out the local one when you are ready to deploy. e.g. https://mark.shinyapps.io/ga-effect/

## Run Shiny

Read how to use [Shiny apps](http://shiny.rstudio.com/) before using this package.  

This package also uses [shinydashboard](http://rstudio.github.io/shinydashboard/) to make the pretty layout, but its not strictly necessary.

# Shiny App Code Examples

Examples of minimal working examples are shown below, for you to adapt. 

## Google Analytics
 
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
                          client.secret = "xxxxxxxxxxxx",
                          type = "analytics")
  
      ## auth returns auth$table() and auth$token() to be used in API calls.
  
      ## get the GA data ----------------------------------------------------------
      gadata <- reactive({
        validate(
          need(auth$token(), "Authentication needed"))
    
        df    <- auth$table()
        token <- auth$token()
        gaid  <- as.character(input$view)
        
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
    
## Google Sheets via googlesheets package

    ###### server.r
    
    library(shiny)
    library(shinyga)
    library(googlesheets)
    
    securityCode <- createCode()
    
    shinyServer(function(input, output, session) {
    
      auth <- doAuthMacro(input, output, session,
                          securityCode,
                          client.id     = "xxxxx.apps.googleusercontent.com",
                          client.secret = "xxxxx",
                          type = "gspreadr"
                          )
    
      doc_data <- reactive({
        validate(
          need(auth$token(), "Authenticate")
          )
      
        ## from gspreadr
        list_sheets()
      
      })
      
      output$your_sheets <- renderDataTable({
        validate(
          need(doc_data(), "no data")
          )
          
        doc_data()
        
        })
        
      })
    
    
    ###### ui.r
    library(shiny)
    library(shinyga)
    
    shinyUI(fluidPage(
    
      titlePanel("gspreadr Demo"),
      
      sidebarLayout(
        sidebarPanel(
          uiOutput("AuthGAURL")
          ),
          
      mainPanel(
        dataTableOutput("your_sheets")
        )
      )
      
    ))