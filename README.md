# shinyga
Easier Google Authentication Dashboards in Shiny

## Installation

    library(devtools)
    devtools::install_github("MarkEdmondson1234/shinyga", auth_token = "7d1ec030bb8aa845276dffb0f5ed4b8c1b728206")
    
    library(shiny)
    library(shinyapps)
    library(shinydashboard)
    library(shinyga)

## Use (to be fleshed out)

Read how to use Shiny apps first.

Then:
 
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

