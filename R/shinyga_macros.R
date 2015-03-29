#' authDropdownRow - creates a row of GA authentication menus
#' 
#' Use with renderAuthDropdownRow()
#' Creates a row of select boxes for GA Account, Webproperty and View.
#' id needed for GA fetches is then available in input${view.id}
#' 
#' @param account.id The shiny id for accounts. Then available at input$<account.id>.
#' @param web.prop.id The shiny id for web properties. Then available at input$<web.prop.id>.
#' @param view.id The shiny id for views. Then available at input$<view.id>.
#' @param multiple Whether you have multi-select on menus. Default FALSE.
#' @seealso Shortcut using \code{\link{doAuthMacro}}.
#' @return A shinydashboard function that generates necessary HTML.
#' @family shiny macro functions
#' @examples
#' \dontrun{
#'  ## server.r
#' shinyServer(function(input, output, session) {
#' 
#'     renderAuthDropdownRow(ga.table = ShinyMakeGAProfileTable(),
#'                           input = input,
#'                           session = session)
#'     
#' }
#' 
#'  ## ui.r
#'  
#'  library(shinydashboard)
#'  dashboardBody(authDropdownRow())
#'  
#'  }
authDropdownRow <- function(account.id  = "accounts",
                            web.prop.id = "web.prop",
                            view.id     = "view",
                            multiple=FALSE){
  fluidRow(
    box(
      selectInput(account.id,
                  label=paste0("Select Account", ifelse(multiple, "s.  (Multiple allowed)","")),
                  choices = NULL,
                  multiple = multiple)
      , width = 4, title="Select Account", status="success", solidHeader=TRUE),
    box(
      selectInput(web.prop.id,
                  label=paste0("Select Web Propert", ifelse(multiple, "ies.  (Multiple allowed)","y")),
                  choices = NULL,
                  multiple = multiple)
      , width = 4, title="Select Web Property", status="success", solidHeader=TRUE),
    box(
      selectInput(view.id,
                  label=paste0("Select View", ifelse(multiple, "s.  (Multiple allowed)","")),
                  choices = NULL,
                  multiple = multiple)
      , width = 4, title="Pick View (ID)", status="success", solidHeader=TRUE)
  )
}

#' renderAuthDropdownRow - creates a row of GA authentication menus
#' 
#' Use with authDropdownRow
#' Creates a row of select boxes for GA Account, Webproperty and View.
#' id needed for GA fetches is then available in input${view.id}
#' 
#' @param ga.table A table of GA profiles from getAndMergeGAAccounts().
#' @param input The shiny input object.
#' @param session The shiny session object.
#' @param account.id The shiny id for accounts. Then available at input$<account.id>.
#' @param web.prop.id The shiny id for web properties. Then available at input$<web.prop.id>.
#' @param view.id The shiny id for views. Then available at input$<view.id>.
#' @return Nothing.
#' @seealso Shortcut using \code{\link{doAuthMacro}}.
#' @family shiny macro functions
#' @examples
#' 
#' \dontrun{
#'  ## server.r
#' shinyServer(function(input, output, session) {
#' 
#'     renderAuthDropdownRow(ga.table = ShinyMakeGAProfileTable(),
#'                           input = input,
#'                           session = session)
#'     
#' }
#' 
#'  ## ui.r
#'  
#'  library(shinydashboard)
#'  dashboardBody(authDropdownRow(),
#'                metricSelect())
#'  
#'  }
renderAuthDropdownRow <- function(ga.table,
                                  input,
                                  session,
                                  account.id  = "accounts",
                                  web.prop.id = "web.prop",
                                  view.id     = "view"){
  
  if(!is.null(account.id)){
    observe({
      validate(
        need(ga.table, "Need profiles")
      )
      pList  <- ga.table[,c('name','webPropertyId','websiteUrl','profilename', 'id')]
      
      choice <- pList$name
      
      updateSelectInput(session, 
                        account.id,
                        label="Accounts",
                        choices = choice)
    })
  }
  
  if(!is.null(web.prop.id)){
    observe({
      validate(
        need(ga.table, "Need profiles")
      )
      pList  <- ga.table[,c('name','webPropertyId','websiteUrl','profilename', 'id')]
      
      pList <- pList[pList$name %in% input$accounts,]
      
      choice <- pList$websiteUrl
      
      updateSelectInput(session, 
                        web.prop.id,
                        label="WebProperty",
                        choices = choice)
    })
  }
  
  if(!is.null(view.id)){
    
    observe({
      validate(
        need(ga.table, "Need profiles")
      )
      pList <- ga.table[,c('name','webPropertyId','websiteUrl','profilename', 'id')]
      
      pList <- pList[pList$websiteUrl %in% input$web.prop,]
      
      choice <- pList$id 
      
      names(choice) <- paste(pList$profilename, pList$id)
      
      updateSelectInput(session, 
                        view.id,
                        label="Views",
                        choices = choice)
    })
    
  }
}

### creates a selection box with metrics to choose
### metric then available in input${inputId}

#' metricSelect - creates a selection of which metric
#' 
#' Creates a select box with GA metrics. Metric var then available in input$<inputId>
#' 
#' @param inputId The id of the input.  Then available in input$<inputId>.
#' @family shiny macro functions
#' @return A shinydashboard function that generates necessary HTML.
#' @examples
#' \dontrun{
#'  ## ui.r
#'  
#'  library(shinydashboard)
#'  dashboardBody(authDropdownRow(),
#'                metricSelect())
#'          }      
metricSelect  <- function(inputId="metric_choice"){
  selectInput(inputId,
              label="Metric",
              choices = c("Sessions" = "sessions",
                          "Users" = "users",
                          "New Users" = "newUsers",
                          "Page Views" = "pageviews",
                          "Revenue" = "transactionRevenue",
                          "Transactions" = "transactions",
                          "Goal 1 Completions" = "goal1Completions",
                          "Goal 2 Completions" = "goal2Completions",
                          "Goal 3 Completions" = "goal3Completions",
                          "Goal 4 Completions" = "goal4Completions",
                          "Goal 5 Completions" = "goal5Completions",
                          "Goal 6 Completions" = "goal6Completions",
                          "Goal 7 Completions" = "goal7Completions",
                          "Goal 8 Completions" = "goal8Completions",
                          "Goal 9 Completions" = "goal9Completions",
                          "Goal 10 Completions" = "goal10Completions",
                          "Goal 11 Completions" = "goal11Completions",
                          "Goal 12 Completions" = "goal12Completions",
                          "Goal 13 Completions" = "goal13Completions",
                          "Goal 14 Completions" = "goal14Completions",
                          "Goal 15 Completions" = "goal15Completions",
                          "Goal 16 Completions" = "goal16Completions",
                          "Goal 17 Completions" = "goal17Completions",
                          "Goal 18 Completions" = "goal18Completions",
                          "Goal 19 Completions" = "goal19Completions",
                          "Goal 20 Completions" = "goal20Completions",
                          "Total Events" = "totalEvents",
                          "Unique Events" = "uniqueEvents",
                          "Event Value" = "eventValue")
  )
}

#' Quick setup of shinyGA authentication
#' 
#' This function calls all the other authentication functions so you can quick start.
#' Sacrifices customisation for speed.
#' 
#' @param securityCode A unique session code, such as from createCode()
#' @param client.id The client ID taken from the Google API Console.
#' @param client.secret The client secret taken from the Google API Console.
#' @param input Shiny input object.
#' @param output Shiny output object.
#' @param session Shiny session object.
#' @param type Type of Google Authentication. c("analytics", "gspreadr")
#' 
#' @return
#' A named list. See example for uses in shinyServer().
#' \describe{
#'   \item{token}{Google Authentication Token needed for API calls.}
#'   \item{table}{Table of Google Analytics Profiles needed for \code{\link{rollupGA}}.}
#' }
#' 
#' The function will also create the outputs for the authentication menu.
#' These are per the defaults of \code{\link{renderAuthDropdownRow}}
#' 
#' Also outputs a DataTable called 'output$GAProfile' for use in ui.r as renderDataTable('GAProfile')
#' 
#' @seealso
#' 
#' Authentication macros \code{\link{renderAuthDropdownRow}} and \code{\link{authDropdownRow}}
#'   
#' @family authentication functions
#' @examples
#' \dontrun{
#' 
#' securityCode <- createCode()
#' 
#' shinyServer(function(input, output, session)){
#'   
#'   ## returns list of token and profile.table
#'   ## client info taken from Google API console.
#'   auth <- doAuthMacro(input, output, session,
#'                       securityCode,
#'                       client.id     = "xxxxx.apps.googleusercontent.com",
#'                       client.secret = "xxxxxxxxxxxx"
#'                       )
#'                       
#'   ga.token         <- auth$token
#'   profile.table    <- auth$table
#'  
#'   ## call the token for API calls
#'  
#'   gadata <- reactive({
#'   
#'     rollupGA(GAProfileTable = profile.table(),
#'              dimensions     = 'ga:date',
#'              start_date     = '2014-03-13',
#'              end_date       = '2015-03-13'
#'              metrics        = 'ga:sessions',
#'              ga             = ga.token())
#'                  }) 
#'                
#'   }
#' }
doAuthMacro <- function(input, output, session,
                        securityCode,
                        client.id,
                        client.secret,
                        type = "analytics"){
  
  types <- list(analytics = c("https://www.googleapis.com/auth/analytics",
                              "https://www.googleapis.com/auth/analytics.readonly"), 
                gspreadr =  c("https://spreadsheets.google.com/feeds",
                              "https://docs.google.com/feeds")
  )
  
  ## get the apps URL as default
  appURL <- reactive({
    if(!is.null(session)){
      
      paste0(session$clientData$url_protocol,
             "//",
             session$clientData$url_hostname,
             ifelse(session$clientData$url_hostname == "127.0.0.1",
                    ":",
                    session$clientData$url_pathname),
             session$clientData$url_port)
    } else {
      NULL
    }
  })
  
  AuthCode <- reactive({
    authReturnCode(session, securityCode)
  })
  
  output$AuthGAURL <- renderUI({
    validate(
      need(appURL(), "AppURL")
    )
    
    if(type == "analytics") {
      linkname <- "Analytics"
    } else if(type == "gspreadr"){
      linkname <- "Sheets"
    } else {
      linkname <- ""
    }
    
    a(paste0("Click here to authorise your Google ", 
             linkname ,
             " access"),
      href=shinygaGetTokenURL(securityCode,
                              client.id=client.id,
                              client.secret=client.secret,
                              redirect.uri=appURL(),
                              scope = types[type][[1]]
      )
    )
    
  })
  
  AccessToken <- reactive({
    validate(
      need(AuthCode(), "Authenticate To See"),
      need(appURL(), "App URL")
    )
    access_token <- shinygaGetToken(code = AuthCode(),
                                    client.id=client.id,
                                    client.secret=client.secret,
                                    redirect.uri=appURL())
    #     token <- access_token$access_token
    token <- access_token$token
  })
  
  if(type == "analytics"){
    
    GAProfileTable <- reactive({
      validate(
        need(AccessToken(), "Authentication working...")
      )
      
      AccountProfiles <- getAndMergeGAAccounts(AccessToken())
      
    })
    
    
    output$GAProfile <- renderDataTable({
      
      ga <- GAProfileTable()[,c('name',
                                'webPropertyId',
                                'websiteUrl',
                                'profilename', 
                                'id')]
      
      names(ga) <- c('account', 
                     'web property id',
                     'website url',
                     'view', 
                     'view id')
      
      ga
      
    })
    
    renderAuthDropdownRow(GAProfileTable(),
                          input,
                          session)
    
    returnme <- list(table    = GAProfileTable,
                     token    = AccessToken)
  } else if(type == "gspreadr"){
    
    returnme <- list(token = AccessToken)
  }
  
  return(returnme)
  
}


#' Quick setup of shinyga segments
#' 
#' This function creates the menu and fetches the GA segments.
#' 
#' @param input Shiny input object.
#' @param output Shiny output object.
#' @param session Shiny session object.
#' @param token GA token.

#' @return
#' A segment table
#' 
#' A DataTable called from ui.r by renderDataTable('SegmentTable')
#' 
#' A selectInput('menuSeg') called from ui.r by uiOutput("controlSeg") and input$menuSeg
#'   
#' @family shiny macro functions
#' @examples
#' \dontrun{
#' 
#' 
#' ## client info taken from Google API console.
#' CLIENT_ID      <-  "xxxxx.apps.googleusercontent.com"
#' CLIENT_SECRET  <-  "xxxxxxxxxxxx"
#' CLIENT_URL     <-  'https://mark.shinyapps.io/ga-effect/'
#' ## comment out for deployment, in for local testing via runApp(port=6423)
#' CLIENT_URL     <-  'http://127.0.0.1:6423' 
#' 
#' securityCode <- createCode()
#' 
#' shinyServer(function(input, output, session)){
#'   
#'   ## returns list of token and profile.table
#'   auth <- doAuthMacro(input, output, session,
#'                       securityCode,
#'                       client.id     = CLIENT_ID,
#'                       client.secret = CLIENT_SECRET, 
#'                       client.uri    = CLIENT_URL)
#'                       
#'   ga.token         <- auth$token
#'   profile.table    <- auth$table
#'   
#'   segments <- doSegmentMacro(input, output, session,
#'                              token=ga.token())
#'  
#'   }
#' }
doSegmentMacro <- function(input, output, session, token){
  
  output$controlSeg <- renderUI({
    
    pList <- ShinyMakeGASegmentTable()[,c('name',
                                          'segmentId',
                                          'definition',
                                          'id')]
    choice        <- pList$segmentId
    choice.names  <- pList$name
    names(choice) <- choice.names
    
    selectInput("menuSeg",
                "Your Segments",
                choices = choice,
                width = "100%")
  })
  
  output$SegmentTable <- renderDataTable({
    
    ShinyMakeGASegmentTable()[,c('name', 'definition')]
    
  })
  
  ShinyMakeGASegmentTable <- reactive({
    #get all the management API stuff
    Segments            <- shinygaGetSegments(token)
    Segments$name       <- as.character(Segments$name)
    Segments$segmentId  <- as.character(Segments$segmentId)
    Segments$definition <- as.character(Segments$definition)
    Segments$id         <- as.character(Segments$id)
    
    Segments
    
  })
}