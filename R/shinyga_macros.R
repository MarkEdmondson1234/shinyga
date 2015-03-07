#' authDropdownRow - creates a row of GA authentication menus
#' 
#' Use with renderAuthDropdownRow()
#' Creates a row of select boxes for GA Account, Webproperty and View.
#' id needed for GA fetches is then available in input${view.id}
#' 
#' @param account.id The shiny id for accounts. Then available at input$<account.id>.
#' @param web.prop.id The shiny id for web properties. Then available at input$<web.prop.id>.
#' @param view.id The shiny id for views. Then available at input$<view.id>.
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
                            view.id     = "view"){
  fluidRow(
    box(
      selectInput(account.id,
                  label="Accounts",
                  choices = NULL)
      , width = 4, title="Select Account", status="success", solidHeader=TRUE),
    box(
      selectInput(web.prop.id,
                  label="WebProperty",
                  choices = NULL)
      , width = 4, title="Select Web Property", status="success", solidHeader=TRUE),
    box(
      selectInput(view.id,
                  label="Select View",
                  choices = NULL)
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
      
      pList <- pList[input$accounts == pList$name,]
      
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
      
      pList <- pList[input$web.prop == pList$websiteUrl,]
      
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
#' @family shiny macro functions.
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