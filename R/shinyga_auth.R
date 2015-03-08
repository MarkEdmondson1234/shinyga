### Authentication functions --------------------------------------------------

# CLIENT_ID      <-  "942634425021.apps.googleusercontent.com"
# CLIENT_SECRET  <-  "Whhj5ZM3goSgWj91Q4vMNcD8"
# CLIENT_URL     <-  'https://mark.shinyapps.io/ga-effect/'
# CLIENT_URL     <-  'http://127.0.0.1:6423'

### Authentication functions
#' Creates a random character code
#' 
#' @param seed The seed for random code.
#' @param num Number of characters in code.
#' @return A random string of digits and characters.
#' @family authentication functions
#' @examples
#' \dontrun{
#' securityCode <- createCode()
#' shinyServer(function(input, output, session)){
#'   
#'   AuthCode <- reactive({
#'   
#'       authReturnCode(session, securityCode)
#'   
#'   })
#' 
#'   output$AuthGAURL <- renderUI({
#'  
#'        a("Click Here to Authorise Your Google Analytics Access", 
#'           href=shinygaGetTokenURL(securityCode)
#'           )
#'        })
#'    
#'   AccessToken <- reactive({
#'       validate(
#'         need(AuthCode(), "Authenticate To See")
#'       )
#'     
#'       access_token <- shinygaGetToken(code = AuthCode())
#'       
#'       token <- access_token$access_token
#'       
#'     })
#' }
#' }
createCode <- function(seed=NULL, num=20){
  set.seed(seed)
  
  paste0(sample(c(1:9, LETTERS, letters), num, replace = T), collapse='')
} 

#' Returns the authentication parameter "code" in redirected URLs
#' 
#' @param session A session object within a shinyServer function.
#' @param securityCode A randomly generate security code passed previously in shinygaGetTokenURL.
#' @return The authentication code from the redirect URL parameter.
#' @family authentication functions
#' @examples
#' \dontrun{
#' securityCode <- createCode()
#' shinyServer(function(input, output, session)){
#'   
#'   AuthCode <- reactive({
#'   
#'       authReturnCode(session, securityCode)
#'   
#'   })
#' 
#'   output$AuthGAURL <- renderUI({
#'  
#'        a("Click Here to Authorise Your Google Analytics Access", 
#'           href=shinygaGetTokenURL(securityCode)
#'           )
#'        })
#'    
#'   AccessToken <- reactive({
#'       validate(
#'         need(AuthCode(), "Authenticate To See")
#'       )
#'     
#'       access_token <- shinygaGetToken(code = AuthCode())
#'       
#'       token <- access_token$access_token
#'       
#'     })
#' }
#' }
authReturnCode <- function(session, securityCode){    
  
  pars <- parseQueryString(session$clientData$url_search)
  
  if(length(pars$state) > 0){ 
    if(pars$state != securityCode){
      warning("securityCode check failed in Authentication! Code:", 
              pars$state, 
              " Expected:", 
              securityCode)
      return()
    } else {
      if(length(pars$code) > 0){
        return(pars$code)
      }
    }
  }
}


#' Returns the authentication URL
#' 
#' @param state A string you pass to check authentication is correct.
#' @param client.id The client ID taken from the Google API Console.
#' @param client.secret The client secret taken from the Google API Console.
#' @param redirect.uri The URL of where your Shiny application sits, that will read state parameter.
#' @return The URL a user should click on to start authentication.
#' @family authentication functions
#' @examples
#' \dontrun{
#' securityCode <- createCode()
#' shinyServer(function(input, output, session)){
#'   
#'   AuthCode <- reactive({
#'   
#'       authReturnCode(session, securityCode)
#'   
#'   })
#' 
#'   output$AuthGAURL <- renderUI({
#'  
#'        a("Click Here to Authorise Your Google Analytics Access", 
#'           href=ShinyGetTokenURL(securityCode)
#'           )
#'        })
#'    
#'   AccessToken <- reactive({
#'       validate(
#'         need(AuthCode(), "Authenticate To See")
#'       )
#'     
#'       access_token <- ShinyGetToken(code = AuthCode())
#'       
#'       token <- access_token$access_token
#'       
#'     })
#' }
#' }
shinygaGetTokenURL <- function(state,
                               client.id     = CLIENT_ID,
                               client.secret = CLIENT_SECRET,
                               redirect.uri  = CLIENT_URL) {
  
  url <- paste('https://accounts.google.com/o/oauth2/auth?',
               'scope=https%3A%2F%2Fwww.googleapis.com%2Fauth%2Fanalytics+',
               #                'https%3A%2F%2Fwww.googleapis.com%2Fauth%2Fanalytics.edit+',
               #                'https%3A%2F%2Fwww.googleapis.com%2Fauth%2Fanalytics.manage.users+',
               'https%3A%2F%2Fwww.googleapis.com%2Fauth%2Fanalytics.readonly&',
               'state=',state,'&',
               'redirect_uri=', redirect.uri, '&',
               'response_type=code&',
               'client_id=', client.id, '&',
               'approval_prompt=auto&',
               'access_type=online', sep='', collapse='');
  return(url)
}

#' Returns the authentication Token
#' 
#' Once a user browses to ShinyGetTokenURL and is redirected back with request
#' shinygaGetToken takes that code and returns a token needed for Google APIs
#' Uses the same client.id and client.secret as ShinyGetTokenURL
#' 
#' @param code The code passed from AuthCode().
#' @param client.id The client ID taken from the Google API Console.
#' @param client.secret The client secret taken from the Google API Console.
#' @param redirect.uri The URL of where your Shiny application sits, that will read state parameter.
#' @return The token from Google that authenticates future API calls.
#' @family authentication functions
#' @examples
#' \dontrun{
#' securityCode <- createCode()
#' shinyServer(function(input, output, session)){
#'   
#'   AuthCode <- reactive({
#'   
#'       authReturnCode(session, securityCode)
#'   
#'   })
#' 
#'   output$AuthGAURL <- renderUI({
#'  
#'        a("Click Here to Authorise Your Google Analytics Access", 
#'           href=ShinyGetTokenURL(securityCode)
#'           )
#'        })
#'    
#'   AccessToken <- reactive({
#'       validate(
#'         need(AuthCode(), "Authenticate To See")
#'       )
#'     
#'       access_token <- ShinyGetToken(code = AuthCode())
#'       
#'       token <- access_token$access_token
#'       
#'     })
#' }
#' }
shinygaGetToken <- function(code,
                            client.id     = CLIENT_ID,
                            client.secret = CLIENT_SECRET,
                            redirect.uri  = CLIENT_URL){
  
  opts <- list(verbose = FALSE);
  raw.data <- RCurl::postForm('https://accounts.google.com/o/oauth2/token',
                              .opts = opts,
                              code = code,
                              client_id = client.id,
                              client_secret = client.secret,
                              redirect_uri = redirect.uri,
                              grant_type = 'authorization_code',
                              style = 'POST')
  
  token.data <- RJSONIO::fromJSON(raw.data);
  now <- as.numeric(Sys.time());
  token <- c(token.data, timestamp = c('first'=now, 'refresh'=now));
  
  return(token)
}

#' Shortcut all authentication functions
#' 
#' This takes all the authentication functions and renders them in a macro
#'   used for quickstart of dashboards. Trade flexibility for speed.
#' 
#' @param input The Shiny input object.
#' @param output The Shiny output object.
#' @param session The Shiny session object.
#' @param securityCode A security code generated by createCode
#' @param client.id From the Google API console
#' @param client.secret From the Google API console
#' @param client.url The URL of your application
#' @param messageData Object created from initMessageData 
#' @return Recreates all authentication functions needed for shinyServer call.
#'  Use in ui.r:
#'  output$AuthGAURL - URL users click on (uiOutput("AuthGAURL"))
#'  output$GAProfile - DataTable of GA Profie data (dataTableOutput("GAProfile"))
#'  Use in server.r:
#'  input$accounts - The accountId 
#'  input$web.prop - The webprop Id
#'  input$view - The View Id (used in GA data fetches)
#' @family authentication functions
#' @seealso \code{\link{renderAuthDropdownRow}}
#'          \code{\link{initMessageData}}
#' @examples
#' \dontrun{
#' securityCode <- createCode()
#' CLIENT_ID <- "xxxxxxxxxxxx.apps.googleusercontent.com"
#' CLIENT_SECRET <- "xxxxxxxxxxxxxxx"
#' CLIENT_URL <- 'https://mark.shinyapps.io/ga-effect/'
#' CLIENT_URL <- 'http://127.0.0.1:6423' #comment out for local testing
#' 
#' shinyServer(function(input, output, session)){
#'   
#'   messageData <- initMessageData()
#'   
#'   renderAuthenticationMacro(input,
#'                             output,
#'                             session,
#'                             securityCode,
#'                             CLIENT_ID,
#'                             CLIENT_SECRET,
#'                             CLIENT_URL,
#'                             messageData)
#'   }
#' }
renderAuthenticationMacro <- function(input,
                                      output,
                                      session,
                                      securityCode,
                                      client.id,
                                      client.secret,
                                      client.url,
                                      messageData){
  
  
  AuthCode <- reactive({
    authReturnCode(session, securityCode)
  })
  
  output$AuthGAURL <- renderUI({
    a("Click Here to Authorise Your Google Analytics Access", 
      href=shinygaGetTokenURL(securityCode,
                              client.id     = client.id,
                              client.secret = client.secret,
                              redirect.uri  = client.url))
  })
  
  AccessToken <- reactive({
    validate(
      need(AuthCode(), "Authenticate To See")
    )
    access_token <- shinygaGetToken(code = AuthCode(),
                                    client.id     = client.id,
                                    client.secret = client.secret,
                                    redirect.uri  = client.url)
    token <- access_token$access_token
  })
  
  GAProfileTable <- reactive({
    validate(
      need(AccessToken(), "Authentication working...")
    )
    
    AccountProfiles <- getAndMergeGAAccounts(AccessToken())
    
    if(!is.na(AccountProfiles$name[1])){
      addMessageData(messageData,
                     addText=paste("Authenticated"),
                     addIcon='check-square',
                     addStatus='success')
    }
    AccountProfiles
  })
  
  output$GAProfile <- renderDataTable({
    df <- GAProfileTable()[,c('name','webPropertyId','websiteUrl','profilename', 'id')]
    names(df) <- c('account', 'web property id','website url','view', 'view id')
    df
  })
  
  renderAuthDropdownRow(ga.table = GAProfileTable(),
                        input = input,
                        session = session) 
}