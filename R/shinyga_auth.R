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