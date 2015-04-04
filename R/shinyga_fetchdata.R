#' Get GA Account data
#' 
#' Gets the accounts available from the user token
#' 
#' @param token Token passed from shinygaGetToken().
#' @param start Which data point to start from.
#' @param max Maximum number to fetch.
#' @return If token exists, a dataframe of GA accounts.
#' @family fetch data functions
#' @examples
#' \dontrun{
#' Accounts <- shinygaGetAccounts(123456)
#' }
shinygaGetAccounts = function(token, start=1, max=1000) {
  url <- paste('https://www.googleapis.com/analytics/v3/management/accounts',
               '?access_token=', token,
               '&start-index=', start,
               '&max-results=', max,
               sep='', collapse='')
  
  if (length(token) > 0) {
    return(processManagementData(url, c('id', 'name', 'created', 'updated')))
  } else {
    return()
  }
}
#' Get GA Web Property data
#' 
#' Gets the web properties available from the user token
#' 
#' @param token Token passed from shinygaGetToken()
#' @param accountId AccountId of webproperty, or all of them with ~all
#' @param start which data point to start from
#' @param max Maximum number to fetch
#' @return If token exists, a dataframe of GA web properties.
#' @family fetch data functions
#' @examples
#' \dontrun{
#' WebProperties <- shinygaGetWebProperties(123456)
#' }
shinygaGetWebProperties = function(token, accountId='~all', start=1, max=1000) {
  url <- paste('https://www.googleapis.com/analytics/v3/management/accounts/', 
               accountId ,'/webproperties',
               '?access_token=', token,
               '&start-index=', start,
               '&max-results=', max,
               sep='', collapse='')
  
  if (length(token) > 0) {
    return(processManagementData(url, 
                                 c('id', 'name', 'websiteUrl', 'created', 'updated')))
  } else {
    return()
  }
}

#' Get GA View data
#' 
#' Gets the Views (Profiles) available from the user token.  Need the ViewId to fetch GA data.
#' 
#' @param token Token passed from shinygaGetToken()
#' @param accountId AccountId of webproperty, or all of them with ~all
#' @param webPropertyId webPropertyId, or all of them with ~all
#' @param start which data point to start from
#' @param max Maximum number to fetch
#' @return If token exists, a dataframe of GA profiles.
#' @family fetch data functions
#' @examples
#' \dontrun{
#' Views <- shinygaGetProfiles(123456)
#' }
shinygaGetProfiles = function(token, 
                              accountId='~all', 
                              webPropertyId='~all', 
                              start=1, 
                              max=1000) {
  url <- paste('https://www.googleapis.com/analytics/v3/management/accounts/', accountId, 
               '/webproperties/', webPropertyId , 
               '/profiles',
               '?access_token=', token,
               '&start-index=', start,
               '&max-results=', max,
               sep='', collapse='')
  
  # possible deparse.error, sapply(test$items,length)
  return(processManagementData(url, 
                               c('id', 
                                 'accountId', 
                                 'webPropertyId', 
                                 'name', 
                                 'currency', 
                                 'timezone', 
                                 'eCommerceTracking', 
                                 'websiteUrl', 
                                 'created', 
                                 'updated')))
}

#' Get GA Filters
#' 
#' Gets the Filters available for each account.
#' 
#' @param token Token passed from shinygaGetToken()
#' @param accountId AccountId of webproperty
#' @param webPropertyId webPropertyId, or all of them with ~all
#' @param profileId profileId (ViewId), or all of them with ~all
#' @param max Maximum number to fetch
#' @return If token exists, a dataframe of filters for the account.
#' @family fetch data functions
#' @examples
#' \dontrun{
#' Views <- shinygaGetGoals(123456)
#' }
shinygaGetFilters = function(token, 
                             accountId, 
                             webPropertyId = '~all', 
                             profileId = '~all', 
                             start=1, 
                             max=1000) { 
  url <- paste('https://www.googleapis.com/analytics/v3/management/accounts/', accountId, 
               '/webproperties/', webPropertyId , 
               '/profiles/', profileId, 
               '/profileFilterLinks',
               '?access_token=', token,
               '&start-index=', start,
               '&max-results=', max,
               sep='', collapse='')
  
  return(processManagementData(url, 
                               c('id',
                                 'filterRef.name',
                                 'rank',
                                 'filterRef.id',
                                 'profileRef.name',
                                 'profileRef.id',
                                 'profileRef.accountId', 
                                 'profileRef.webPropertyId', 
                                 'profileRef.internalWebPropertyId'
                                 )
                               )
         )
}

#' Get GA AdWords links
#' 
#' Gets the AdWord links available for each web property.
#' 
#' @param token Token passed from shinygaGetToken()
#' @param accountId AccountId of webproperty
#' @param webPropertyId webPropertyId
#' @param max Maximum number to fetch
#' @return If token exists, a dataframe of filters for the account.
#' @family fetch data functions
#' @examples
#' \dontrun{
#' Views <- shinygaGetGoals(123456)
#' }
shinygaGetAdWords = function(token, 
                             accountId, 
                             webPropertyId,
                             start=1, 
                             max=1000) { 
  url <- paste0('https://www.googleapis.com/analytics/v3/management/accounts/', accountId, 
               '/webproperties/', webPropertyId, 
               '/entityAdWordsLinks',
               '?access_token=', token,
               '&start-index=', start,
               '&max-results=', max)
  
  ## the adWordsAccounts is a little untidy with schema "analytics#adWordsAccount, 178-280-7367, TRUE"
  keep <- c('id',
            'entity.webPropertyRef.id',
            'entity.webPropertyRef.name',
            'entity.webPropertyRef.accountId',
            'adWordsAccounts', 
            'name',
            'profileIds'
  )
  
  aw <- processManagementData(url, 
                              keep)

  ## processing of untidy adWordsAccounts column
  awc <- cbind(aw, Reduce(rbind, aw$adWordsAccounts))
  
  return(awc)
}

#' Get GA Goals
#' 
#' Gets the Goals available from the user token.
#' 
#' @param token Token passed from shinygaGetToken()
#' @param accountId AccountId of webproperty, or all of them with ~all
#' @param webPropertyId webPropertyId, or all of them with ~all
#' @param profileId profileId (ViewId), or all of them with ~all
#' @param start which data point to start from
#' @param max Maximum number to fetch
#' @return If token exists, a dataframe of GA Goals.
#' @family fetch data functions
#' @examples
#' \dontrun{
#' Views <- shinygaGetGoals(123456)
#' }
shinygaGetGoals = function(token, 
                           accountId = '~all', 
                           webPropertyId = '~all', 
                           profileId = '~all', 
                           start=1, 
                           max=1000) { # FIX: deparse error
  url <- paste('https://www.googleapis.com/analytics/v3/management/accounts/', accountId, 
               '/webproperties/', webPropertyId , 
               '/profiles/', profileId, 
               '/goals',
               '?access_token=', token,
               '&start-index=', start,
               '&max-results=', max,
               sep='', collapse='')
  
  return(processManagementData(url, 
                               c('id', 
                                 'accountId', 
                                 'webPropertyId', 
                                 'profileId', 
                                 'name', 
                                 'value', 
                                 'active', 
                                 'type')))
}

#' Get GA Segments
#' 
#' Gets the Segments available from the user token.
#' 
#' @param token Token passed from shinygaGetToken()
#' @param start which data point to start from
#' @param max Maximum number to fetch
#' @return If token exists, a dataframe of GA segments.
#' @family fetch data functions
#' @examples
#' \dontrun{
#' Views <- shinygaGetSegments(123456)
#' }

shinygaGetSegments = function(token, start=1, max=1000) {
  url <- paste('https://www.googleapis.com/analytics/v3/management/segments',
               '?access_token=', token,
               '&start-index=', start,
               '&max-results=', max,
               sep='', collapse='')
  
  keepme <- c('id', 'segmentId', 'name', 'definition')
  pmd <- processManagementData(url, keepme)
  
  if(all(names(pmd) %in% keepme)){
    
    return(pmd)
  } else {
    warning("Error fetching segments!")
    lapply(pmd, warning)
    
    lapply()
    
    backup <- data.frame(id = c(-(1:19), -(100:105)))
    backup$segmentId <- paste0("gaid::",backup$id)
    
    backup$name <-  c("All Sessions",
                      "New Users",
                      "Returning Users",
                      "Paid Traffic",  
                      "Organic Traffic",	
                      "Search Traffic",	
                      "Direct Traffic",	
                      "Referral Traffic",	
                      "Sessions with Conversions",	
                      "Sessions with Transactions",
                      "Mobile and Tablet Traffic",
                      "Non-bounce Sessions",
                      "Tablet Traffic",
                      "Mobile Traffic",
                      "Tablet and Desktop Traffic",
                      "Android Traffic",
                      "iOS Traffic",
                      "Other Traffic (Neither iOS nor Android)",
                      "Bounced Sessions",
                      "Single Session Users",
                      "Multi-session Users",
                      "Converters",
                      "Non-Converters",
                      "Made a Purchase",
                      "Performed Site Search")
    
    backup$definition <- c('',  
                           'sessions::condition::ga:userType==New Visitor',	
                           'sessions::condition::ga:userType==Returning Visitor',	
                           'sessions::condition::ga:medium=~^(cpc|ppc|cpa|cpm|cpv|cpp)$',	
                           'sessions::condition::ga:medium==organic',	
                           'sessions::condition::ga:medium=~^(cpc|ppc|cpa|cpm|cpv|cpp|organic)$',	
                           'sessions::condition::ga:medium==(none)',	
                           'sessions::condition::ga:medium==referral',	
                           'sessions::condition::ga:goalCompletionsAll>0',	
                           'sessions::condition::ga:transactions>0',	
                           'sessions::condition::ga:deviceCategory==mobile,ga:deviceCategory==tablet',	
                           'sessions::condition::ga:bounces==0',	
                           'sessions::condition::ga:deviceCategory==tablet',	
                           'sessions::condition::ga:deviceCategory==mobile',	
                           'sessions::condition::ga:deviceCategory==tablet,ga:deviceCategory==desktop',	
                           'sessions::condition::ga:operatingSystem==Android',	
                           'sessions::condition::ga:operatingSystem=~^(iOS|iPad|iPhone|iPod)$',	
                           'sessions::condition::ga:operatingSystem!~^(Android|iOS|iPad|iPhone|iPod)$',	
                           'sessions::condition::ga:bounces>0',	
                           'users::condition::ga:sessions==1',	
                           'users::condition::ga:sessions>1',	
                           'users::condition::ga:goalCompletionsAll>0,ga:transactions>0',	
                           'users::condition::ga:goalCompletionsAll==0;ga:transactions==0',	
                           'users::condition::ga:transactions>0',	
                           'users::sequence::ga:searchKeyword!~^$|^(not set)$')
    
    
    return(backup)
  }
}

#' Take GA API output and parses it into data.frame
#' 
#' Internal function for returning a dataframe from API response
#' 
#' @param url The GA API response URL.
#' @param keep Which colums to keep.
#' @return A dataframe of GA data.
#' @family fetch data functions
#' @examples
#' \dontrun{
#' shinygaGetSegments = function(token, start=1, max=1000) {
#' url <- paste('https://www.googleapis.com/analytics/v3/management/segments',
#'              '?access_token=', token,
#'              '&start-index=', start,
#'              '&max-results=', max,
#'              sep='', collapse='')
#'              
#'              return(processManagementData(url, 
#'                     c('id', 'segmentId', 'name', 'definition', 'created', 'updated')))
#'                     }
#'}
processManagementData = function(url, keep) {
  
  ga.json <- httr::content(httr::GET(url), as = "text", type = "application/json")
  ga.json <- jsonlite::fromJSON(ga.json)
  
  if (is.null(ga.json)) { stop('data fetching did not output correct format') }
  if (!is.null(ga.json$error$message)) {stop("JSON fetch error: ",ga.json$error$message)}
  if (grepl("Error 400 (Bad Request)",ga.json[[1]])) {
    stop('JSON fetch error: Bad request URL - 400. Fetched: ', url)
  }
  
  if(is.data.frame(ga.json$items)){
    df <- jsonlite::flatten(ga.json$items)
  } else {
    stop("is.data.frame(ga.json$items was false \n ",
            ga.json$items)
  }
  

  if(all(keep %in% names(df))) {
    return(df[keep])    
  } else {
    warning("Requested columns to keep not found in return dataframe.
            \n Keep:", keep, 
            "\n Found: ", names(df), 
            "\n Returning all dataframe columns instead.")
    return(df)
  }

}

#' rollupGA - get GA data from multiple Views
#' 
#' If you have multiple GA views in GAProfileTable (from getAndMergeGAAccounts() )
#' will walk through results for all of them. 
#' @seealso
#' Refer to the dimensions and metric Google help file for more details.
#' \url{https://developers.google.com/analytics/devguides/reporting/core/dimsmets}
#' 
#' @param GAProfileTable Table of property Ids, generated from 
#' @param start.date Start date of data fetch
#' @param end.date End date of data fetch
#' @param metrics Metrics called, in GA API format (e.g. ga:visits,ga:pageviews)
#' @param dimensions Dimensions called, in GA API format (e.g. ga:date, ga:source)
#' @param sort Sort the results before returning data
#' @param filters Filters of data
#' @param segment Any segments
#' @param fields What fields from the API should be returned
#' @param start What start index of the data, used for walking through big results
#' @param max Maximum number of results to fetch
#' @param date.format Date format of results
#' @param messages Feedback messages
#' @param batch If results over 10000 limit, whether to batch them
#' @param walk If results are sampled, whether to do daily fetches to avoid sampling
#' @param output.raw Whether the results should be outputed not parsed into data.frame
#' @param output.formats Formats of data output
#' @param return.url Whether to return a URL
#' @param rbr Row by Row setting, will return NAs for empty rows
#' @param envir Which environment the data will be 
#' @param ga The token needed to fetch the data
#' @return A dataframe of GA data with the GA View specified in a column.
#' @family fetch data functions
#' @examples
#' \dontrun{
#'  chartData <- reactive({
#'      validate(
#'        need(ShinyMakeGAProfileTable(), "Need Profiles"),
#'        need(input$menuSeg != "", "Need Segment"),
#'        need(input$metric_choice != "", "Need Metric"),
#'        need(datePeriod(), "Need Dates")
#'      )
#'  
#'    data <- ShinyMakeGAProfileTable()
#'  
#'    token <- AccessToken()
#'    start <- input$range_date[1]
#'    end   <- input$range_date[2]
#'    segment_choice <- as.character(input$menuSeg)
#'    gaid <- as.character(input$view)
#'    metric_choice <- as.character(input$metric_choice)
#'    
#'    profileRow <- df[df$id == gaid,]
#'    d_dates <- as.character(datePeriod())
#'  
#'  data <- rollupGA(GAProfileTable = profileRow,
#'                   sort = d_dates,
#'                   max_results = -1,
#'                   dimensions = d_dates,
#'                   start_date = start,
#'                   start_index = 1,
#'                   metrics = paste0('ga:',metric_choice),
#'                   filters = '',
#'                   segment = segment_choice,
#'                   end_date = end,
#'                   walk = FALSE,
#'                   ga = token)
#'  
#'  data
#'  
#'  })
#'  }
rollupGA <- function(GAProfileTable,
                     sort = '',
                     max_results = -1,
                     dimensions = 'ga:date',
                     #year-month-day
                     start_date = '2013-02-01',
                     start_index = 1,
                     metrics = 'ga:visits,ga:transactionRevenue',
                     filters = '',
                     segment = '',
                     #year-month-day
                     end_date = '2013-02-01',
                     walk = FALSE,
                     ## Next line asks for the authentication token
                     ga=someGAtoken) {
  
  Results <- data.frame()
  
  ## Adding check of columns
  expected.columns <- unlist(stringr::str_split(gsub("ga:",
                                            "",
                                            paste(dimensions, metrics, sep=",")),
                                       pattern=","))
  
  i <- 0
  for (i in 1:length(GAProfileTable$id)) {
    
    print(paste("Fetching:", GAProfileTable$name[i],
                "ProfileID:", GAProfileTable$id[i],
                "Date Range:", start_date, end_date,
                metrics, dimensions,
                "sort:", sort,
                "filters:", filters,
                "segment:", segment))
    # If max is -1, then no limit on max and it will get all results
    if (max_results<0) {
      GA.data <- try(MEgetData(GAProfileTable$id[i],
                               batch = TRUE, walk = walk,
                               start.date = start_date, end.date = end_date,
                               metrics = metrics, dimensions = dimensions,
                               sort = sort, filters = filters, segment = segment,
                               start = start_index,
                               token = ga),
                     silent = TRUE)
    } else { #otherwise it will get how many indicated by max_results.
      print("Max_results:")
      print(max_results)
      GA.data <- try(MEgetData(GAProfileTable$id[i],
                               batch = TRUE, walk = walk,
                               start.date = start_date, end.date = end_date,
                               metrics = metrics, dimensions = dimensions,
                               sort = sort, filters = filters, segment = segment,
                               start = start_index, max = max_results,
                               token =ga),
                     silent = TRUE)
    }
    
    #if error, class is "try-error"
    if (class(GA.data) != "try-error"){
      
      ## Adding check of columns, but only output warning
      cat("\n### Expected Columns: ", expected.columns, "\n")
      #cat("\n### GA.data:", names(GA.data), "\n")
      #       columnChecker(expected.columns, names(GA.data), stopTF = FALSE)
      
      GA.data$name <- GAProfileTable$id[i]
      GA.data$currency <- GAProfileTable$currency[i]
      print(paste0("Fetch Successful, writing ",
                   nrow(GA.data), " rows, ",
                   ncol(GA.data), " columns, to ",
                   GA.data$name[1], ", currency: ", GA.data$currency[1]))
      Results <- rbind(Results, GA.data)
    } else {
      ErrorMessage <- paste("WARK WARK ERROR WITH GAProfileTable$id = ",
                            GAProfileTable$id[i],
                            "name:",
                            GAProfileTable$name[i], "\n ## Error:",
                            attributes(GA.data)$condition$message, 
                            GA.data)
      
      warning(ErrorMessage, immediate. = TRUE)

    }
    
  } #end for loop for each profile id
  
  return (Results)
}

#' MEgetData - does the actual fetch of GA data, called from rollupGA
#' 
#' An internal function called from rollupGA, this does the actual fetch from GA.
#' @seealso
#' Refer to the dimensions and metric Google help file for more details.
#' \url{https://developers.google.com/analytics/devguides/reporting/core/dimsmets}
#' 
#' @param ids The view ID
#' @param start.date Start date of data fetch
#' @param end.date End date of data fetch
#' @param metrics Metrics called, in GA API format (e.g. ga:visits,ga:pageviews)
#' @param dimensions Dimensions called, in GA API format (e.g. ga:date, ga:source)
#' @param sort Sort the results before returning data
#' @param filters Filters of data
#' @param segment Any segments
#' @param fields What fields from the API should be returned
#' @param start What start index of the data, used for walking through big results
#' @param max Maximum number of results to fetch
#' @param date.format Date format of results
#' @param messages Feedback messages
#' @param batch If results over 10000 limit, whether to batch them
#' @param walk If results are sampled, whether to do daily fetches to avoid sampling
#' @param output.raw Whether the results should be outputed not parsed into data.frame
#' @param output.formats Formats of data output
#' @param return.url Whether to return a URL
#' @param rbr Row by Row setting, will return NAs for empty rows
#' @param envir Which environment the data will be 
#' @param The token needed to fetch the data
#' @return A dataframe of GA data.
#' @family fetch data functions
#' @examples
#' gadata <- MEgetData(123456, token=gatoken)
MEgetData = function(ids, 
                     start.date = format(Sys.time(), "%Y-%m-%d"),
                     end.date = format(Sys.time(), "%Y-%m-%d"), 
                     metrics = 'ga:visits', dimensions = 'ga:date', 
                     sort = '', filters = '', segment = '', fields = '',
                     start = 1, max, date.format = '%Y-%m-%d', messages = TRUE, 
                     batch, walk = FALSE,
                     output.raw, output.formats, 
                     return.url = FALSE, rbr = FALSE, envir = .GlobalEnv,
                     token) {
  
#   options(RCurlOptions = list(verbose = FALSE, 
#                               capath = system.file("CurlSSL", "cacert.pem", package = "RCurl"),
#                               cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"),
#                               ssl.verifypeer = FALSE))
  
  if (missing(ids)) { stop('please enter a profile id') }
  
  if (missing(batch) || batch == FALSE) {
    isBatch <- FALSE
    if (missing(max)) {
      max <- 1000 # standard
    }
  } else {
    isBatch <- TRUE;
    if (!is.numeric(batch)) {
      if (!missing(max) && max < 10000) {
        batch <- max # no need
      } else {
        batch <- 10000 # max batch size
      }
    } else {
      if (batch > 10000) {

        stop('batch size can max be set to 10000');
      }
    }
    
    if (missing(max)) {
      adjustMax <- TRUE;
      max <- 10000; # arbitrary target, adjust later
    } else {
      adjustMax <- FALSE;
    }
  }
  
  # ensure that profile id begings with 'ga:'
  if (!as.logical(length(as.numeric(grep('ga:', ids))))) {
    ids <- paste('ga:', ids, sep = '')
  }
  
  # build url with variables
  url <- paste('https://www.googleapis.com/analytics/v3/data/ga',
               '?access_token=', token,
               '&ids=', ids,
               '&start-date=', start.date,
               '&end-date=', end.date,
               '&metrics=', metrics,
               '&dimensions=', dimensions,
               '&start-index=', start,
               '&max-results=', max,
               sep = '', collapse = '')
  
  if (sort != '') { url <- paste(url, '&sort=', sort, sep='', collapse='') }
  if (segment != '') { url <- paste(url, '&segment=', segment, sep='', collapse='') }
  if (fields != '') { url <- paste(url, '&fields=', fields, sep='', collapse='') }
  
  if (filters != '') {
    url <- paste(url, '&filters=', curlEscape(filters), sep='', collapse='');
  }  
  
  if (return.url) {
    return(url);
  }
  
  request <- httr::GET(url);
  ga.data <- httr::content(request);
  
  ###### # returns character encoded in UTF-8
  
  # possibility to extract the raw data
  if (!missing(output.raw)) {
    assign(output.raw, ga.data, envir = envir);
  }
  
  # output error and stop
  if (!is.null(ga.data$error)) {
    stop(paste('error in fetching data: ', ga.data$error$message, sep=''))
  }
  
  if (ga.data$containsSampledData == 'TRUE') {
    isSampled <- TRUE;
    if (!walk) {
      message('Notice: Data set contains sampled data');
    }
  } else {
    isSampled <- FALSE;
  }
  
  #   if (isSampled && walk) {
  #     return(.self$getDataInWalks(total = ga.data$totalResults, max = max, batch = batch,
  #                                 ids = ids, start.date = start.date, end.date = end.date,
  #                                 metrics = metrics, dimensions = dimensions, sort = sort,
  #                                 filters = filters, segment = segment, fields = fields,
  #                                 date.format = date.format, envir = envir));
  #   }
  
  #   # check if all data is being extracted
  #   if (length(ga.data$rows) < ga.data$totalResults && (messages || isBatch)) {
  #     if (!isBatch) {
  #       message(paste('Only pulling', length(ga.data$rows), 'observations of', ga.data$totalResults, 'total (set batch = TRUE to get all observations)'));
  #     } else {
  #       if (adjustMax) {
  #         max <- ga.data$totalResults;
  #       }
  #       message(paste('Pulling', max, 'observations in batches of', batch));
  #       # pass variables to batch-function
  #       return(.self$getDataInBatches(total = ga.data$totalResults, max = max, batchSize = batch,
  #                                     ids = ids, start.date = start.date, end.date = end.date,
  #                                     metrics = metrics, dimensions = dimensions, sort = sort,
  #                                     filters = filters, segment = segment, fields = fields,
  #                                     date.format = date.format, envir = envir));
  #     }
  #   }
  #   
  # get column names
  ga.headers <- as.data.frame(do.call(rbind, ga.data$columnHeaders));
  
  # did not return any results
  if (class(ga.data$rows) != 'list' && !rbr) {
    stop(paste('no results: ', ga.data$totalResults, sep = ''));
  } else if (class(ga.data$rows) != 'list' && rbr) {
    # return data.frame with NA, if row-by-row setting is true
    row <- as.data.frame(matrix(rep(NA, 
                                    length(sub('ga:', '', 
                                               as.data.frame(do.call(rbind, 
                                                                     ga.data$columnHeaders))$name))
                                    ), 
                                nrow=1))
    names(row) <- sub('ga:', 
                      '', 
                      as.data.frame(do.call(rbind, 
                                            ga.data$columnHeaders))$name)
    return(row);
  }
  
  # convert to data.frame
  ga.data.df <- as.data.frame(do.call(rbind, ga.data$rows));
  
  ga.data.df <- data.frame(lapply(ga.data.df, as.character), stringsAsFactors = F); # convert to characters
  ga.headers$name <- sub('ga:', '', ga.headers$name); # remove ga: from column headers
  
  names(ga.data.df) <- ga.headers$name; # insert column names
  
  if ('date' %in% names(ga.data.df)) {
    # mos-def optimize
    ga.data.df$'date' <- as.Date(format(as.Date(ga.data.df$'date', '%Y%m%d'), 
                                        date.format), 
                                 format=date.format);
  }
  
  # find formats
  formats <- as.data.frame(do.call(rbind, ga.data$columnHeaders))
  
  # convert to r friendly
  formats$name                                     <- sub('ga:', '', formats$name)
  formats$columnType                               <- tolower(formats$columnType)
  formats$dataType                                 <- lapply(formats$dataType, as.character)
  formats$dataType[formats$dataType == 'STRING']   <- 'character'
  formats$dataType[formats$dataType == 'INTEGER']  <- 'numeric'
  formats$dataType[formats$dataType == 'PERCENT']  <- 'numeric'
  formats$dataType[formats$dataType == 'TIME']     <- 'numeric'
  formats$dataType[formats$dataType == 'CURRENCY'] <- 'numeric'
  formats$dataType[formats$dataType == 'FLOAT']    <- 'numeric'
  formats$dataType[formats$name == 'date']         <- 'Date'
  
  # looping through columns and setting classes
  for (i in 1:nrow(formats)) {
    column <- formats$name[i]
    class <- formats$dataType[[i]]
    ga.data.df[[column]] <- as(ga.data.df[[column]], class)
  }
  
  if (!missing(output.formats)) {
    assign(output.formats, formats, envir = envir)
  }
  
  # and we're done
  return(ga.data.df);
}

#' getAndMergeGAAccounts - creates dataframe of all GA accounts/webproperties/views
#' 
#' creates dataframe of GA accounts, webprops, views that is passed to rollupGA
#' 
#' @param token Token passed from shinygaGetToken().
#' @return A dataframe of GA Account, Webproperty and View ID.
#' @family fetch data functions
#' @examples
#' Views <- getAndMergeGAAccounts(123456)
getAndMergeGAAccounts <- function(token){
  #get all the management API stuff
  Accounts      <- shinygaGetAccounts(token)
  Profiles      <- shinygaGetProfiles(token)
  WebProperties <- shinygaGetWebProperties(token)
  
  #Merge em
  names(Accounts)    <- c("accountId","name","created","updated")
  names(Profiles)[4] <- "profilename"
  
  AccountProfiles <- merge(Accounts,Profiles,by="accountId")
  
  AccountProfiles$name          <- as.character(AccountProfiles$name)
  AccountProfiles$webPropertyId <- as.character(AccountProfiles$webPropertyId)
  AccountProfiles$websiteUrl    <- as.character(AccountProfiles$websiteUrl)
  AccountProfiles$profilename   <- as.character(AccountProfiles$profilename)
  AccountProfiles$id            <- as.character(AccountProfiles$id)

  ## remove NA ids as they may pass to rollupGA
  AccountProfiles <- AccountProfiles[!is.na(AccountProfiles$id),]
  
  AccountProfiles
  
}