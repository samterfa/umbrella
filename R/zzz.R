unlistItems <- function(x){
  
  if(class(x) == 'list'){
    
    x %>% purrr::map(~ifelse(length(.x) == 0, NA, .x))
    
  }else{
    
    x %>% dplyr::mutate_at(.vars = which(unlist(lapply(x, is.list))), .funs = ~unlist(lapply(.x, function(y) ifelse(length(y) == 0, NA, y))))
  }
}

formatUmbrellaDatetime <- function(datetime){
  
  if(!stringr::str_detect(datetime, 'day') & !stringr::str_detect(datetime, 'now')){
    
    datetime <- 1000 * (datetime %>% as_datetime(tz = 'America/Chicago') %>% as.numeric())
  }
  
  format(datetime, scientific = F)
}

getAuthHeader <- function(apiType, authType, refresh = F){
  
  apikeyvar <- glue::glue('umbrella{apiType}Key')
  apikey <- Sys.getenv(apikeyvar)
  apisecretvar <- glue::glue('umbrella{apiType}Secret')
  apisecret <- Sys.getenv(apisecretvar)
  
  if(Sys.getenv('umbrellaOrganizationId') == '') stop('You must set your OrganizationId using Sys.setenv(umbrellaOrganizationId = {YourOrganizationId}).')
  if(apikey == '' | apisecret == '') stop(glue::glue('You must set the relevant API key and secret using Sys.setenv({apikeyvar} = {{YourAPIkey}}, {apisecretvar} = {{YourAPIsecret}}).'))
  
  if(authType == 'basic'){
    
    auth <- glue::glue('Basic {base64enc::base64encode(charToRaw(paste0(apikey, ":", apisecret)))}')
    
    return(c(Authorization = auth))
  }
  
  if(authType == 'oauth2'){
    
    options("httr_oauth_cache" = NA)
    
    endpoint <- httr::oauth_endpoint(authorize = NULL, access = 'https://management.api.umbrella.com/auth/v2/oauth2/token')
    app <- httr::oauth_app(appname = 'umbrella', key = apikey, secret = apisecret)
    
    tokenDir <- '.umbrellaTokenCache'
    tokenFile <- glue::glue('umbrella{apiType}Token')
    
    if(!dir.exists(tokenDir)) dir.create(tokenDir)
    
    tokenCacheFile <- glue::glue('{tokenDir}/{tokenFile}')
    
    if(refresh) file.remove(tokenCacheFile)
    
    auth <- httr::oauth2.0_token(endpoint = endpoint, app = app, use_basic_auth = T, client_credentials = T, cache = tokenCacheFile)
    
    return(c(Authorization = glue::glue('Bearer {auth$credentials$access_token}')))
  }
  stop('No matching authType')
}

downloadUpdatedAPIspecs <- function(){
  
  if(!dir.exists('data')) dir.create('data')
  
  require(dplyr)
  
  doc <- xml2::read_html('https://docs.umbrella.com/umbrella-api/reference')
  
  scripts <- doc %>% rvest::html_nodes('script') %>% purrr::keep(~!is.na(.x %>% rvest::html_attr('id')))
  
  specUrls <- scripts %>% purrr::keep(~(.x %>% rvest::html_attr('id') == 'readme-data-oasUrls')) %>% purrr::map(~.x %>% rvest::html_attr('data-json')) %>% unlist(recursive = F) %>% jsonlite::fromJSON()
  
  for(url in specUrls){
    
    spec <- jsonlite::read_json(url, auto_unbox = T)
    
    write_json(spec, glue::glue('data/{spec$info$title %>% stringr::str_replace_all(" ", "")}.json'), auto_unbox = T)
  }
}

generateReportingAPIfunctions <- function(){
  
  stop('Already generated!')
  
  require(dplyr)
  require(pluralize)
  
  if(!dir.exists('R')) dir.create('R')
  
  api <- 'ReportingAPI'
  
  if(file.exists(glue::glue('R/{api}.R'))) file.remove(glue::glue('R/{api}.R'))
 
  if(!file.exists(glue::glue('data/{api}.json'))) downloadUpdatedAPIspecs()
  
  spec <- jsonlite::read_json(glue::glue('data/{api}.json'), auto_unbox = T)
  
  authType <- spec$components$securitySchemes %>% names() %>% stringr::str_to_lower()
  if(length(authType) == 0){
    authType <- 'oauth2'
  }else{
    authType <- 'basic'
  }
  
  baseUrl <- spec$servers %>% unlist(recursive = F) %>% purrr::pluck('url')
  
  # Track all functions and function categories created and enforce uniqueness.
  allFunctions <- character()
  
  for(path in (spec$paths %>% names())){
    
    pathInfo <- spec %>% purrr::pluck('paths') %>% purrr::pluck(path)
    
    path <- path %>% stringr::str_replace('organizationid', 'organizationId')
    
    for(method in (pathInfo %>% names())){
      
      methodInfo <- pathInfo %>% purrr::pluck(method)
      
      functionSummary <- methodInfo$summary
      
      functionCategory <- api
      
      functionDescription <- ifelse(is.null(methodInfo$description), methodInfo$summary, methodInfo$description %>% stringr::str_replace_all(stringr::fixed('\n'), ''))
      
      slashPositions <- stringr::str_locate_all(path, stringr::fixed('/')) %>% as.data.frame() %>% pull(start)
      
      if(method == 'get'){
        if(path %>% stringr::str_sub(-1) == stringr::fixed('}')){
          
          verb <- 'get'
          object <- path %>% stringr::str_sub(slashPositions %>% tail(2) %>% first() + 1, slashPositions %>% tail(2) %>% last() - 1) %>% pluralize::singularize() %>% stringr::str_to_title() %>% stringr::str_replace_all('-', '')
          
        }else{
          
          verb <- 'list'
          object <- path %>% stringr::str_sub(slashPositions %>% tail(1) + 1) %>% stringr::str_to_title() %>% stringr::str_replace_all('-', '') %>% pluralize::pluralize()
          if(object %>% stringr::str_detect('By')) object <- object %>% pluralize::singularize()
          prevObject <- path %>% stringr::str_sub(slashPositions %>% tail(3) %>% first() + 1, slashPositions %>% tail(2) %>% first() - 1) %>% pluralize::singularize() %>% stringr::str_to_title() %>% stringr::str_replace_all('-', '')
        }
      }
      
      # Skips one unnecessary repeat function (getIdentities)
      if(method == 'post'){
        
        next()
      }
      
      if(object %>% stringr::str_detect('list$') | object %>% stringr::str_detect('lists$')) object <- object %>% stringr::str_replace('list', 'List')
      if(object %>% stringr::str_detect('device$') | object %>% stringr::str_detect('devices$')) object <- object %>% stringr::str_replace('device', 'Device')
      
      functionName <- paste0(verb, object)
      
      if(functionName %in% allFunctions){
        functionName <- paste0(verb, prevObject %>% pluralize::singularize(), object %>% pluralize::pluralize())
        if(functionName %>% stringr::str_detect('By')) functionName <- functionName <- paste0(verb, prevObject %>% pluralize::singularize(), object %>% pluralize::singularize())
      }
      
      allFunctions <- append(allFunctions, functionName)
      
      functionParametersNames <- methodInfo$parameters %>% unlist() %>% unname() %>% stringr::str_replace(stringr::fixed('#/components/parameters/'), '')
      
      functionParameters <- spec$components$parameters %>% purrr::keep(names(.) %in% functionParametersNames) %>% unname()
      
      # These parameters are substituted as part of the endpoint URI and are thus inherently required.
      pathParameters <- functionParameters %>% purrr::keep(function(x) x$`in` == 'path')
      pathParameters <- pathParameters %>% lapply(function(x) {if(x$name == 'organizationid') x <- purrr::list_modify(x, name = 'organizationId'); x})
      
      # These parameters are passed into the query following the endpoint URI and may be optional.
      queryParameters <- functionParameters %>% purrr::keep(function(x) x$`in` == 'query')
      
      # These parameters are passed into the body of a post or patch request.
      bodyParameters <- NULL
        
      # Begin documentation text according to R rOxygen specifications
      documentationText <- glue::glue("\n\n#' {functionSummary}", .trim = F)
      documentationText <- paste0(documentationText, "\n#' ")
      documentationText <- paste0(documentationText, glue::glue("\n#' @description {functionDescription}", .trim = F))
      documentationText <- paste0(documentationText, "\n#' ")
      
      # Begin function text
      functionText <- glue::glue("\n\t{functionName} <- function(", .trim = F)
      
      # Loop through path parameters and add function text.
      for(paramInfo in pathParameters){
        
        required <- F
        if('required' %in% names(paramInfo)){
          if(paramInfo$required %>% as.logical()) required <- T
        }
        
        documentationText <- paste0(documentationText, glue::glue("\n#' @param {paramInfo$name} {paramInfo$description}", .trim = F))
        
        orgId <- paramInfo$name == "organizationId"
        orgIdDefault <- ' = Sys.getenv("umbrellaOrganizationId")'
        
        functionText <- paste0(functionText, glue::glue('{paramInfo$name}{ifelse(required, "", " = NULL")}{ifelse(orgId, orgIdDefault, "")}, ', .trim = F))
      }
      
      # Loop through query parameters and add to function text.
      for(paramInfo in queryParameters){
        
        required <- F
        if('required' %in% names(paramInfo)){
          if(paramInfo$required %>% as.logical()) required <- T
        }
        
        documentationText <- paste0(documentationText, glue::glue("\n#' @param {paramInfo$name} {paramInfo$description}", .trim = F))
        
        functionText <- paste0(functionText, glue::glue('{paramInfo$name}{ifelse(required, "", " = NULL")}, ', .trim = F))
      }
      
      # Finish off function text opening line starting the function's definition.
     
      # Move Limit and Offset to end.
      if(functionText %>% stringr::str_detect("offset")){
        functionText <- functionText %>% stringr::str_replace('offset = NULL, ', '')
        functionText <- paste0(functionText, 'offset = 0, ')
      }
      
      # Move Limit and Offset to end.
      if(functionText %>% stringr::str_detect("limit")){
        functionText <- functionText %>% stringr::str_replace('limit, ', '')
        functionText <- paste0(functionText, 'limit = 5000, ')
      }
      
      # Move organizationId = Sys.getenv("umbrellaOrganizationId") to end of function params.
      if(functionText %>% stringr::str_detect("umbrellaOrganizationId")){
        functionText <- functionText %>% stringr::str_replace(stringr::fixed('organizationId = Sys.getenv("umbrellaOrganizationId"), '), '')
        functionText <- paste0(functionText, 'organizationId = Sys.getenv("umbrellaOrganizationId")')
      }
      
      functionText <- paste0(functionText, '){') %>% stringr::str_replace(stringr::fixed(', )'), ')')
      
      functionText <- paste0(functionText, '\n\n\t\tparams <- as.list(environment()) %>% purrr::compact()')
      
      # Grab query parameter names.
      if(length(queryParameters) > 0){
        functionText <- paste0(functionText, glue::glue('\n\n\t\tqueryParamNames <- "{queryParameters %>% purrr::map(function(x) x$name) %>% unlist() %>% paste(collapse = ", ")}"', .trim = F))
        functionText <- paste0(functionText, '\n\t\tqueryParams <- params %>% purrr::keep(names(params) %in% (queryParamNames %>% stringr::str_split(", ") %>% purrr::pluck(1)))')
      }else{
        functionText <- paste0(functionText, '\n\n\t\tqueryParams <- NULL')
      }
      
      functionText <- paste0(functionText, glue::glue('\n\n\t\tapi <- "{api}"', .trim = F))
      functionText <- paste0(functionText, glue::glue('\n\n\t\tauthType <- "{authType}"', .trim = F))
      
      functionText <- paste0(functionText, glue::glue('\n\n\t\tresponse <- httr::{method %>% stringr::str_to_upper()}(glue::glue("{baseUrl}{path}"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))', .trim = F))
      
      functionText <- paste0(functionText, '\n\n\t\tif(response$status_code == 401){')
      
      functionText <- paste0(functionText, glue::glue('\n\t\t\tresponse <- httr::{method %>% stringr::str_to_upper()}(glue::glue("{baseUrl}{path}"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))', .trim = F))
      
      functionText <- paste0(functionText, '\n\t\t}')
      
      functionText <- paste0(functionText, '\n\n\t\tif(response$status_code > 300) return(response)')
      
      functionText <- paste0(functionText, '\n\n\t\tif(httr::content(response) %>% class() %>% stringr::str_detect("xml") %>% any()){')
      
      functionText <- paste0(functionText, '\n\t\t\treturn(httr::content(response) %>% rvest::html_text() %>% jsonlite::fromJSON(simplifyVector = F, simplifyDataFrame = F, simplifyMatrix = F, flatten = F))')
      
      functionText <- paste0(functionText, '\n\t\t}')
      
      functionText <- paste0(functionText, '\n\n\t\thttr::content(response)')
      
      # End Function definition
      functionText <- paste0(functionText, '\n\t}')
      
      # End Documentation definition
      documentationText <- paste0(documentationText, glue::glue("\n#' @concept {functionCategory %>% stringr::str_replace_all(' ', '')}", .trim = F))
      documentationText <- paste0(documentationText, "\n#' @export")
      
      # Update Script text with Documentation text and Function text.
      scriptText <- paste0(documentationText, functionText)
      
      # Write scripts to file.
      readr::write_lines(x = scriptText, path = glue::glue('R/{functionCategory}.R'), append = T)
      
      Sys.sleep(.2)
    }
  }
}

generateDestinationAPIGatewayFunctions <- function(){
  
  stop("Already generated! You'll need to hand edit after this.")
  
  require(dplyr)
  require(pluralize)
  
  if(!dir.exists('R')) dir.create('R')
  
  api <- 'DestinationAPIGateway'
  
  if(file.exists(glue::glue('R/{api}.R'))) file.remove(glue::glue('R/{api}.R'))
  
  if(!file.exists(glue::glue('data/{api}.json'))) downloadUpdatedAPIspecs()
  
  spec <- jsonlite::read_json(glue::glue('data/{api}.json'), auto_unbox = T)
  
  authType <- spec$components$securitySchemes %>% names() %>% stringr::str_to_lower()
  if(length(authType) == 0){
    authType <- 'oauth2'
  }else{
    authType <- 'basic'
  }
  
  baseUrl <- spec$servers %>% unlist(recursive = F) %>% purrr::pluck('url')
  
  # Track all functions and function categories created and enforce uniqueness.
  allFunctions <- character()
  
  for(path in (spec$paths %>% names())){
    
    pathInfo <- spec %>% purrr::pluck('paths') %>% purrr::pluck(path)
    
    for(method in (pathInfo %>% names())){
      
      methodInfo <- pathInfo %>% purrr::pluck(method)
      
      functionSummary <- methodInfo$summary
      
      functionCategory <- api
      
      functionDescription <- ifelse(is.null(methodInfo$description), methodInfo$summary, methodInfo$description %>% stringr::str_replace_all(stringr::fixed('\n'), ''))
      
      slashPositions <- stringr::str_locate_all(path, stringr::fixed('/')) %>% as.data.frame() %>% pull(start)
      
      if(method == 'get'){
        if(path %>% stringr::str_sub(-1) == stringr::fixed('}')){
          
          verb <- 'get'
          object <- path %>% stringr::str_sub(slashPositions %>% tail(2) %>% first() + 1, slashPositions %>% tail(2) %>% last() - 1) %>% pluralize::singularize() %>% stringr::str_to_title() %>% stringr::str_replace_all('-', '')
          
        }else{
          
          verb <- 'list'
          object <- path %>% stringr::str_sub(slashPositions %>% tail(1) + 1) %>% stringr::str_to_title() %>% stringr::str_replace_all('-', '') %>% pluralize::pluralize()
          if(object %>% stringr::str_detect('By')) object <- object %>% pluralize::singularize()
          prevObject <- path %>% stringr::str_sub(slashPositions %>% tail(3) %>% first() + 1, slashPositions %>% tail(2) %>% first() - 1) %>% pluralize::singularize() %>% stringr::str_to_title() %>% stringr::str_replace_all('-', '')
        }
      }
      
      if(method %in% c('put', 'patch')){
        
        verb <- 'modify'
        object <- path %>% stringr::str_sub(slashPositions %>% tail(2) %>% first() + 1, slashPositions %>% tail(2) %>% last() - 1) %>% pluralize::singularize() %>% stringr::str_to_title() %>% stringr::str_replace_all('-', '')
        
      }
      
      if(method == 'post'){
        
        verb <- 'create'
        object <- path %>% stringr::str_sub(slashPositions %>% tail(1) + 1) %>% pluralize::singularize() %>% stringr::str_to_title() %>% stringr::str_replace_all('-', '')
        
      }
      
      if(method == 'delete'){
        
        verb <- 'delete'
        object <- path %>% stringr::str_sub(slashPositions %>% tail(2) %>% first() + 1, slashPositions %>% tail(2) %>% last() - 1) %>% pluralize::singularize() %>% stringr::str_to_title() %>% stringr::str_replace_all('-', '')
        
      }
      
      if(object %>% stringr::str_detect('list$') | object %>% stringr::str_detect('lists$')) object <- object %>% stringr::str_replace('list', 'List')
      if(object %>% stringr::str_detect('device$') | object %>% stringr::str_detect('devices$')) object <- object %>% stringr::str_replace('device', 'Device')
      
      functionName <- paste0(verb, object)
      
      if(functionName %in% allFunctions){
        functionName <- paste0(verb, prevObject %>% pluralize::singularize(), object %>% pluralize::pluralize())
        if(functionName %>% stringr::str_detect('By')) functionName <- functionName <- paste0(verb, prevObject %>% pluralize::singularize(), object %>% pluralize::singularize())
      }
      
      allFunctions <- append(allFunctions, functionName)
        
      functionParameters <- methodInfo$parameters %>% purrr::keep(~ 'in' %in% names(.x))
       
      # These parameters are substituted as part of the endpoint URI and are thus inherently required.
      pathParameters <- functionParameters %>% purrr::keep(function(x) x$`in` == 'path')
      
      # These parameters are passed into the query following the endpoint URI and may be optional.
      queryParameters <- functionParameters %>% purrr::keep(function(x) x$`in` == 'query')
      
      # These parameters are passed into the body of a post or patch request.
      bodyParameters <- NULL
      
      if('requestBody' %in% (methodInfo %>% names())){
        
        if('$ref' %in% names(methodInfo$requestBody$content$`application/json`$schema)){
          schemaObject <- methodInfo$requestBody$content$`application/json`$schema$`$ref` %>% stringr::str_replace('#/components/schemas/', '')
        }else{
          schemaObject <- methodInfo$requestBody$content$`application/json`$schema$items$`$ref` %>% stringr::str_replace('#/components/schemas/', '')
        }
        
        bodyParameters <- list(description = methodInfo$requestBody$description, required = methodInfo$requestBody$required, name = schemaObject, properties = spec$components$schemas %>% purrr::pluck(schemaObject) %>% purrr::pluck('properties')) %>% list()
      }
      
      # Begin documentation text according to R rOxygen specifications
      documentationText <- glue::glue("\n\n#' {functionSummary}", .trim = F)
      documentationText <- paste0(documentationText, "\n#' ")
      documentationText <- paste0(documentationText, glue::glue("\n#' @description {functionDescription}", .trim = F))
      documentationText <- paste0(documentationText, "\n#' ")
      
      # Begin function text
      functionText <- glue::glue("\n\t{functionName} <- function(", .trim = F)
      
      # Loop through path parameters and add function text.
      for(paramInfo in pathParameters){
        
        required <- F
        if('required' %in% names(paramInfo)){
          if(paramInfo$required %>% as.logical()) required <- T
        }
        
        documentationText <- paste0(documentationText, glue::glue("\n#' @param {paramInfo$name} {paramInfo$description}", .trim = F))
        
        orgId <- paramInfo$name == "organizationId"
        orgIdDefault <- ' = Sys.getenv("umbrellaOrganizationId")'
        
        functionText <- paste0(functionText, glue::glue('{paramInfo$name}{ifelse(required, "", " = NULL")}{ifelse(orgId, orgIdDefault, "")}, ', .trim = F))
      }
      
      # Loop through query parameters and add to function text.
      for(paramInfo in queryParameters){
        
        required <- F
        if('required' %in% names(paramInfo)){
          if(paramInfo$required %>% as.logical()) required <- T
        }
        
        documentationText <- paste0(documentationText, glue::glue("\n#' @param {paramInfo$name} {paramInfo$description}", .trim = F))
        
        functionText <- paste0(functionText, glue::glue('{paramInfo$name}{ifelse(required, "", " = NULL")}, ', .trim = F))
      }
      
      # Loop through body parameters and add to function text.
      for(param in bodyParameters){
        
        documentationText <- paste0(documentationText, glue::glue("\n#' @param {param$name} {param$description}", .trim = F))
        
        functionText <- paste0(functionText, glue::glue('body = {param$name}(), ', .trim = F))
      }
      
      # Finish off function text opening line starting the function's definition.
      # Move organizationId = Sys.getenv("umbrellaOrganizationId") to end of function params.
      
      if(functionText %>% stringr::str_detect("umbrellaOrganizationId")){
        functionText <- functionText %>% stringr::str_replace(stringr::fixed('organizationId = Sys.getenv("umbrellaOrganizationId"), '), '')
        functionText <- paste0(functionText, 'organizationId = Sys.getenv("umbrellaOrganizationId")')
      }
      
      functionText <- paste0(functionText, '){') %>% stringr::str_replace(stringr::fixed(', )'), ')')
      
      functionText <- paste0(functionText, '\n\n\t\tparams <- as.list(environment()) %>% purrr::compact()')
      
      # Grab query parameter names.
      if(length(queryParameters) > 0){
        functionText <- paste0(functionText, glue::glue('\n\n\t\tqueryParamNames <- "{queryParameters %>% purrr::map(function(x) x$name) %>% unlist() %>% paste(collapse = ", ")}"', .trim = F))
        functionText <- paste0(functionText, '\n\t\tqueryParams <- params %>% purrr::keep(names(params) %in% (queryParamNames %>% stringr::str_split(", ") %>% purrr::pluck(1)))')
      }else{
        functionText <- paste0(functionText, '\n\n\t\tqueryParams <- NULL')
      }
      
      functionText <- paste0(functionText, glue::glue('\n\n\t\tapi <- "{api}"', .trim = F))
      functionText <- paste0(functionText, glue::glue('\n\n\t\tauthType <- "{authType}"', .trim = F))
      
      functionText <- paste0(functionText, glue::glue('\n\n\t\tresponse <- httr::{method %>% stringr::str_to_upper()}(glue::glue("{baseUrl}{path}"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))', .trim = F))
      
      functionText <- paste0(functionText, '\n\n\t\tif(response$status_code == 401){')
      
      functionText <- paste0(functionText, glue::glue('\n\n\t\t\tresponse <- httr::{method %>% stringr::str_to_upper()}(glue::glue("{baseUrl}{path}"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))', .trim = F))
      
      functionText <- paste0(functionText, '\n\t\t}')
      
      functionText <- paste0(functionText, '\n\n\t\tif(response$status_code > 300) return(response)')
      
      functionText <- paste0(functionText, '\n\n\t\thttr::content(response)')
      
      # End Function definition
      functionText <- paste0(functionText, '\n\t}')
      
      # End Documentation definition
      documentationText <- paste0(documentationText, glue::glue("\n#' @concept {functionCategory %>% stringr::str_replace_all(' ', '')}", .trim = F))
      documentationText <- paste0(documentationText, "\n#' @export")
      
      # Update Script text with Documentation text and Function text.
      scriptText <- paste0(documentationText, functionText)
      
      # Write scripts to file.
      readr::write_lines(x = scriptText, path = glue::glue('R/{functionCategory}.R'), append = T)
      
      Sys.sleep(.2)
    }
  }
}


generateManagementAPIFunctions <- function(){
  
  require(dplyr)
  require(pluralize)
  
  if(!dir.exists('R')) dir.create('R')
  
  api <- 'ManagementAPI'
  
  if(file.exists(glue::glue('R/{api}.R'))) file.remove(glue::glue('R/{api}.R'))
  
  if(!file.exists(glue::glue('data/{api}.json'))) downloadUpdatedAPIspecs()
  
  spec <- jsonlite::read_json(glue::glue('data/{api}.json'), auto_unbox = T)
  
  authType <- spec$components$securitySchemes %>% names() %>% stringr::str_to_lower()
  if(length(authType) == 0){
    authType <- 'oauth2'
  }else{
    authType <- 'basic'
  }
  
  baseUrl <- spec$servers %>% unlist(recursive = F) %>% purrr::pluck('url')
  
  # Track all functions and function categories created and enforce uniqueness.
  allFunctions <- character()
  
  for(path in (spec$paths %>% names())){
    
    pathInfo <- spec %>% purrr::pluck('paths') %>% purrr::pluck(path)
    
    for(method in (pathInfo %>% names())){
      
      methodInfo <- pathInfo %>% purrr::pluck(method)
      
      functionSummary <- methodInfo$description
      
      functionDescription <- methodInfo$description
      
      functionCategory <- api
      
      slashPositions <- stringr::str_locate_all(path, stringr::fixed('/')) %>% as.data.frame() %>% pull(start)
      
      if(method == 'get'){
        if(path %>% stringr::str_sub(-1) == stringr::fixed('}')){
          
          verb <- 'get'
          object <- path %>% stringr::str_sub(slashPositions %>% tail(2) %>% first() + 1, slashPositions %>% tail(2) %>% last() - 1) %>% pluralize::singularize() %>% stringr::str_to_title() %>% stringr::str_replace_all('-', '')
          
        }else{
          
          verb <- 'list'
          object <- path %>% stringr::str_sub(slashPositions %>% tail(1) + 1) %>% stringr::str_to_title() %>% stringr::str_replace_all('-', '') %>% pluralize::pluralize()
          if(object %>% stringr::str_detect('By')) object <- object %>% pluralize::singularize()
          prevObject <- path %>% stringr::str_sub(slashPositions %>% tail(3) %>% first() + 1, slashPositions %>% tail(2) %>% first() - 1) %>% pluralize::singularize() %>% stringr::str_to_title() %>% stringr::str_replace_all('-', '')
        }
      }
      
      if(method %in% c('put', 'patch')){
        
        verb <- 'modify'
        object <- path %>% stringr::str_sub(slashPositions %>% tail(2) %>% first() + 1, slashPositions %>% tail(2) %>% last() - 1) %>% pluralize::singularize() %>% stringr::str_to_title() %>% stringr::str_replace_all('-', '')
        
      }
      
      if(method == 'post'){
        
        verb <- 'create'
        object <- path %>% stringr::str_sub(slashPositions %>% tail(1) + 1) %>% pluralize::singularize() %>% stringr::str_to_title() %>% stringr::str_replace_all('-', '')
        
      }
      
      if(method == 'delete'){
        
        verb <- 'delete'
        object <- path %>% stringr::str_sub(slashPositions %>% tail(2) %>% first() + 1, slashPositions %>% tail(2) %>% last() - 1) %>% pluralize::singularize() %>% stringr::str_to_title() %>% stringr::str_replace_all('-', '')
        
      }
      
      if(object %>% stringr::str_detect('list$') | object %>% stringr::str_detect('lists$')) object <- object %>% stringr::str_replace('list', 'List')
      if(object %>% stringr::str_detect('device$') | object %>% stringr::str_detect('devices$')) object <- object %>% stringr::str_replace('device', 'Device')
      
      functionName <- paste0(verb, object)
      
      if(functionName %in% allFunctions){
        functionName <- paste0(verb, prevObject %>% pluralize::singularize(), object %>% pluralize::pluralize())
        if(functionName %>% stringr::str_detect('By')) functionName <- functionName <- paste0(verb, prevObject %>% pluralize::singularize(), object %>% pluralize::singularize())
      }
      
      allFunctions <- append(allFunctions, functionName)
      
      functionParameters <- methodInfo$parameters %>% purrr::keep(~ 'in' %in% names(.x))
      
      # These parameters are substituted as part of the endpoint URI and are thus inherently required.
      pathParameters <- functionParameters %>% purrr::keep(function(x) x$`in` == 'path')
      
      # These parameters are passed into the query following the endpoint URI and may be optional.
      queryParameters <- functionParameters %>% purrr::keep(function(x) x$`in` == 'query')
      
      # These parameters are passed into the body of a post or patch request.
      bodyParameters <- NULL
      
      if('requestBody' %in% (methodInfo %>% names())){
        paramsNames <- methodInfo$requestBody$content$`application/json`$schema$properties %>% names()
        bodyParameters <- data.frame(description = methodInfo$requestBody$content$`application/json`$schema$properties %>% purrr::map(~.x %>% purrr::pluck('description') %>% stringr::str_replace_all(stringr::fixed('\n'), ' ')) %>% unname() %>% unlist(), 
                                     required = paramsNames %in% (methodInfo$requestBody$content$`application/json`$schema$required),
                                     name = paramsNames) %>% jsonlite::toJSON(auto_unbox = T) %>% jsonlite::fromJSON(F)
      }
      
      # Begin documentation text according to R rOxygen specifications
      documentationText <- glue::glue("\n\n#' {functionSummary}", .trim = F)
      documentationText <- paste0(documentationText, "\n#' ")
      documentationText <- paste0(documentationText, glue::glue("\n#' @description {functionDescription}", .trim = F))
      documentationText <- paste0(documentationText, "\n#' ")
      
      # Begin function text
      functionText <- glue::glue("\n\t{functionName} <- function(", .trim = F)
      
      # Loop through path parameters and add function text.
      for(paramInfo in pathParameters){
        
        required <- F
        if('required' %in% names(paramInfo)){
          if(paramInfo$required %>% as.logical()) required <- T
        }
        
        documentationText <- paste0(documentationText, glue::glue("\n#' @param {paramInfo$name} {paramInfo$description}", .trim = F))
        
        orgId <- paramInfo$name == "organizationId"
        orgIdDefault <- ' = Sys.getenv("umbrellaOrganizationId")'
        
        functionText <- paste0(functionText, glue::glue('{paramInfo$name}{ifelse(required, "", " = NULL")}{ifelse(orgId, orgIdDefault, "")}, ', .trim = F))
      }
      
      # Loop through query parameters and add to function text.
      for(paramInfo in queryParameters){
        
        required <- F
        if('required' %in% names(paramInfo)){
          if(paramInfo$required %>% as.logical()) required <- T
        }
        
        documentationText <- paste0(documentationText, glue::glue("\n#' @param {paramInfo$name} {ifelse(is.null(paramInfo$description), '', paramInfo$description)}", .trim = F))
        
        functionText <- paste0(functionText, glue::glue('{paramInfo$name}{ifelse(required, "", " = NULL")}, ', .trim = F))
      }
      
      # Loop through body parameters and add to function text.
      for(paramInfo in bodyParameters){
        
        required <- paramInfo$required
        
        documentationText <- paste0(documentationText, glue::glue("\n#' @param {paramInfo$name} {paramInfo$description}", .trim = F))
        
        functionText <- paste0(functionText, glue::glue('{paramInfo$name}{ifelse(required, "", " = NULL")}, ', .trim = F))
      }
      
      # Finish off function text opening line starting the function's definition.
      # Move organizationId = Sys.getenv("umbrellaOrganizationId") to end of function params.
      
      if(functionText %>% stringr::str_detect("umbrellaOrganizationId")){
        functionText <- functionText %>% stringr::str_replace(stringr::fixed('organizationId = Sys.getenv("umbrellaOrganizationId"), '), '')
        functionText <- paste0(functionText, 'organizationId = Sys.getenv("umbrellaOrganizationId")')
      }
      
      functionText <- paste0(functionText, '){') %>% stringr::str_replace(stringr::fixed(', )'), ')')
      
      functionText <- paste0(functionText, '\n\n\t\tparams <- as.list(environment()) %>% purrr::compact()')
      
      # Grab query parameter names.
      if(length(queryParameters) > 0){
        functionText <- paste0(functionText, glue::glue('\n\n\t\tqueryParamNames <- "{queryParameters %>% purrr::map(function(x) x$name) %>% unlist() %>% paste(collapse = ", ")}"', .trim = F))
        functionText <- paste0(functionText, '\n\t\tqueryParams <- params %>% purrr::keep(names(params) %in% (queryParamNames %>% stringr::str_split(", ") %>% purrr::pluck(1)))')
      }else{
        functionText <- paste0(functionText, '\n\n\t\tqueryParams <- NULL')
      }
      
      # Grab body parameter names.
      if(length(bodyParameters) > 0){
        functionText <- paste0(functionText, glue::glue('\n\n\t\tbodyParamNames <- "{bodyParameters %>% purrr::map(~.x$name) %>% paste(collapse = ", ")}"', .trim = F))
        functionText <- paste0(functionText, '\n\t\tbodyParams <- params %>% purrr::keep(names(params) %in% (bodyParamNames %>% stringr::str_split(", ") %>% purrr::pluck(1)))')
      }else{
        functionText <- paste0(functionText, '\n\n\t\tbodyParams <- NULL')
      }
      
      functionText <- paste0(functionText, glue::glue('\n\n\t\tapi <- "{api}"', .trim = F))
      functionText <- paste0(functionText, glue::glue('\n\n\t\tauthType <- "{authType}"', .trim = F))
      
      functionText <- paste0(functionText, glue::glue('\n\n\t\tresponse <- httr::{method %>% stringr::str_to_upper()}(glue::glue("{baseUrl}{path}"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))', .trim = F))
      
      functionText <- paste0(functionText, '\n\n\t\tif(response$status_code == 401){')
      
      functionText <- paste0(functionText, glue::glue('\n\n\t\t\tresponse <- httr::{method %>% stringr::str_to_upper()}(glue::glue("{baseUrl}{path}"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))', .trim = F))
      
      functionText <- paste0(functionText, '\n\t\t}')
      
      functionText <- paste0(functionText, '\n\n\t\tif(response$status_code > 300) return(response)')
      
      functionText <- paste0(functionText, '\n\n\t\thttr::content(response)')
      
      # End Function definition
      functionText <- paste0(functionText, '\n\t}')
      functionText <- functionText %>% stringr::str_replace_all('limit = NULL', 'limit = 100')
      functionText <- functionText %>% stringr::str_replace_all('page = NULL', 'page = 1')
      
      # End Documentation definition
      documentationText <- paste0(documentationText, glue::glue("\n#' @concept {functionCategory %>% stringr::str_replace_all(' ', '')}", .trim = F))
      documentationText <- paste0(documentationText, "\n#' @export")
      documentationText <- documentationText %>% stringr::str_replace_all(stringr::fixed("@param page "), "@param page Page number of results.")
      documentationText <- documentationText %>% stringr::str_replace_all(stringr::fixed("@param limit "), "@param limit Number of results to return for the given page number.")
      
      # Update Script text with Documentation text and Function text.
      scriptText <- paste0(documentationText, functionText)
      
      # Write scripts to file.
      readr::write_lines(x = scriptText, path = glue::glue('R/{functionCategory}.R'), append = T)
      
      Sys.sleep(.2)
    }
  }
}



generateUmbrellaManagementAPIfunctions <- function(){
  
  stop("Already generated! You'll need to hand edit after this.")
  
  require(dplyr)
  require(pluralize)
  
  if(!dir.exists('R')) dir.create('R')
  
  api <- 'UmbrellaManagementAPI'
  
  if(file.exists(glue::glue('R/{api}.R'))) file.remove(glue::glue('R/{api}.R'))
  
  if(!file.exists(glue::glue('data/{api}.json'))) downloadUpdatedAPIspecs()
  
  spec <- jsonlite::read_json(glue::glue('data/{api}.json'), auto_unbox = T)
  
  authType <- spec$components$securitySchemes %>% names() %>% stringr::str_to_lower()
  if(length(authType) == 0){
    authType <- 'oauth2'
  }else{
    authType <- 'basic'
  }
  
  baseUrl <- spec$servers %>% unlist(recursive = F) %>% purrr::pluck('url')
  
  # Track all functions and function categories created and enforce uniqueness.
  allFunctions <- character()
  
  for(path in (spec$paths %>% names())){
    
    pathInfo <- spec %>% purrr::pluck('paths') %>% purrr::pluck(path)
    
    for(method in (pathInfo %>% names())){
      
      methodInfo <- pathInfo %>% purrr::pluck(method)
      
      functionSummary <- methodInfo$summary
      
      functionCategory <- api
      
      functionDescription <- ifelse(is.null(methodInfo$description), methodInfo$summary, methodInfo$description %>% stringr::str_replace_all(stringr::fixed('\n'), ''))
      
      slashPositions <- stringr::str_locate_all(path, stringr::fixed('/')) %>% as.data.frame() %>% pull(start)
      
      if(method == 'get'){
        if(path %>% stringr::str_sub(-1) == stringr::fixed('}')){
          
          verb <- 'get'
          object <- path %>% stringr::str_sub(slashPositions %>% tail(2) %>% first() + 1, slashPositions %>% tail(2) %>% last() - 1) %>% pluralize::singularize() %>% stringr::str_to_title() %>% stringr::str_replace_all('-', '')
          
        }else{
          
          verb <- 'list'
          object <- path %>% stringr::str_sub(slashPositions %>% tail(1) + 1) %>% stringr::str_to_title() %>% stringr::str_replace_all('-', '') %>% pluralize::pluralize()
          if(object %>% stringr::str_detect('By')) object <- object %>% pluralize::singularize()
          prevObject <- path %>% stringr::str_sub(slashPositions %>% tail(3) %>% first() + 1, slashPositions %>% tail(2) %>% first() - 1) %>% pluralize::singularize() %>% stringr::str_to_title() %>% stringr::str_replace_all('-', '')
        }
      }
      
      if(method %in% c('put', 'patch')){
        
        verb <- 'modify'
        object <- path %>% stringr::str_sub(slashPositions %>% tail(2) %>% first() + 1, slashPositions %>% tail(2) %>% last() - 1) %>% pluralize::singularize() %>% stringr::str_to_title() %>% stringr::str_replace_all('-', '')
        
      }
      
      if(method == 'post'){
        
        verb <- 'create'
        object <- path %>% stringr::str_sub(slashPositions %>% tail(1) + 1) %>% pluralize::singularize() %>% stringr::str_to_title() %>% stringr::str_replace_all('-', '')
        
      }
      
      if(method == 'delete'){
        
        verb <- 'delete'
        object <- path %>% stringr::str_sub(slashPositions %>% tail(2) %>% first() + 1, slashPositions %>% tail(2) %>% last() - 1) %>% pluralize::singularize() %>% stringr::str_to_title() %>% stringr::str_replace_all('-', '')
        
      }
      
      if(object %>% stringr::str_detect('list$') | object %>% stringr::str_detect('lists$')) object <- object %>% stringr::str_replace('list', 'List')
      if(object %>% stringr::str_detect('device$') | object %>% stringr::str_detect('devices$')) object <- object %>% stringr::str_replace('device', 'Device')
      
      functionName <- paste0(verb, object)
      
      if(functionName %in% allFunctions){
        functionName <- paste0(verb, prevObject %>% pluralize::singularize(), object %>% pluralize::pluralize())
        if(functionName %>% stringr::str_detect('By')) functionName <- functionName <- paste0(verb, prevObject %>% pluralize::singularize(), object %>% pluralize::singularize())
      }
      
      allFunctions <- append(allFunctions, functionName)
      
      functionParametersNames <- methodInfo$parameters %>% unlist() %>% unname() %>% stringr::str_replace(stringr::fixed('#/components/parameters/'), '')
      functionParameters <- spec$components$parameters %>% purrr::keep(names(.) %in% functionParametersNames) %>% unname()
      
      # These parameters are substituted as part of the endpoint URI and are thus inherently required.
      pathParameters <- functionParameters %>% purrr::keep(function(x) x$`in` == 'path')
      
      # These parameters are passed into the query following the endpoint URI and may be optional.
      queryParameters <- functionParameters %>% purrr::keep(function(x) x$`in` == 'query')
      
      # These parameters are passed into the body of a post or patch request.
      bodyParameters <- NULL
      
      if('requestBody' %in% (methodInfo %>% names())){
        
        schemaObject <- methodInfo$requestBody$content$`application/json`$schema$`$ref` %>% stringr::str_replace('#/components/schemas/', '')
        paramsNames <- spec$components$schemas %>% purrr::pluck(schemaObject) %>% purrr::pluck('properties') %>% names()
        bodyParameters <- data.frame(description = spec$components$schemas %>% purrr::pluck(schemaObject) %>% purrr::pluck('properties') %>% purrr::map(~.x %>% purrr::pluck('description') %>% ifelse(is.null(.), '', .) %>% stringr::str_replace_all(stringr::fixed('\n'), ' ')) %>% unname() %>% unlist(), 
                                     required = paramsNames %in% (spec$components$schemas %>% purrr::pluck(schemaObject) %>% purrr::pluck('required') %>% unlist()),
                                     name = paramsNames) %>% jsonlite::toJSON(auto_unbox = T) %>% jsonlite::fromJSON(F)
      }
      
      # Begin documentation text according to R rOxygen specifications
      documentationText <- glue::glue("\n\n#' {functionSummary}", .trim = F)
      documentationText <- paste0(documentationText, "\n#' ")
      documentationText <- paste0(documentationText, glue::glue("\n#' @description {functionDescription}", .trim = F))
      documentationText <- paste0(documentationText, "\n#' ")
      
      # Begin function text
      functionText <- glue::glue("\n\t{functionName} <- function(", .trim = F)
      
      # Loop through path parameters and add function text.
      for(paramInfo in pathParameters){
        
        required <- F
        if('required' %in% names(paramInfo)){
          if(paramInfo$required %>% as.logical()) required <- T
        }
        
        documentationText <- paste0(documentationText, glue::glue("\n#' @param {paramInfo$name} {paramInfo$description}", .trim = F))
        
        orgId <- paramInfo$name == "organizationId"
        orgIdDefault <- ' = Sys.getenv("umbrellaOrganizationId")'
        
        functionText <- paste0(functionText, glue::glue('{paramInfo$name}{ifelse(required, "", " = NULL")}{ifelse(orgId, orgIdDefault, "")}, ', .trim = F))
      }
      
      # Loop through query parameters and add to function text.
      for(paramInfo in queryParameters){
        
        required <- F
        if('required' %in% names(paramInfo)){
          if(paramInfo$required %>% as.logical()) required <- T
        }
        
        documentationText <- paste0(documentationText, glue::glue("\n#' @param {paramInfo$name} {paramInfo$description}", .trim = F))
        
        functionText <- paste0(functionText, glue::glue('{paramInfo$name}{ifelse(required, "", " = NULL")}, ', .trim = F))
      }
      
      # Loop through body parameters and add to function text.
      for(paramInfo in bodyParameters){
        
        required <- paramInfo$required
        
        documentationText <- paste0(documentationText, glue::glue("\n#' @param {paramInfo$name} {paramInfo$description}", .trim = F))
        
        functionText <- paste0(functionText, glue::glue('{paramInfo$name}{ifelse(required, "", " = NULL")}, ', .trim = F))
      }
      
      # Finish off function text opening line starting the function's definition.
      # Move organizationId = Sys.getenv("umbrellaOrganizationId") to end of function params.
      
      if(functionText %>% stringr::str_detect("umbrellaOrganizationId")){
        functionText <- functionText %>% stringr::str_replace(stringr::fixed('organizationId = Sys.getenv("umbrellaOrganizationId"), '), '')
        functionText <- paste0(functionText, 'organizationId = Sys.getenv("umbrellaOrganizationId")')
      }
      
      functionText <- paste0(functionText, '){') %>% stringr::str_replace(stringr::fixed(', )'), ')')
      
      functionText <- paste0(functionText, '\n\n\t\tparams <- as.list(environment()) %>% purrr::compact()')
      
      # Grab query parameter names.
      if(length(queryParameters) > 0){
        functionText <- paste0(functionText, glue::glue('\n\n\t\tqueryParamNames <- "{queryParameters %>% purrr::map(function(x) x$name) %>% unlist() %>% paste(collapse = ", ")}"', .trim = F))
        functionText <- paste0(functionText, '\n\t\tqueryParams <- params %>% purrr::keep(names(params) %in% (queryParamNames %>% stringr::str_split(", ") %>% purrr::pluck(1)))')
      }else{
        functionText <- paste0(functionText, '\n\n\t\tqueryParams <- NULL')
      }
      
      # Grab body parameter names.
      if(length(bodyParameters) > 0){
        functionText <- paste0(functionText, glue::glue('\n\n\t\tbodyParamNames <- "{bodyParameters %>% purrr::map(~.x$name) %>% paste(collapse = ", ")}"', .trim = F))
        functionText <- paste0(functionText, '\n\t\tbodyParams <- params %>% purrr::keep(names(params) %in% (bodyParamNames %>% stringr::str_split(", ") %>% purrr::pluck(1)))')
      }else{
        functionText <- paste0(functionText, '\n\n\t\tbodyParams <- NULL')
      }
      
      functionText <- paste0(functionText, glue::glue('\n\n\t\tapi <- "{api}"', .trim = F))
      functionText <- paste0(functionText, glue::glue('\n\n\t\tauthType <- "{authType}"', .trim = F))
      
      functionText <- paste0(functionText, glue::glue('\n\n\t\tresponse <- httr::{method %>% stringr::str_to_upper()}(glue::glue("{baseUrl}{path}"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))', .trim = F))
      
      functionText <- paste0(functionText, '\n\n\t\tif(response$status_code == 401){')
      
      functionText <- paste0(functionText, glue::glue('\n\n\t\t\tresponse <- httr::{method %>% stringr::str_to_upper()}(glue::glue("{baseUrl}{path}"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))', .trim = F))
      
      functionText <- paste0(functionText, '\n\t\t}')
      
      functionText <- paste0(functionText, '\n\n\t\tif(response$status_code > 300) return(response)')
      
      functionText <- paste0(functionText, '\n\n\t\thttr::content(response)')
      
      # End Function definition
      functionText <- paste0(functionText, '\n\t}')
      
      # End Documentation definition
      documentationText <- paste0(documentationText, glue::glue("\n#' @concept {functionCategory %>% stringr::str_replace_all(' ', '')}", .trim = F))
      documentationText <- paste0(documentationText, "\n#' @export")
      
      # Update Script text with Documentation text and Function text.
      scriptText <- paste0(documentationText, functionText)
      
      # Write scripts to file.
      readr::write_lines(x = scriptText, path = glue::glue('R/{functionCategory}.R'), append = T)
      
      Sys.sleep(.2)
    }
  }
}
