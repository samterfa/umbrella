

#' Activity (all)
#' 
#' @description Gets a list of all activity entries (dns/proxy/firewall/ip) within timeframe.Note: the last 20 minutes of DNS activity cannot be filtered by identity type, threats, or threattypes.
#' 
#' @param organizationId the organization id
#' @param from timestamp to filter from (or relative, e.g. -1days)
#' @param to timestamp to filter to  (or relative, e.g. now)
#' @param offset offset
#' @param limit limit
#' @param domains domain filter, comma delimited
#' @param urls url filter, comma delimited
#' @param categories categories filter, comma delimited ints
#' @param policycategories policy-triggering categories filter, comma delimited ints
#' @param ip ip filter
#' @param ports 81 - port filter, comma delimited ports and ranges
#' @param identityids identity filter, comma delimited
#' @param identitytypes identity type filter, comma delimited.
#' @param applicationid application id filter
#' @param verdict verdict filter, comma delimited
#' @param ruleid firewall rule ID
#' @param filename filename.exe - file name filter, supports wild-carding with *
#' @param securityoverridden whether security was overridden for this request
#' @param bundleid proxy bundle ID
#' @param threats threat names filter, comma delimited
#' @param threattypes threat types filter, comma delimited
#' @param ampdisposition AMP disposition filter, comma delimited
#' @param antivirusthreats threat names caught by antivirus, comma delimited
#' @concept ReportingAPI
#' @export
	listActivities <- function(from, to, domains = NULL, urls = NULL, categories = NULL, policycategories = NULL, ip = NULL, ports = NULL, identityids = NULL, identitytypes = NULL, applicationid = NULL, verdict = NULL, ruleid = NULL, filename = NULL, securityoverridden = NULL, bundleid = NULL, threats = NULL, threattypes = NULL, ampdisposition = NULL, antivirusthreats = NULL, offset = 0, limit = 100, organizationId = Sys.getenv("umbrellaOrganizationId")){

	  from <- formatDatetime(from)
	  to <- formatDatetime(to)
	  
		params <- as.list(environment()) %>% purrr::compact()

		queryParamNames <- "from, to, offset, limit, domains, urls, categories, policycategories, ip, ports, identityids, identitytypes, applicationid, verdict, ruleid, filename, securityoverridden, bundleid, threats, threattypes, ampdisposition, antivirusthreats"
		queryParams <- params %>% purrr::keep(names(params) %in% (queryParamNames %>% stringr::str_split(", ") %>% purrr::pluck(1)))

		api <- "ReportingAPI"

		authType <- "oauth2"

		response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/organizations/{organizationId}/activity"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){
			response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/organizations/{organizationId}/activity"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		if(httr::content(response) %>% class() %>% stringr::str_detect("xml") %>% any()){
			return(httr::content(response) %>% rvest::html_text() %>% jsonlite::fromJSON(simplifyVector = F, simplifyDataFrame = F, simplifyMatrix = F, flatten = F))
		}

		httr::content(response)
	}


#' Activity DNS
#' 
#' @description Gets a list of all DNS entries within timeframe.Note: the last 20 minutes of DNS activity cannot be filtered by identity type, threats, or threattypes.
#' 
#' @param organizationId the organization id
#' @param from timestamp to filter from (or relative, e.g. -1days)
#' @param to timestamp to filter to  (or relative, e.g. now)
#' @param offset offset
#' @param limit limit
#' @param domains domain filter, comma delimited
#' @param categories categories filter, comma delimited ints
#' @param policycategories policy-triggering categories filter, comma delimited ints
#' @param ip ip filter
#' @param order ordering by timestamp
#' @param identityids identity filter, comma delimited
#' @param identitytypes identity type filter, comma delimited.
#' @param applicationid application id filter
#' @param verdict verdict filter, comma delimited
#' @param threats threat names filter, comma delimited
#' @param threattypes threat types filter, comma delimited
#' @concept ReportingAPI
#' @export
	listDns <- function(from, to, domains = NULL, categories = NULL, policycategories = NULL, ip = NULL, order = NULL, identityids = NULL, identitytypes = NULL, applicationid = NULL, verdict = NULL, threats = NULL, threattypes = NULL, offset = 0, limit = 100, organizationId = Sys.getenv("umbrellaOrganizationId")){

	  from <- formatDatetime(from)
	  to <- formatDatetime(to)
	  
		params <- as.list(environment()) %>% purrr::compact()

		queryParamNames <- "from, to, offset, limit, domains, categories, policycategories, ip, order, identityids, identitytypes, applicationid, verdict, threats, threattypes"
		queryParams <- params %>% purrr::keep(names(params) %in% (queryParamNames %>% stringr::str_split(", ") %>% purrr::pluck(1)))

		api <- "ReportingAPI"

		authType <- "oauth2"

		response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/organizations/{organizationId}/activity/dns"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){
			response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/organizations/{organizationId}/activity/dns"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		if(httr::content(response) %>% class() %>% stringr::str_detect("xml") %>% any()){
			return(httr::content(response) %>% rvest::html_text() %>% jsonlite::fromJSON(simplifyVector = F, simplifyDataFrame = F, simplifyMatrix = F, flatten = F))
		}

		httr::content(response)
	}


#' Activity Proxy
#' 
#' @description Gets a list of all Proxy entries within timeframe.
#' 
#' @param organizationId the organization id
#' @param from timestamp to filter from (or relative, e.g. -1days)
#' @param to timestamp to filter to  (or relative, e.g. now)
#' @param offset offset
#' @param limit limit
#' @param domains domain filter, comma delimited
#' @param urls url filter, comma delimited
#' @param categories categories filter, comma delimited ints
#' @param policycategories policy-triggering categories filter, comma delimited ints
#' @param ip ip filter
#' @param order ordering by timestamp
#' @param ports 81 - port filter, comma delimited ports and ranges
#' @param identityids identity filter, comma delimited
#' @param identitytypes identity type filter, comma delimited.
#' @param applicationid application id filter
#' @param verdict verdict filter, comma delimited
#' @param ruleid firewall rule ID
#' @param filename filename.exe - file name filter, supports wild-carding with *
#' @param securityoverridden whether security was overridden for this request
#' @param bundleid proxy bundle ID
#' @param threats threat names filter, comma delimited
#' @param threattypes threat types filter, comma delimited
#' @param ampdisposition AMP disposition filter, comma delimited
#' @param antivirusthreats threat names caught by antivirus, comma delimited
#' @param tenantcontrols filter requests that were part of a tenant control policy
#' @concept ReportingAPI
#' @export
	listProxies <- function(from, to, domains = NULL, urls = NULL, categories = NULL, policycategories = NULL, ip = NULL, order = NULL, ports = NULL, identityids = NULL, identitytypes = NULL, applicationid = NULL, verdict = NULL, ruleid = NULL, filename = NULL, securityoverridden = NULL, bundleid = NULL, threats = NULL, threattypes = NULL, ampdisposition = NULL, antivirusthreats = NULL, tenantcontrols = NULL, offset = 0, limit = 100, organizationId = Sys.getenv("umbrellaOrganizationId")){

	  from <- formatDatetime(from)
	  to <- formatDatetime(to)
	  
		params <- as.list(environment()) %>% purrr::compact()

		queryParamNames <- "from, to, offset, limit, domains, urls, categories, policycategories, ip, order, ports, identityids, identitytypes, applicationid, verdict, ruleid, filename, securityoverridden, bundleid, threats, threattypes, ampdisposition, antivirusthreats, tenantcontrols"
		queryParams <- params %>% purrr::keep(names(params) %in% (queryParamNames %>% stringr::str_split(", ") %>% purrr::pluck(1)))

		api <- "ReportingAPI"

		authType <- "oauth2"

		response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/organizations/{organizationId}/activity/proxy"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){
			response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/organizations/{organizationId}/activity/proxy"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		if(httr::content(response) %>% class() %>% stringr::str_detect("xml") %>% any()){
			return(httr::content(response) %>% rvest::html_text() %>% jsonlite::fromJSON(simplifyVector = F, simplifyDataFrame = F, simplifyMatrix = F, flatten = F))
		}

		httr::content(response)
	}


#' Activity Firewall
#' 
#' @description Gets a list of all Firewall entries within timeframe.
#' 
#' @param organizationId the organization id
#' @param from timestamp to filter from (or relative, e.g. -1days)
#' @param to timestamp to filter to  (or relative, e.g. now)
#' @param offset offset
#' @param limit limit
#' @param ip ip filter
#' @param ports 81 - port filter, comma delimited ports and ranges
#' @param identityids identity filter, comma delimited
#' @param verdict verdict filter, comma delimited
#' @param ruleid firewall rule ID
#' @concept ReportingAPI
#' @export
	listFirewalls <- function(from, to, ip = NULL, ports = NULL, identityids = NULL, verdict = NULL, ruleid = NULL, offset = 0, limit = 100, organizationId = Sys.getenv("umbrellaOrganizationId")){
	  
	  from <- formatDatetime(from)
	  to <- formatDatetime(to)
	  
		params <- as.list(environment()) %>% purrr::compact()

		queryParamNames <- "from, to, offset, limit, ip, ports, identityids, verdict, ruleid"
		queryParams <- params %>% purrr::keep(names(params) %in% (queryParamNames %>% stringr::str_split(", ") %>% purrr::pluck(1)))

		api <- "ReportingAPI"

		authType <- "oauth2"

		response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/organizations/{organizationId}/activity/firewall"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){
			response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/organizations/{organizationId}/activity/firewall"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		if(httr::content(response) %>% class() %>% stringr::str_detect("xml") %>% any()){
			return(httr::content(response) %>% rvest::html_text() %>% jsonlite::fromJSON(simplifyVector = F, simplifyDataFrame = F, simplifyMatrix = F, flatten = F))
		}

		httr::content(response)
	}


#' Activity IP
#' 
#' @description Gets a list of all IP entries within timeframe.
#' 
#' @param organizationId the organization id
#' @param from timestamp to filter from (or relative, e.g. -1days)
#' @param to timestamp to filter to  (or relative, e.g. now)
#' @param offset offset
#' @param limit limit
#' @param categories categories filter, comma delimited ints
#' @param ip ip filter
#' @param ports 81 - port filter, comma delimited ports and ranges
#' @param identityids identity filter, comma delimited
#' @param identitytypes identity type filter, comma delimited.
#' @param verdict verdict filter, comma delimited
#' @concept ReportingAPI
#' @export
	listIps <- function(from, to, categories = NULL, ip = NULL, ports = NULL, identityids = NULL, identitytypes = NULL, verdict = NULL, offset = 0, limit = 100, organizationId = Sys.getenv("umbrellaOrganizationId")){
	  
	  from <- formatDatetime(from)
	  to <- formatDatetime(to)
	  
		params <- as.list(environment()) %>% purrr::compact()

		queryParamNames <- "from, to, offset, limit, categories, ip, ports, identityids, identitytypes, verdict"
		queryParams <- params %>% purrr::keep(names(params) %in% (queryParamNames %>% stringr::str_split(", ") %>% purrr::pluck(1)))

		api <- "ReportingAPI"

		authType <- "oauth2"

		response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/organizations/{organizationId}/activity/ip"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){
			response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/organizations/{organizationId}/activity/ip"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		if(httr::content(response) %>% class() %>% stringr::str_detect("xml") %>% any()){
			return(httr::content(response) %>% rvest::html_text() %>% jsonlite::fromJSON(simplifyVector = F, simplifyDataFrame = F, simplifyMatrix = F, flatten = F))
		}

		httr::content(response)
	}


#' Activity AMP Retrospective
#' 
#' @description Gets a list of all AMP retrospective entries within timeframe.
#' 
#' @param organizationId the organization id
#' @param from timestamp to filter from (or relative, e.g. -1days)
#' @param to timestamp to filter to  (or relative, e.g. now)
#' @param offset offset
#' @param limit limit
#' @param ampdisposition AMP disposition filter, comma delimited
#' @param sha256 sha256 filter
#' @concept ReportingAPI
#' @export
	listAmpRetrospectives <- function(from, to, ampdisposition = NULL, sha256 = NULL, offset = 0, limit = 100, organizationId = Sys.getenv("umbrellaOrganizationId")){
	  
	  from <- formatDatetime(from)
	  to <- formatDatetime(to)
	  
		params <- as.list(environment()) %>% purrr::compact()

		queryParamNames <- "from, to, offset, limit, ampdisposition, sha256"
		queryParams <- params %>% purrr::keep(names(params) %in% (queryParamNames %>% stringr::str_split(", ") %>% purrr::pluck(1)))

		api <- "ReportingAPI"

		authType <- "oauth2"

		response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/organizations/{organizationId}/activity/amp-retrospective"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){
			response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/organizations/{organizationId}/activity/amp-retrospective"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		if(httr::content(response) %>% class() %>% stringr::str_detect("xml") %>% any()){
			return(httr::content(response) %>% rvest::html_text() %>% jsonlite::fromJSON(simplifyVector = F, simplifyDataFrame = F, simplifyMatrix = F, flatten = F))
		}

		httr::content(response)
	}


#' Top Identities (all)
#' 
#' @description Gets a list of identities ordered by the number of requests they made in descending order
#' 
#' @param organizationId the organization id
#' @param from timestamp to filter from (or relative, e.g. -1days)
#' @param to timestamp to filter to  (or relative, e.g. now)
#' @param offset offset
#' @param limit limit
#' @param domains domain filter, comma delimited
#' @param urls url filter, comma delimited
#' @param categories categories filter, comma delimited ints
#' @param policycategories policy-triggering categories filter, comma delimited ints
#' @param ip ip filter
#' @param ports 81 - port filter, comma delimited ports and ranges
#' @param identityids identity filter, comma delimited
#' @param identitytypes identity type filter, comma delimited.
#' @param applicationid application id filter
#' @param verdict verdict filter, comma delimited
#' @param securityoverridden whether security was overridden for this request
#' @param bundleid proxy bundle ID
#' @param threats threat names filter, comma delimited
#' @param threattypes threat types filter, comma delimited
#' @param ampdisposition AMP disposition filter, comma delimited
#' @param sha256 sha256 filter
#' @param antivirusthreats threat names caught by antivirus, comma delimited
#' @concept ReportingAPI
#' @export
	listTopIdentities <- function(from, to, domains = NULL, urls = NULL, categories = NULL, policycategories = NULL, ip = NULL, ports = NULL, identityids = NULL, identitytypes = NULL, applicationid = NULL, verdict = NULL, securityoverridden = NULL, bundleid = NULL, threats = NULL, threattypes = NULL, ampdisposition = NULL, sha256 = NULL, antivirusthreats = NULL, offset = 0, limit = 100, organizationId = Sys.getenv("umbrellaOrganizationId")){
	  
	  from <- formatDatetime(from)
	  to <- formatDatetime(to)
	  
		params <- as.list(environment()) %>% purrr::compact()

		queryParamNames <- "from, to, offset, limit, domains, urls, categories, policycategories, ip, ports, identityids, identitytypes, applicationid, verdict, securityoverridden, bundleid, threats, threattypes, ampdisposition, sha256, antivirusthreats"
		queryParams <- params %>% purrr::keep(names(params) %in% (queryParamNames %>% stringr::str_split(", ") %>% purrr::pluck(1)))

		api <- "ReportingAPI"

		authType <- "oauth2"

		response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/organizations/{organizationId}/top-identities"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){
			response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/organizations/{organizationId}/top-identities"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		if(httr::content(response) %>% class() %>% stringr::str_detect("xml") %>% any()){
			return(httr::content(response) %>% rvest::html_text() %>% jsonlite::fromJSON(simplifyVector = F, simplifyDataFrame = F, simplifyMatrix = F, flatten = F))
		}

		httr::content(response)
	}


#' Top Identities
#' 
#' @description Gets a list of identities ordered by the number of requests they made in descending order
#' 
#' @param organizationId the organization id
#' @param type the type of entries used to calculate the result (dns, proxy, firewall)
#' @param from timestamp to filter from (or relative, e.g. -1days)
#' @param to timestamp to filter to  (or relative, e.g. now)
#' @param offset offset
#' @param limit limit
#' @param domains domain filter, comma delimited
#' @param urls url filter, comma delimited
#' @param categories categories filter, comma delimited ints
#' @param policycategories policy-triggering categories filter, comma delimited ints
#' @param ip ip filter
#' @param ports 81 - port filter, comma delimited ports and ranges
#' @param identityids identity filter, comma delimited
#' @param identitytypes identity type filter, comma delimited.
#' @param applicationid application id filter
#' @param verdict verdict filter, comma delimited
#' @param securityoverridden whether security was overridden for this request
#' @param bundleid proxy bundle ID
#' @param threats threat names filter, comma delimited
#' @param threattypes threat types filter, comma delimited
#' @param ampdisposition AMP disposition filter, comma delimited
#' @param sha256 sha256 filter
#' @param antivirusthreats threat names caught by antivirus, comma delimited
#' @concept ReportingAPI
#' @export
	getTopIdentity <- function(type, from, to, domains = NULL, urls = NULL, categories = NULL, policycategories = NULL, ip = NULL, ports = NULL, identityids = NULL, identitytypes = NULL, applicationid = NULL, verdict = NULL, securityoverridden = NULL, bundleid = NULL, threats = NULL, threattypes = NULL, ampdisposition = NULL, sha256 = NULL, antivirusthreats = NULL, offset = 0, limit = 100, organizationId = Sys.getenv("umbrellaOrganizationId")){

		params <- as.list(environment()) %>% purrr::compact()

		queryParamNames <- "from, to, offset, limit, domains, urls, categories, policycategories, ip, ports, identityids, identitytypes, applicationid, verdict, securityoverridden, bundleid, threats, threattypes, ampdisposition, sha256, antivirusthreats"
		queryParams <- params %>% purrr::keep(names(params) %in% (queryParamNames %>% stringr::str_split(", ") %>% purrr::pluck(1)))

		api <- "ReportingAPI"

		authType <- "oauth2"

		response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/organizations/{organizationId}/top-identities/{type}"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){
			response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/organizations/{organizationId}/top-identities/{type}"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		if(httr::content(response) %>% class() %>% stringr::str_detect("xml") %>% any()){
			return(httr::content(response) %>% rvest::html_text() %>% jsonlite::fromJSON(simplifyVector = F, simplifyDataFrame = F, simplifyMatrix = F, flatten = F))
		}

		httr::content(response)
	}


#' Identity Distribution (all)
#' 
#' @description Gets number of requests by identity types
#' 
#' @param organizationId the organization id
#' @param from timestamp to filter from (or relative, e.g. -1days)
#' @param to timestamp to filter to  (or relative, e.g. now)
#' @param offset offset
#' @param limit limit
#' @param domains domain filter, comma delimited
#' @param urls url filter, comma delimited
#' @param categories categories filter, comma delimited ints
#' @param policycategories policy-triggering categories filter, comma delimited ints
#' @param ip ip filter
#' @param ports 81 - port filter, comma delimited ports and ranges
#' @param identityids identity filter, comma delimited
#' @param identitytypes identity type filter, comma delimited.
#' @param applicationid application id filter
#' @param verdict verdict filter, comma delimited
#' @param securityoverridden whether security was overridden for this request
#' @param bundleid proxy bundle ID
#' @param threats threat names filter, comma delimited
#' @param threattypes threat types filter, comma delimited
#' @param ampdisposition AMP disposition filter, comma delimited
#' @param sha256 sha256 filter
#' @param antivirusthreats threat names caught by antivirus, comma delimited
#' @concept ReportingAPI
#' @export
	listIdentityDistributions <- function(from, to, domains = NULL, urls = NULL, categories = NULL, policycategories = NULL, ip = NULL, ports = NULL, identityids = NULL, identitytypes = NULL, applicationid = NULL, verdict = NULL, securityoverridden = NULL, bundleid = NULL, threats = NULL, threattypes = NULL, ampdisposition = NULL, sha256 = NULL, antivirusthreats = NULL, offset = 0, limit = 100, organizationId = Sys.getenv("umbrellaOrganizationId")){
	  
	  from <- formatDatetime(from)
	  to <- formatDatetime(to)
	  
		params <- as.list(environment()) %>% purrr::compact()

		queryParamNames <- "from, to, offset, limit, domains, urls, categories, policycategories, ip, ports, identityids, identitytypes, applicationid, verdict, securityoverridden, bundleid, threats, threattypes, ampdisposition, sha256, antivirusthreats"
		queryParams <- params %>% purrr::keep(names(params) %in% (queryParamNames %>% stringr::str_split(", ") %>% purrr::pluck(1)))

		api <- "ReportingAPI"

		authType <- "oauth2"

		response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/organizations/{organizationId}/identity-distribution"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){
			response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/organizations/{organizationId}/identity-distribution"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		if(httr::content(response) %>% class() %>% stringr::str_detect("xml") %>% any()){
			return(httr::content(response) %>% rvest::html_text() %>% jsonlite::fromJSON(simplifyVector = F, simplifyDataFrame = F, simplifyMatrix = F, flatten = F))
		}

		httr::content(response)
	}


#' Identity Distribution
#' 
#' @description Gets number of requests by identity types for the traffic type
#' 
#' @param organizationId the organization id
#' @param type the type of entries used to calculate the result (dns, proxy, firewall)
#' @param from timestamp to filter from (or relative, e.g. -1days)
#' @param to timestamp to filter to  (or relative, e.g. now)
#' @param offset offset
#' @param limit limit
#' @param domains domain filter, comma delimited
#' @param urls url filter, comma delimited
#' @param categories categories filter, comma delimited ints
#' @param policycategories policy-triggering categories filter, comma delimited ints
#' @param ip ip filter
#' @param ports 81 - port filter, comma delimited ports and ranges
#' @param identityids identity filter, comma delimited
#' @param identitytypes identity type filter, comma delimited.
#' @param applicationid application id filter
#' @param verdict verdict filter, comma delimited
#' @param securityoverridden whether security was overridden for this request
#' @param bundleid proxy bundle ID
#' @param threats threat names filter, comma delimited
#' @param threattypes threat types filter, comma delimited
#' @param ampdisposition AMP disposition filter, comma delimited
#' @param sha256 sha256 filter
#' @param antivirusthreats threat names caught by antivirus, comma delimited
#' @concept ReportingAPI
#' @export
	getIdentityDistribution <- function(type, from, to, domains = NULL, urls = NULL, categories = NULL, policycategories = NULL, ip = NULL, ports = NULL, identityids = NULL, identitytypes = NULL, applicationid = NULL, verdict = NULL, securityoverridden = NULL, bundleid = NULL, threats = NULL, threattypes = NULL, ampdisposition = NULL, sha256 = NULL, antivirusthreats = NULL, offset = 0, limit = 100, organizationId = Sys.getenv("umbrellaOrganizationId")){

		params <- as.list(environment()) %>% purrr::compact()

		queryParamNames <- "from, to, offset, limit, domains, urls, categories, policycategories, ip, ports, identityids, identitytypes, applicationid, verdict, securityoverridden, bundleid, threats, threattypes, ampdisposition, sha256, antivirusthreats"
		queryParams <- params %>% purrr::keep(names(params) %in% (queryParamNames %>% stringr::str_split(", ") %>% purrr::pluck(1)))

		api <- "ReportingAPI"

		authType <- "oauth2"

		response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/organizations/{organizationId}/identity-distribution/{type}"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){
			response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/organizations/{organizationId}/identity-distribution/{type}"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		if(httr::content(response) %>% class() %>% stringr::str_detect("xml") %>% any()){
			return(httr::content(response) %>% rvest::html_text() %>% jsonlite::fromJSON(simplifyVector = F, simplifyDataFrame = F, simplifyMatrix = F, flatten = F))
		}

		httr::content(response)
	}


#' Top Destinations
#' 
#' @description Gets a list of destinations ordered by the number of requests made in descending order
#' 
#' @param organizationId the organization id
#' @param type the type of entries used to calculate the result (dns, proxy, firewall)
#' @param from timestamp to filter from (or relative, e.g. -1days)
#' @param to timestamp to filter to  (or relative, e.g. now)
#' @param offset offset
#' @param limit limit
#' @param domains domain filter, comma delimited
#' @param urls url filter, comma delimited
#' @param categories categories filter, comma delimited ints
#' @param policycategories policy-triggering categories filter, comma delimited ints
#' @param ip ip filter
#' @param ports 81 - port filter, comma delimited ports and ranges
#' @param identityids identity filter, comma delimited
#' @param identitytypes identity type filter, comma delimited.
#' @param applicationid application id filter
#' @param verdict verdict filter, comma delimited
#' @param securityoverridden whether security was overridden for this request
#' @param bundleid proxy bundle ID
#' @param threats threat names filter, comma delimited
#' @param threattypes threat types filter, comma delimited
#' @param ampdisposition AMP disposition filter, comma delimited
#' @param sha256 sha256 filter
#' @param antivirusthreats threat names caught by antivirus, comma delimited
#' @concept ReportingAPI
#' @export
	getTopDestination <- function(type, from, to, domains = NULL, urls = NULL, categories = NULL, policycategories = NULL, ip = NULL, ports = NULL, identityids = NULL, identitytypes = NULL, applicationid = NULL, verdict = NULL, securityoverridden = NULL, bundleid = NULL, threats = NULL, threattypes = NULL, ampdisposition = NULL, sha256 = NULL, antivirusthreats = NULL, offset = 0, limit = 100, organizationId = Sys.getenv("umbrellaOrganizationId")){

		params <- as.list(environment()) %>% purrr::compact()

		queryParamNames <- "from, to, offset, limit, domains, urls, categories, policycategories, ip, ports, identityids, identitytypes, applicationid, verdict, securityoverridden, bundleid, threats, threattypes, ampdisposition, sha256, antivirusthreats"
		queryParams <- params %>% purrr::keep(names(params) %in% (queryParamNames %>% stringr::str_split(", ") %>% purrr::pluck(1)))

		api <- "ReportingAPI"

		authType <- "oauth2"

		response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/organizations/{organizationId}/top-destinations/{type}"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){
			response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/organizations/{organizationId}/top-destinations/{type}"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		if(httr::content(response) %>% class() %>% stringr::str_detect("xml") %>% any()){
			return(httr::content(response) %>% rvest::html_text() %>% jsonlite::fromJSON(simplifyVector = F, simplifyDataFrame = F, simplifyMatrix = F, flatten = F))
		}

		httr::content(response)
	}


#' Top URLs
#' 
#' @description Returns a list of top-N URLs proxied for a given domain.
#' 
#' @param organizationId the organization id
#' @param from timestamp to filter from (or relative, e.g. -1days)
#' @param to timestamp to filter to  (or relative, e.g. now)
#' @param offset offset
#' @param limit limit
#' @param domains domain filter, comma delimited
#' @param securityoverridden whether security was overridden for this request
#' @param bundleid proxy bundle ID
#' @param ampdisposition AMP disposition filter, comma delimited
#' @param sha256 sha256 filter
#' @param antivirusthreats threat names caught by antivirus, comma delimited
#' @concept ReportingAPI
#' @export
	listTopUrls <- function(from, to, domains = NULL, securityoverridden = NULL, bundleid = NULL, ampdisposition = NULL, sha256 = NULL, antivirusthreats = NULL, offset = 0, limit = 100, organizationId = Sys.getenv("umbrellaOrganizationId")){
	  
	  from <- formatDatetime(from)
	  to <- formatDatetime(to)
	  
		params <- as.list(environment()) %>% purrr::compact()

		queryParamNames <- "from, to, offset, limit, domains, securityoverridden, bundleid, ampdisposition, sha256, antivirusthreats"
		queryParams <- params %>% purrr::keep(names(params) %in% (queryParamNames %>% stringr::str_split(", ") %>% purrr::pluck(1)))

		api <- "ReportingAPI"

		authType <- "oauth2"

		response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/organizations/{organizationId}/top-urls"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){
			response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/organizations/{organizationId}/top-urls"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		if(httr::content(response) %>% class() %>% stringr::str_detect("xml") %>% any()){
			return(httr::content(response) %>% rvest::html_text() %>% jsonlite::fromJSON(simplifyVector = F, simplifyDataFrame = F, simplifyMatrix = F, flatten = F))
		}

		httr::content(response)
	}


#' Top Categories (all)
#' 
#' @description Gets a list of categories ordered by the number of requests made matching the categories in descending order
#' 
#' @param organizationId the organization id
#' @param from timestamp to filter from (or relative, e.g. -1days)
#' @param to timestamp to filter to  (or relative, e.g. now)
#' @param offset offset
#' @param limit limit
#' @param domains domain filter, comma delimited
#' @param urls url filter, comma delimited
#' @param categories categories filter, comma delimited ints
#' @param policycategories policy-triggering categories filter, comma delimited ints
#' @param ip ip filter
#' @param identityids identity filter, comma delimited
#' @param identitytypes identity type filter, comma delimited.
#' @param applicationid application id filter
#' @param verdict verdict filter, comma delimited
#' @param securityoverridden whether security was overridden for this request
#' @param bundleid proxy bundle ID
#' @param threats threat names filter, comma delimited
#' @param threattypes threat types filter, comma delimited
#' @param ampdisposition AMP disposition filter, comma delimited
#' @param sha256 sha256 filter
#' @param antivirusthreats threat names caught by antivirus, comma delimited
#' @concept ReportingAPI
#' @export
	listTopCategories <- function(from, to, domains = NULL, urls = NULL, categories = NULL, policycategories = NULL, ip = NULL, identityids = NULL, identitytypes = NULL, applicationid = NULL, verdict = NULL, securityoverridden = NULL, bundleid = NULL, threats = NULL, threattypes = NULL, ampdisposition = NULL, sha256 = NULL, antivirusthreats = NULL, offset = 0, limit = 100, organizationId = Sys.getenv("umbrellaOrganizationId")){
	  
	  from <- formatDatetime(from)
	  to <- formatDatetime(to)
	  
		params <- as.list(environment()) %>% purrr::compact()

		queryParamNames <- "from, to, offset, limit, domains, urls, categories, policycategories, ip, identityids, identitytypes, applicationid, verdict, securityoverridden, bundleid, threats, threattypes, ampdisposition, sha256, antivirusthreats"
		queryParams <- params %>% purrr::keep(names(params) %in% (queryParamNames %>% stringr::str_split(", ") %>% purrr::pluck(1)))

		api <- "ReportingAPI"

		authType <- "oauth2"

		response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/organizations/{organizationId}/top-categories"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){
			response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/organizations/{organizationId}/top-categories"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		if(httr::content(response) %>% class() %>% stringr::str_detect("xml") %>% any()){
			return(httr::content(response) %>% rvest::html_text() %>% jsonlite::fromJSON(simplifyVector = F, simplifyDataFrame = F, simplifyMatrix = F, flatten = F))
		}

		httr::content(response)
	}


#' Top Categories
#' 
#' @description Gets a list of categories ordered by the number of requests made matching the categories in descending order
#' 
#' @param organizationId the organization id
#' @param type the type of entries used to calculate the result (dns, proxy, firewall)
#' @param from timestamp to filter from (or relative, e.g. -1days)
#' @param to timestamp to filter to  (or relative, e.g. now)
#' @param offset offset
#' @param limit limit
#' @param domains domain filter, comma delimited
#' @param urls url filter, comma delimited
#' @param categories categories filter, comma delimited ints
#' @param policycategories policy-triggering categories filter, comma delimited ints
#' @param ip ip filter
#' @param identityids identity filter, comma delimited
#' @param identitytypes identity type filter, comma delimited.
#' @param applicationid application id filter
#' @param verdict verdict filter, comma delimited
#' @param securityoverridden whether security was overridden for this request
#' @param bundleid proxy bundle ID
#' @param threats threat names filter, comma delimited
#' @param threattypes threat types filter, comma delimited
#' @param ampdisposition AMP disposition filter, comma delimited
#' @param sha256 sha256 filter
#' @param antivirusthreats threat names caught by antivirus, comma delimited
#' @concept ReportingAPI
#' @export
	getTopCategory <- function(type, from, to, domains = NULL, urls = NULL, categories = NULL, policycategories = NULL, ip = NULL, identityids = NULL, identitytypes = NULL, applicationid = NULL, verdict = NULL, securityoverridden = NULL, bundleid = NULL, threats = NULL, threattypes = NULL, ampdisposition = NULL, sha256 = NULL, antivirusthreats = NULL, offset = 0, limit = 100, organizationId = Sys.getenv("umbrellaOrganizationId")){

		params <- as.list(environment()) %>% purrr::compact()

		queryParamNames <- "from, to, offset, limit, domains, urls, categories, policycategories, ip, identityids, identitytypes, applicationid, verdict, securityoverridden, bundleid, threats, threattypes, ampdisposition, sha256, antivirusthreats"
		queryParams <- params %>% purrr::keep(names(params) %in% (queryParamNames %>% stringr::str_split(", ") %>% purrr::pluck(1)))

		api <- "ReportingAPI"

		authType <- "oauth2"

		response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/organizations/{organizationId}/top-categories/{type}"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){
			response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/organizations/{organizationId}/top-categories/{type}"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		if(httr::content(response) %>% class() %>% stringr::str_detect("xml") %>% any()){
			return(httr::content(response) %>% rvest::html_text() %>% jsonlite::fromJSON(simplifyVector = F, simplifyDataFrame = F, simplifyMatrix = F, flatten = F))
		}

		httr::content(response)
	}


#' Top Event Types (all)
#' 
#' @description Gets a list of eventtypes ordered by the number of requests made for each eventtype in descending order. The list of eventtypes is "domain_security", "domain_integration", "url_security", "url_integration", "cisco_amp" and "antivirus".
#' 
#' @param organizationId the organization id
#' @param from timestamp to filter from (or relative, e.g. -1days)
#' @param to timestamp to filter to  (or relative, e.g. now)
#' @param domains domain filter, comma delimited
#' @param urls url filter, comma delimited
#' @param categories categories filter, comma delimited ints
#' @param policycategories policy-triggering categories filter, comma delimited ints
#' @param ip ip filter
#' @param identityids identity filter, comma delimited
#' @param identitytypes identity type filter, comma delimited.
#' @param applicationid application id filter
#' @param verdict verdict filter, comma delimited
#' @param securityoverridden whether security was overridden for this request
#' @param bundleid proxy bundle ID
#' @param threats threat names filter, comma delimited
#' @param threattypes threat types filter, comma delimited
#' @param ampdisposition AMP disposition filter, comma delimited
#' @param antivirusthreats threat names caught by antivirus, comma delimited
#' @concept ReportingAPI
#' @export
	listTopEventtypes <- function(from, to, domains = NULL, urls = NULL, categories = NULL, policycategories = NULL, ip = NULL, identityids = NULL, identitytypes = NULL, applicationid = NULL, verdict = NULL, securityoverridden = NULL, bundleid = NULL, threats = NULL, threattypes = NULL, ampdisposition = NULL, antivirusthreats = NULL, organizationId = Sys.getenv("umbrellaOrganizationId")){
	  
	  from <- formatDatetime(from)
	  to <- formatDatetime(to)
	  
		params <- as.list(environment()) %>% purrr::compact()

		queryParamNames <- "from, to, domains, urls, categories, policycategories, ip, identityids, identitytypes, applicationid, verdict, securityoverridden, bundleid, threats, threattypes, ampdisposition, antivirusthreats"
		queryParams <- params %>% purrr::keep(names(params) %in% (queryParamNames %>% stringr::str_split(", ") %>% purrr::pluck(1)))

		api <- "ReportingAPI"

		authType <- "oauth2"

		response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/organizations/{organizationId}/top-eventtypes"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){
			response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/organizations/{organizationId}/top-eventtypes"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		if(httr::content(response) %>% class() %>% stringr::str_detect("xml") %>% any()){
			return(httr::content(response) %>% rvest::html_text() %>% jsonlite::fromJSON(simplifyVector = F, simplifyDataFrame = F, simplifyMatrix = F, flatten = F))
		}

		httr::content(response)
	}


#' Top DNS Query Types
#' 
#' @description Top DNS Query Types
#' 
#' @param organizationId the organization id
#' @param from timestamp to filter from (or relative, e.g. -1days)
#' @param to timestamp to filter to  (or relative, e.g. now)
#' @param offset offset
#' @param limit limit
#' @param domains domain filter, comma delimited
#' @param categories categories filter, comma delimited ints
#' @param policycategories policy-triggering categories filter, comma delimited ints
#' @param ip ip filter
#' @param order ordering by timestamp
#' @param identityids identity filter, comma delimited
#' @param identitytypes identity type filter, comma delimited.
#' @param applicationid application id filter
#' @param verdict verdict filter, comma delimited
#' @param threats threat names filter, comma delimited
#' @param threattypes threat types filter, comma delimited
#' @concept ReportingAPI
#' @export
	listTopDnsQueryTypes <- function(from, to, domains = NULL, categories = NULL, policycategories = NULL, ip = NULL, order = NULL, identityids = NULL, identitytypes = NULL, applicationid = NULL, verdict = NULL, threats = NULL, threattypes = NULL, offset = 0, limit = 100, organizationId = Sys.getenv("umbrellaOrganizationId")){
	  
	  from <- formatDatetime(from)
	  to <- formatDatetime(to)
	  
		params <- as.list(environment()) %>% purrr::compact()

		queryParamNames <- "from, to, offset, limit, domains, categories, policycategories, ip, order, identityids, identitytypes, applicationid, verdict, threats, threattypes"
		queryParams <- params %>% purrr::keep(names(params) %in% (queryParamNames %>% stringr::str_split(", ") %>% purrr::pluck(1)))

		api <- "ReportingAPI"

		authType <- "oauth2"

		response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/organizations/{organizationId}/top-dns-query-types"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){
			response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/organizations/{organizationId}/top-dns-query-types"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		if(httr::content(response) %>% class() %>% stringr::str_detect("xml") %>% any()){
			return(httr::content(response) %>% rvest::html_text() %>% jsonlite::fromJSON(simplifyVector = F, simplifyDataFrame = F, simplifyMatrix = F, flatten = F))
		}

		httr::content(response)
	}


#' Requests by Hour (ALL)
#' 
#' @description Gets activity volume within timeframe
#' 
#' @param organizationId the organization id
#' @param from timestamp to filter from (or relative, e.g. -1days)
#' @param to timestamp to filter to  (or relative, e.g. now)
#' @param offset offset
#' @param limit limit
#' @param domains domain filter, comma delimited
#' @param urls url filter, comma delimited
#' @param categories categories filter, comma delimited ints
#' @param policycategories policy-triggering categories filter, comma delimited ints
#' @param ip ip filter
#' @param ports 81 - port filter, comma delimited ports and ranges
#' @param identityids identity filter, comma delimited
#' @param identitytypes identity type filter, comma delimited.
#' @param applicationid application id filter
#' @param verdict verdict filter, comma delimited
#' @param securityoverridden whether security was overridden for this request
#' @param bundleid proxy bundle ID
#' @param threats threat names filter, comma delimited
#' @param threattypes threat types filter, comma delimited
#' @param ampdisposition AMP disposition filter, comma delimited
#' @param sha256 sha256 filter
#' @param antivirusthreats threat names caught by antivirus, comma delimited
#' @concept ReportingAPI
#' @export
	listRequestsByHour <- function(from, to, domains = NULL, urls = NULL, categories = NULL, policycategories = NULL, ip = NULL, ports = NULL, identityids = NULL, identitytypes = NULL, applicationid = NULL, verdict = NULL, securityoverridden = NULL, bundleid = NULL, threats = NULL, threattypes = NULL, ampdisposition = NULL, sha256 = NULL, antivirusthreats = NULL, offset = 0, limit = 100, organizationId = Sys.getenv("umbrellaOrganizationId")){
	  
	  from <- formatDatetime(from)
	  to <- formatDatetime(to)
	  
		params <- as.list(environment()) %>% purrr::compact()

		queryParamNames <- "from, to, offset, limit, domains, urls, categories, policycategories, ip, ports, identityids, identitytypes, applicationid, verdict, securityoverridden, bundleid, threats, threattypes, ampdisposition, sha256, antivirusthreats"
		queryParams <- params %>% purrr::keep(names(params) %in% (queryParamNames %>% stringr::str_split(", ") %>% purrr::pluck(1)))

		api <- "ReportingAPI"

		authType <- "oauth2"

		response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/organizations/{organizationId}/requests-by-hour"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){
			response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/organizations/{organizationId}/requests-by-hour"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		if(httr::content(response) %>% class() %>% stringr::str_detect("xml") %>% any()){
			return(httr::content(response) %>% rvest::html_text() %>% jsonlite::fromJSON(simplifyVector = F, simplifyDataFrame = F, simplifyMatrix = F, flatten = F))
		}

		httr::content(response)
	}


#' Requests by Hour
#' 
#' @description Gets activity volume within timeframe
#' 
#' @param organizationId the organization id
#' @param type the type of entries used to calculate the result (dns, proxy, firewall)
#' @param from timestamp to filter from (or relative, e.g. -1days)
#' @param to timestamp to filter to  (or relative, e.g. now)
#' @param offset offset
#' @param limit limit
#' @param domains domain filter, comma delimited
#' @param urls url filter, comma delimited
#' @param categories categories filter, comma delimited ints
#' @param policycategories policy-triggering categories filter, comma delimited ints
#' @param ip ip filter
#' @param ports 81 - port filter, comma delimited ports and ranges
#' @param identityids identity filter, comma delimited
#' @param identitytypes identity type filter, comma delimited.
#' @param applicationid application id filter
#' @param verdict verdict filter, comma delimited
#' @param securityoverridden whether security was overridden for this request
#' @param bundleid proxy bundle ID
#' @param threats threat names filter, comma delimited
#' @param threattypes threat types filter, comma delimited
#' @param ampdisposition AMP disposition filter, comma delimited
#' @param sha256 sha256 filter
#' @param antivirusthreats threat names caught by antivirus, comma delimited
#' @concept ReportingAPI
#' @export
	getRequestsByHour <- function(type, from, to, domains = NULL, urls = NULL, categories = NULL, policycategories = NULL, ip = NULL, ports = NULL, identityids = NULL, identitytypes = NULL, applicationid = NULL, verdict = NULL, securityoverridden = NULL, bundleid = NULL, threats = NULL, threattypes = NULL, ampdisposition = NULL, sha256 = NULL, antivirusthreats = NULL, offset = 0, limit = 100, organizationId = Sys.getenv("umbrellaOrganizationId")){

		params <- as.list(environment()) %>% purrr::compact()

		queryParamNames <- "from, to, offset, limit, domains, urls, categories, policycategories, ip, ports, identityids, identitytypes, applicationid, verdict, securityoverridden, bundleid, threats, threattypes, ampdisposition, sha256, antivirusthreats"
		queryParams <- params %>% purrr::keep(names(params) %in% (queryParamNames %>% stringr::str_split(", ") %>% purrr::pluck(1)))

		api <- "ReportingAPI"

		authType <- "oauth2"

		response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/organizations/{organizationId}/requests-by-hour/{type}"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){
			response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/organizations/{organizationId}/requests-by-hour/{type}"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		if(httr::content(response) %>% class() %>% stringr::str_detect("xml") %>% any()){
			return(httr::content(response) %>% rvest::html_text() %>% jsonlite::fromJSON(simplifyVector = F, simplifyDataFrame = F, simplifyMatrix = F, flatten = F))
		}

		httr::content(response)
	}


#' Requests by Timerange (ALL)
#' 
#' @description Gets activity volume within timeframe
#' 
#' @param organizationId the organization id
#' @param from timestamp to filter from (or relative, e.g. -1days)
#' @param to timestamp to filter to  (or relative, e.g. now)
#' @param offset offset
#' @param limit limit
#' @param domains domain filter, comma delimited
#' @param urls url filter, comma delimited
#' @param categories categories filter, comma delimited ints
#' @param policycategories policy-triggering categories filter, comma delimited ints
#' @param ip ip filter
#' @param ports 81 - port filter, comma delimited ports and ranges
#' @param identityids identity filter, comma delimited
#' @param identitytypes identity type filter, comma delimited.
#' @param applicationid application id filter
#' @param verdict verdict filter, comma delimited
#' @param securityoverridden whether security was overridden for this request
#' @param bundleid proxy bundle ID
#' @param threats threat names filter, comma delimited
#' @param threattypes threat types filter, comma delimited
#' @param ampdisposition AMP disposition filter, comma delimited
#' @param antivirusthreats threat names caught by antivirus, comma delimited
#' @concept ReportingAPI
#' @export
	listRequestsByTimerange <- function(from, to, domains = NULL, urls = NULL, categories = NULL, policycategories = NULL, ip = NULL, ports = NULL, identityids = NULL, identitytypes = NULL, applicationid = NULL, verdict = NULL, securityoverridden = NULL, bundleid = NULL, threats = NULL, threattypes = NULL, ampdisposition = NULL, antivirusthreats = NULL, offset = 0, limit = 100, organizationId = Sys.getenv("umbrellaOrganizationId")){
	  
	  from <- formatDatetime(from)
	  to <- formatDatetime(to)
	  
		params <- as.list(environment()) %>% purrr::compact()

		queryParamNames <- "from, to, offset, limit, domains, urls, categories, policycategories, ip, ports, identityids, identitytypes, applicationid, verdict, securityoverridden, bundleid, threats, threattypes, ampdisposition, antivirusthreats"
		queryParams <- params %>% purrr::keep(names(params) %in% (queryParamNames %>% stringr::str_split(", ") %>% purrr::pluck(1)))

		api <- "ReportingAPI"

		authType <- "oauth2"

		response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/organizations/{organizationId}/requests-by-timerange"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){
			response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/organizations/{organizationId}/requests-by-timerange"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		if(httr::content(response) %>% class() %>% stringr::str_detect("xml") %>% any()){
			return(httr::content(response) %>% rvest::html_text() %>% jsonlite::fromJSON(simplifyVector = F, simplifyDataFrame = F, simplifyMatrix = F, flatten = F))
		}

		httr::content(response)
	}


#' Requests by Timerange
#' 
#' @description Gets activity volume within timeframe
#' 
#' @param organizationId the organization id
#' @param type the type of entries used to calculate the result (dns, proxy, firewall)
#' @param from timestamp to filter from (or relative, e.g. -1days)
#' @param to timestamp to filter to  (or relative, e.g. now)
#' @param offset offset
#' @param limit limit
#' @param domains domain filter, comma delimited
#' @param urls url filter, comma delimited
#' @param categories categories filter, comma delimited ints
#' @param policycategories policy-triggering categories filter, comma delimited ints
#' @param ip ip filter
#' @param ports 81 - port filter, comma delimited ports and ranges
#' @param identityids identity filter, comma delimited
#' @param identitytypes identity type filter, comma delimited.
#' @param applicationid application id filter
#' @param verdict verdict filter, comma delimited
#' @param securityoverridden whether security was overridden for this request
#' @param bundleid proxy bundle ID
#' @param threats threat names filter, comma delimited
#' @param threattypes threat types filter, comma delimited
#' @param ampdisposition AMP disposition filter, comma delimited
#' @param antivirusthreats threat names caught by antivirus, comma delimited
#' @concept ReportingAPI
#' @export
	getRequestsByTimerange <- function(type, from, to, domains = NULL, urls = NULL, categories = NULL, policycategories = NULL, ip = NULL, ports = NULL, identityids = NULL, identitytypes = NULL, applicationid = NULL, verdict = NULL, securityoverridden = NULL, bundleid = NULL, threats = NULL, threattypes = NULL, ampdisposition = NULL, antivirusthreats = NULL, offset = 0, limit = 100, organizationId = Sys.getenv("umbrellaOrganizationId")){

		params <- as.list(environment()) %>% purrr::compact()

		queryParamNames <- "from, to, offset, limit, domains, urls, categories, policycategories, ip, ports, identityids, identitytypes, applicationid, verdict, securityoverridden, bundleid, threats, threattypes, ampdisposition, antivirusthreats"
		queryParams <- params %>% purrr::keep(names(params) %in% (queryParamNames %>% stringr::str_split(", ") %>% purrr::pluck(1)))

		api <- "ReportingAPI"

		authType <- "oauth2"

		response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/organizations/{organizationId}/requests-by-timerange/{type}"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){
			response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/organizations/{organizationId}/requests-by-timerange/{type}"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		if(httr::content(response) %>% class() %>% stringr::str_detect("xml") %>% any()){
			return(httr::content(response) %>% rvest::html_text() %>% jsonlite::fromJSON(simplifyVector = F, simplifyDataFrame = F, simplifyMatrix = F, flatten = F))
		}

		httr::content(response)
	}


#' Requests by Hour and Category (ALL)
#' 
#' @description Gets activity volume within timeframe breakdown by categories
#' 
#' @param organizationId the organization id
#' @param from timestamp to filter from (or relative, e.g. -1days)
#' @param to timestamp to filter to  (or relative, e.g. now)
#' @param offset offset
#' @param limit limit
#' @param domains domain filter, comma delimited
#' @param urls url filter, comma delimited
#' @param categories categories filter, comma delimited ints
#' @param policycategories policy-triggering categories filter, comma delimited ints
#' @param ip ip filter
#' @param identityids identity filter, comma delimited
#' @param identitytypes identity type filter, comma delimited.
#' @param applicationid application id filter
#' @param verdict verdict filter, comma delimited
#' @param securityoverridden whether security was overridden for this request
#' @param bundleid proxy bundle ID
#' @param threats threat names filter, comma delimited
#' @param threattypes threat types filter, comma delimited
#' @param ampdisposition AMP disposition filter, comma delimited
#' @param sha256 sha256 filter
#' @param antivirusthreats threat names caught by antivirus, comma delimited
#' @concept ReportingAPI
#' @export
	listCategoriesByHour <- function(from, to, domains = NULL, urls = NULL, categories = NULL, policycategories = NULL, ip = NULL, identityids = NULL, identitytypes = NULL, applicationid = NULL, verdict = NULL, securityoverridden = NULL, bundleid = NULL, threats = NULL, threattypes = NULL, ampdisposition = NULL, sha256 = NULL, antivirusthreats = NULL, offset = 0, limit = 100, organizationId = Sys.getenv("umbrellaOrganizationId")){
	  
	  from <- formatDatetime(from)
	  to <- formatDatetime(to)
	  
		params <- as.list(environment()) %>% purrr::compact()

		queryParamNames <- "from, to, offset, limit, domains, urls, categories, policycategories, ip, identityids, identitytypes, applicationid, verdict, securityoverridden, bundleid, threats, threattypes, ampdisposition, sha256, antivirusthreats"
		queryParams <- params %>% purrr::keep(names(params) %in% (queryParamNames %>% stringr::str_split(", ") %>% purrr::pluck(1)))

		api <- "ReportingAPI"

		authType <- "oauth2"

		response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/organizations/{organizationId}/categories-by-hour"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){
			response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/organizations/{organizationId}/categories-by-hour"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		if(httr::content(response) %>% class() %>% stringr::str_detect("xml") %>% any()){
			return(httr::content(response) %>% rvest::html_text() %>% jsonlite::fromJSON(simplifyVector = F, simplifyDataFrame = F, simplifyMatrix = F, flatten = F))
		}

		httr::content(response)
	}


#' Requests by Hour and Category
#' 
#' @description Gets activity volume within timeframe breakdown by categories
#' 
#' @param organizationId the organization id
#' @param type the type of entries used to calculate the result (dns, proxy, firewall)
#' @param from timestamp to filter from (or relative, e.g. -1days)
#' @param to timestamp to filter to  (or relative, e.g. now)
#' @param offset offset
#' @param limit limit
#' @param domains domain filter, comma delimited
#' @param urls url filter, comma delimited
#' @param categories categories filter, comma delimited ints
#' @param policycategories policy-triggering categories filter, comma delimited ints
#' @param ip ip filter
#' @param identityids identity filter, comma delimited
#' @param identitytypes identity type filter, comma delimited.
#' @param applicationid application id filter
#' @param verdict verdict filter, comma delimited
#' @param securityoverridden whether security was overridden for this request
#' @param bundleid proxy bundle ID
#' @param threats threat names filter, comma delimited
#' @param threattypes threat types filter, comma delimited
#' @param ampdisposition AMP disposition filter, comma delimited
#' @param sha256 sha256 filter
#' @param antivirusthreats threat names caught by antivirus, comma delimited
#' @concept ReportingAPI
#' @export
	getCategoriesByHour <- function(type, from, to, domains = NULL, urls = NULL, categories = NULL, policycategories = NULL, ip = NULL, identityids = NULL, identitytypes = NULL, applicationid = NULL, verdict = NULL, securityoverridden = NULL, bundleid = NULL, threats = NULL, threattypes = NULL, ampdisposition = NULL, sha256 = NULL, antivirusthreats = NULL, offset = 0, limit = 100, organizationId = Sys.getenv("umbrellaOrganizationId")){

		params <- as.list(environment()) %>% purrr::compact()

		queryParamNames <- "from, to, offset, limit, domains, urls, categories, policycategories, ip, identityids, identitytypes, applicationid, verdict, securityoverridden, bundleid, threats, threattypes, ampdisposition, sha256, antivirusthreats"
		queryParams <- params %>% purrr::keep(names(params) %in% (queryParamNames %>% stringr::str_split(", ") %>% purrr::pluck(1)))

		api <- "ReportingAPI"

		authType <- "oauth2"

		response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/organizations/{organizationId}/categories-by-hour/{type}"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){
			response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/organizations/{organizationId}/categories-by-hour/{type}"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		if(httr::content(response) %>% class() %>% stringr::str_detect("xml") %>% any()){
			return(httr::content(response) %>% rvest::html_text() %>% jsonlite::fromJSON(simplifyVector = F, simplifyDataFrame = F, simplifyMatrix = F, flatten = F))
		}

		httr::content(response)
	}


#' Requests by Timerange and Category (ALL)
#' 
#' @description Gets activity volume within timeframe breakdown by categories
#' 
#' @param organizationId the organization id
#' @param from timestamp to filter from (or relative, e.g. -1days)
#' @param to timestamp to filter to  (or relative, e.g. now)
#' @param offset offset
#' @param limit limit
#' @param domains domain filter, comma delimited
#' @param urls url filter, comma delimited
#' @param categories categories filter, comma delimited ints
#' @param policycategories policy-triggering categories filter, comma delimited ints
#' @param ip ip filter
#' @param identityids identity filter, comma delimited
#' @param identitytypes identity type filter, comma delimited.
#' @param applicationid application id filter
#' @param verdict verdict filter, comma delimited
#' @param securityoverridden whether security was overridden for this request
#' @param bundleid proxy bundle ID
#' @param threats threat names filter, comma delimited
#' @param threattypes threat types filter, comma delimited
#' @param ampdisposition AMP disposition filter, comma delimited
#' @param antivirusthreats threat names caught by antivirus, comma delimited
#' @concept ReportingAPI
#' @export
	listCategoriesByTimerange <- function(from, to, domains = NULL, urls = NULL, categories = NULL, policycategories = NULL, ip = NULL, identityids = NULL, identitytypes = NULL, applicationid = NULL, verdict = NULL, securityoverridden = NULL, bundleid = NULL, threats = NULL, threattypes = NULL, ampdisposition = NULL, antivirusthreats = NULL, offset = 0, limit = 100, organizationId = Sys.getenv("umbrellaOrganizationId")){
	  
	  from <- formatDatetime(from)
	  to <- formatDatetime(to)
	  
		params <- as.list(environment()) %>% purrr::compact()

		queryParamNames <- "from, to, offset, limit, domains, urls, categories, policycategories, ip, identityids, identitytypes, applicationid, verdict, securityoverridden, bundleid, threats, threattypes, ampdisposition, antivirusthreats"
		queryParams <- params %>% purrr::keep(names(params) %in% (queryParamNames %>% stringr::str_split(", ") %>% purrr::pluck(1)))

		api <- "ReportingAPI"

		authType <- "oauth2"

		response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/organizations/{organizationId}/categories-by-timerange"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){
			response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/organizations/{organizationId}/categories-by-timerange"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		if(httr::content(response) %>% class() %>% stringr::str_detect("xml") %>% any()){
			return(httr::content(response) %>% rvest::html_text() %>% jsonlite::fromJSON(simplifyVector = F, simplifyDataFrame = F, simplifyMatrix = F, flatten = F))
		}

		httr::content(response)
	}


#' Requests by Timerange and Category
#' 
#' @description Gets activity volume within timeframe breakdown by categories
#' 
#' @param organizationId the organization id
#' @param type the type of entries used to calculate the result (dns, proxy, firewall)
#' @param from timestamp to filter from (or relative, e.g. -1days)
#' @param to timestamp to filter to  (or relative, e.g. now)
#' @param offset offset
#' @param limit limit
#' @param domains domain filter, comma delimited
#' @param urls url filter, comma delimited
#' @param categories categories filter, comma delimited ints
#' @param policycategories policy-triggering categories filter, comma delimited ints
#' @param ip ip filter
#' @param identityids identity filter, comma delimited
#' @param identitytypes identity type filter, comma delimited.
#' @param applicationid application id filter
#' @param verdict verdict filter, comma delimited
#' @param securityoverridden whether security was overridden for this request
#' @param bundleid proxy bundle ID
#' @param threats threat names filter, comma delimited
#' @param threattypes threat types filter, comma delimited
#' @param ampdisposition AMP disposition filter, comma delimited
#' @param antivirusthreats threat names caught by antivirus, comma delimited
#' @concept ReportingAPI
#' @export
	getCategoriesByTimerange <- function(type, from, to, domains = NULL, urls = NULL, categories = NULL, policycategories = NULL, ip = NULL, identityids = NULL, identitytypes = NULL, applicationid = NULL, verdict = NULL, securityoverridden = NULL, bundleid = NULL, threats = NULL, threattypes = NULL, ampdisposition = NULL, antivirusthreats = NULL, offset = 0, limit = 100, organizationId = Sys.getenv("umbrellaOrganizationId")){

		params <- as.list(environment()) %>% purrr::compact()

		queryParamNames <- "from, to, offset, limit, domains, urls, categories, policycategories, ip, identityids, identitytypes, applicationid, verdict, securityoverridden, bundleid, threats, threattypes, ampdisposition, antivirusthreats"
		queryParams <- params %>% purrr::keep(names(params) %in% (queryParamNames %>% stringr::str_split(", ") %>% purrr::pluck(1)))

		api <- "ReportingAPI"

		authType <- "oauth2"

		response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/organizations/{organizationId}/categories-by-timerange/{type}"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){
			response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/organizations/{organizationId}/categories-by-timerange/{type}"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		if(httr::content(response) %>% class() %>% stringr::str_detect("xml") %>% any()){
			return(httr::content(response) %>% rvest::html_text() %>% jsonlite::fromJSON(simplifyVector = F, simplifyDataFrame = F, simplifyMatrix = F, flatten = F))
		}

		httr::content(response)
	}


#' Deployment Status
#' 
#' @description Gets deployment status within timeframe
#' 
#' @param organizationId the organization id
#' @param from timestamp to filter from (or relative, e.g. -1days)
#' @param to timestamp to filter to  (or relative, e.g. now)
#' @param threats threat names filter, comma delimited
#' @param threattypes threat types filter, comma delimited
#' @concept ReportingAPI
#' @export
	listDeploymentStatuses <- function(from, to, threats = NULL, threattypes = NULL, organizationId = Sys.getenv("umbrellaOrganizationId")){
	  
	  from <- formatDatetime(from)
	  to <- formatDatetime(to)
	  
		params <- as.list(environment()) %>% purrr::compact()

		queryParamNames <- "from, to, threats, threattypes"
		queryParams <- params %>% purrr::keep(names(params) %in% (queryParamNames %>% stringr::str_split(", ") %>% purrr::pluck(1)))

		api <- "ReportingAPI"

		authType <- "oauth2"

		response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/organizations/{organizationId}/deployment-status"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){
			response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/organizations/{organizationId}/deployment-status"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		if(httr::content(response) %>% class() %>% stringr::str_detect("xml") %>% any()){
			return(httr::content(response) %>% rvest::html_text() %>% jsonlite::fromJSON(simplifyVector = F, simplifyDataFrame = F, simplifyMatrix = F, flatten = F))
		}

		httr::content(response)
	}


#' Deployment Status by Organization
#' 
#' @description Gets summary counts of Deployment Status by Organization within timeframe
#' 
#' @param msporganizationid the provider organization id (multiple orgs can live under a provider)
#' @param from timestamp to filter from (or relative, e.g. -1days)
#' @param to timestamp to filter to  (or relative, e.g. now)
#' @param threats threat names filter, comma delimited
#' @param threattypes threat types filter, comma delimited
#' @concept ReportingAPI
#' @export
	listDeployments <- function(msporganizationid, from, to, threats = NULL, threattypes = NULL){

		params <- as.list(environment()) %>% purrr::compact()

		queryParamNames <- "from, to, threats, threattypes"
		queryParams <- params %>% purrr::keep(names(params) %in% (queryParamNames %>% stringr::str_split(", ") %>% purrr::pluck(1)))

		api <- "ReportingAPI"

		authType <- "oauth2"

		response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/providers/{msporganizationId}/deployments"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){
			response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/providers/{msporganizationId}/deployments"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		if(httr::content(response) %>% class() %>% stringr::str_detect("xml") %>% any()){
			return(httr::content(response) %>% rvest::html_text() %>% jsonlite::fromJSON(simplifyVector = F, simplifyDataFrame = F, simplifyMatrix = F, flatten = F))
		}

		httr::content(response)
	}


#' MSP Requests by Hour (ALL)
#' 
#' @description Gets activity volume within timeframe
#' 
#' @param msporganizationid the provider organization id (multiple orgs can live under a provider)
#' @param from timestamp to filter from (or relative, e.g. -1days)
#' @param to timestamp to filter to  (or relative, e.g. now)
#' @param offset offset
#' @param limit limit
#' @param domains domain filter, comma delimited
#' @param urls url filter, comma delimited
#' @param categories categories filter, comma delimited ints
#' @param policycategories policy-triggering categories filter, comma delimited ints
#' @param ip ip filter
#' @param identityids identity filter, comma delimited
#' @param identitytypes identity type filter, comma delimited.
#' @param applicationid application id filter
#' @param verdict verdict filter, comma delimited
#' @param threats threat names filter, comma delimited
#' @param threattypes threat types filter, comma delimited
#' @param sha256 sha256 filter
#' @concept ReportingAPI
#' @export
	listProviderRequestsByHour <- function(msporganizationid, from, to, domains = NULL, urls = NULL, categories = NULL, policycategories = NULL, ip = NULL, identityids = NULL, identitytypes = NULL, applicationid = NULL, verdict = NULL, threats = NULL, threattypes = NULL, sha256 = NULL, offset = 0, limit = 100){

		params <- as.list(environment()) %>% purrr::compact()

		queryParamNames <- "from, to, offset, limit, domains, urls, categories, policycategories, ip, identityids, identitytypes, applicationid, verdict, threats, threattypes, sha256"
		queryParams <- params %>% purrr::keep(names(params) %in% (queryParamNames %>% stringr::str_split(", ") %>% purrr::pluck(1)))

		api <- "ReportingAPI"

		authType <- "oauth2"

		response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/providers/{msporganizationId}/requests-by-hour"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){
			response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/providers/{msporganizationId}/requests-by-hour"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		if(httr::content(response) %>% class() %>% stringr::str_detect("xml") %>% any()){
			return(httr::content(response) %>% rvest::html_text() %>% jsonlite::fromJSON(simplifyVector = F, simplifyDataFrame = F, simplifyMatrix = F, flatten = F))
		}

		httr::content(response)
	}


#' MSP Requests by Timerange (ALL)
#' 
#' @description Gets activity volume within timeframe
#' 
#' @param msporganizationid the provider organization id (multiple orgs can live under a provider)
#' @param from timestamp to filter from (or relative, e.g. -1days)
#' @param to timestamp to filter to  (or relative, e.g. now)
#' @param offset offset
#' @param limit limit
#' @param domains domain filter, comma delimited
#' @param urls url filter, comma delimited
#' @param categories categories filter, comma delimited ints
#' @param policycategories policy-triggering categories filter, comma delimited ints
#' @param ip ip filter
#' @param identityids identity filter, comma delimited
#' @param identitytypes identity type filter, comma delimited.
#' @param applicationid application id filter
#' @param verdict verdict filter, comma delimited
#' @param threats threat names filter, comma delimited
#' @param threattypes threat types filter, comma delimited
#' @concept ReportingAPI
#' @export
	listProviderRequestsByTimerange <- function(msporganizationid, from, to, domains = NULL, urls = NULL, categories = NULL, policycategories = NULL, ip = NULL, identityids = NULL, identitytypes = NULL, applicationid = NULL, verdict = NULL, threats = NULL, threattypes = NULL, offset = 0, limit = 100){

		params <- as.list(environment()) %>% purrr::compact()

		queryParamNames <- "from, to, offset, limit, domains, urls, categories, policycategories, ip, identityids, identitytypes, applicationid, verdict, threats, threattypes"
		queryParams <- params %>% purrr::keep(names(params) %in% (queryParamNames %>% stringr::str_split(", ") %>% purrr::pluck(1)))

		api <- "ReportingAPI"

		authType <- "oauth2"

		response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/providers/{msporganizationId}/requests-by-timerange"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){
			response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/providers/{msporganizationId}/requests-by-timerange"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		if(httr::content(response) %>% class() %>% stringr::str_detect("xml") %>% any()){
			return(httr::content(response) %>% rvest::html_text() %>% jsonlite::fromJSON(simplifyVector = F, simplifyDataFrame = F, simplifyMatrix = F, flatten = F))
		}

		httr::content(response)
	}


#' MSP Requests by Organization
#' 
#' @description Gets summary counts of all requests within timeframe
#' 
#' @param msporganizationid the provider organization id (multiple orgs can live under a provider)
#' @param from timestamp to filter from (or relative, e.g. -1days)
#' @param to timestamp to filter to  (or relative, e.g. now)
#' @param limit limit
#' @param threats threat names filter, comma delimited
#' @param threattypes threat types filter, comma delimited
#' @concept ReportingAPI
#' @export
	listRequestsByOrg <- function(msporganizationid, from, to, threats = NULL, threattypes = NULL, limit = 100){

		params <- as.list(environment()) %>% purrr::compact()

		queryParamNames <- "from, to, limit, threats, threattypes"
		queryParams <- params %>% purrr::keep(names(params) %in% (queryParamNames %>% stringr::str_split(", ") %>% purrr::pluck(1)))

		api <- "ReportingAPI"

		authType <- "oauth2"

		response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/providers/{msporganizationId}/requests-by-org"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){
			response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/providers/{msporganizationId}/requests-by-org"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		if(httr::content(response) %>% class() %>% stringr::str_detect("xml") %>% any()){
			return(httr::content(response) %>% rvest::html_text() %>% jsonlite::fromJSON(simplifyVector = F, simplifyDataFrame = F, simplifyMatrix = F, flatten = F))
		}

		httr::content(response)
	}


#' MSP Requests by Category
#' 
#' @description Gets summary counts of all requests within timeframe
#' 
#' @param msporganizationid the provider organization id (multiple orgs can live under a provider)
#' @param from timestamp to filter from (or relative, e.g. -1days)
#' @param to timestamp to filter to  (or relative, e.g. now)
#' @param limit limit
#' @param threats threat names filter, comma delimited
#' @param threattypes threat types filter, comma delimited
#' @concept ReportingAPI
#' @export
	listRequestsByCategory <- function(msporganizationid, from, to, threats = NULL, threattypes = NULL, limit = 100){

		params <- as.list(environment()) %>% purrr::compact()

		queryParamNames <- "from, to, limit, threats, threattypes"
		queryParams <- params %>% purrr::keep(names(params) %in% (queryParamNames %>% stringr::str_split(", ") %>% purrr::pluck(1)))

		api <- "ReportingAPI"

		authType <- "oauth2"

		response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/providers/{msporganizationId}/requests-by-category"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){
			response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/providers/{msporganizationId}/requests-by-category"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		if(httr::content(response) %>% class() %>% stringr::str_detect("xml") %>% any()){
			return(httr::content(response) %>% rvest::html_text() %>% jsonlite::fromJSON(simplifyVector = F, simplifyDataFrame = F, simplifyMatrix = F, flatten = F))
		}

		httr::content(response)
	}


#' MSP Requests by Destination
#' 
#' @description Gets summary counts of all requests within timeframe
#' 
#' @param msporganizationid the provider organization id (multiple orgs can live under a provider)
#' @param from timestamp to filter from (or relative, e.g. -1days)
#' @param to timestamp to filter to  (or relative, e.g. now)
#' @param limit limit
#' @param threats threat names filter, comma delimited
#' @param threattypes threat types filter, comma delimited
#' @concept ReportingAPI
#' @export
	listRequestsByDestination <- function(msporganizationid, from, to, threats = NULL, threattypes = NULL, limit = 100){

		params <- as.list(environment()) %>% purrr::compact()

		queryParamNames <- "from, to, limit, threats, threattypes"
		queryParams <- params %>% purrr::keep(names(params) %in% (queryParamNames %>% stringr::str_split(", ") %>% purrr::pluck(1)))

		api <- "ReportingAPI"

		authType <- "oauth2"

		response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/providers/{msporganizationId}/requests-by-destination"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){
			response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/providers/{msporganizationId}/requests-by-destination"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		if(httr::content(response) %>% class() %>% stringr::str_detect("xml") %>% any()){
			return(httr::content(response) %>% rvest::html_text() %>% jsonlite::fromJSON(simplifyVector = F, simplifyDataFrame = F, simplifyMatrix = F, flatten = F))
		}

		httr::content(response)
	}


#' MSP Category Requests by Organization
#' 
#' @description Gets summary counts of all requests for each category per organization within the timeframe
#' 
#' @param msporganizationid the provider organization id (multiple orgs can live under a provider)
#' @param from timestamp to filter from (or relative, e.g. -1days)
#' @param to timestamp to filter to  (or relative, e.g. now)
#' @param limit limit
#' @param threats threat names filter, comma delimited
#' @param threattypes threat types filter, comma delimited
#' @concept ReportingAPI
#' @export
	listCategoryRequestsByOrg <- function(msporganizationid, from, to, threats = NULL, threattypes = NULL, limit = 100){

		params <- as.list(environment()) %>% purrr::compact()

		queryParamNames <- "from, to, limit, threats, threattypes"
		queryParams <- params %>% purrr::keep(names(params) %in% (queryParamNames %>% stringr::str_split(", ") %>% purrr::pluck(1)))

		api <- "ReportingAPI"

		authType <- "oauth2"

		response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/providers/{msporganizationId}/category-requests-by-org"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){
			response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/providers/{msporganizationId}/category-requests-by-org"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		if(httr::content(response) %>% class() %>% stringr::str_detect("xml") %>% any()){
			return(httr::content(response) %>% rvest::html_text() %>% jsonlite::fromJSON(simplifyVector = F, simplifyDataFrame = F, simplifyMatrix = F, flatten = F))
		}

		httr::content(response)
	}


#' Bandwidth by Hour (All)
#' 
#' @description Gets bandwidth in bytes within timeframe. Only returns proxy data.
#' 
#' @param organizationId the organization id
#' @param from timestamp to filter from (or relative, e.g. -1days)
#' @param to timestamp to filter to  (or relative, e.g. now)
#' @param offset offset
#' @param limit limit
#' @param domains domain filter, comma delimited
#' @param urls url filter, comma delimited
#' @param categories categories filter, comma delimited ints
#' @param policycategories policy-triggering categories filter, comma delimited ints
#' @param ip ip filter
#' @param identityids identity filter, comma delimited
#' @param identitytypes identity type filter, comma delimited.
#' @param applicationid application id filter
#' @param verdict verdict filter, comma delimited
#' @param securityoverridden whether security was overridden for this request
#' @param bundleid proxy bundle ID
#' @param ampdisposition AMP disposition filter, comma delimited
#' @param sha256 sha256 filter
#' @param antivirusthreats threat names caught by antivirus, comma delimited
#' @concept ReportingAPI
#' @export
	listBandwidthByHour <- function(from, to, domains = NULL, urls = NULL, categories = NULL, policycategories = NULL, ip = NULL, identityids = NULL, identitytypes = NULL, applicationid = NULL, verdict = NULL, securityoverridden = NULL, bundleid = NULL, ampdisposition = NULL, sha256 = NULL, antivirusthreats = NULL, offset = 0, limit = 100, organizationId = Sys.getenv("umbrellaOrganizationId")){
	  
	  from <- formatDatetime(from)
	  to <- formatDatetime(to)
	  
		params <- as.list(environment()) %>% purrr::compact()

		queryParamNames <- "from, to, offset, limit, domains, urls, categories, policycategories, ip, identityids, identitytypes, applicationid, verdict, securityoverridden, bundleid, ampdisposition, sha256, antivirusthreats"
		queryParams <- params %>% purrr::keep(names(params) %in% (queryParamNames %>% stringr::str_split(", ") %>% purrr::pluck(1)))

		api <- "ReportingAPI"

		authType <- "oauth2"

		response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/organizations/{organizationId}/bandwidth-by-hour"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){
			response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/organizations/{organizationId}/bandwidth-by-hour"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		if(httr::content(response) %>% class() %>% stringr::str_detect("xml") %>% any()){
			return(httr::content(response) %>% rvest::html_text() %>% jsonlite::fromJSON(simplifyVector = F, simplifyDataFrame = F, simplifyMatrix = F, flatten = F))
		}

		httr::content(response)
	}


#' Bandwidth by Timerange (All)
#' 
#' @description Gets bandwidth in bytes within timeframe. Only returns proxy data.
#' 
#' @param organizationId the organization id
#' @param from timestamp to filter from (or relative, e.g. -1days)
#' @param to timestamp to filter to  (or relative, e.g. now)
#' @param offset offset
#' @param limit limit
#' @param domains domain filter, comma delimited
#' @param urls url filter, comma delimited
#' @param categories categories filter, comma delimited ints
#' @param policycategories policy-triggering categories filter, comma delimited ints
#' @param ip ip filter
#' @param identityids identity filter, comma delimited
#' @param identitytypes identity type filter, comma delimited.
#' @param applicationid application id filter
#' @param verdict verdict filter, comma delimited
#' @param securityoverridden whether security was overridden for this request
#' @param bundleid proxy bundle ID
#' @param ampdisposition AMP disposition filter, comma delimited
#' @param sha256 sha256 filter
#' @param antivirusthreats threat names caught by antivirus, comma delimited
#' @concept ReportingAPI
#' @export
	listBandwidthByTimerange <- function(from, to, domains = NULL, urls = NULL, categories = NULL, policycategories = NULL, ip = NULL, identityids = NULL, identitytypes = NULL, applicationid = NULL, verdict = NULL, securityoverridden = NULL, bundleid = NULL, ampdisposition = NULL, sha256 = NULL, antivirusthreats = NULL, offset = 0, limit = 100, organizationId = Sys.getenv("umbrellaOrganizationId")){
	  
	  from <- formatDatetime(from)
	  to <- formatDatetime(to)
	  
		params <- as.list(environment()) %>% purrr::compact()

		queryParamNames <- "from, to, offset, limit, domains, urls, categories, policycategories, ip, identityids, identitytypes, applicationid, verdict, securityoverridden, bundleid, ampdisposition, sha256, antivirusthreats"
		queryParams <- params %>% purrr::keep(names(params) %in% (queryParamNames %>% stringr::str_split(", ") %>% purrr::pluck(1)))

		api <- "ReportingAPI"

		authType <- "oauth2"

		response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/organizations/{organizationId}/bandwidth-by-timerange"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){
			response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/organizations/{organizationId}/bandwidth-by-timerange"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		if(httr::content(response) %>% class() %>% stringr::str_detect("xml") %>% any()){
			return(httr::content(response) %>% rvest::html_text() %>% jsonlite::fromJSON(simplifyVector = F, simplifyDataFrame = F, simplifyMatrix = F, flatten = F))
		}

		httr::content(response)
	}


#' Top Files (Proxy)
#' 
#' @description Gets top files within timeframe. Only returns proxy data.
#' 
#' @param organizationId the organization id
#' @param from timestamp to filter from (or relative, e.g. -1days)
#' @param to timestamp to filter to  (or relative, e.g. now)
#' @param offset offset
#' @param limit limit
#' @param domains domain filter, comma delimited
#' @param urls url filter, comma delimited
#' @param categories categories filter, comma delimited ints
#' @param policycategories policy-triggering categories filter, comma delimited ints
#' @param ip ip filter
#' @param identityids identity filter, comma delimited
#' @param identitytypes identity type filter, comma delimited.
#' @param applicationid application id filter
#' @param verdict verdict filter, comma delimited
#' @param securityoverridden whether security was overridden for this request
#' @param bundleid proxy bundle ID
#' @param ampdisposition AMP disposition filter, comma delimited
#' @param sha256 sha256 filter
#' @param antivirusthreats threat names caught by antivirus, comma delimited
#' @concept ReportingAPI
#' @export
	listTopFiles <- function(from, to, domains = NULL, urls = NULL, categories = NULL, policycategories = NULL, ip = NULL, identityids = NULL, identitytypes = NULL, applicationid = NULL, verdict = NULL, securityoverridden = NULL, bundleid = NULL, ampdisposition = NULL, sha256 = NULL, antivirusthreats = NULL, offset = 0, limit = 100, organizationId = Sys.getenv("umbrellaOrganizationId")){
	  
	  from <- formatDatetime(from)
	  to <- formatDatetime(to)
	  
		params <- as.list(environment()) %>% purrr::compact()

		queryParamNames <- "from, to, offset, limit, domains, urls, categories, policycategories, ip, identityids, identitytypes, applicationid, verdict, securityoverridden, bundleid, ampdisposition, sha256, antivirusthreats"
		queryParams <- params %>% purrr::keep(names(params) %in% (queryParamNames %>% stringr::str_split(", ") %>% purrr::pluck(1)))

		api <- "ReportingAPI"

		authType <- "oauth2"

		response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/organizations/{organizationId}/top-files"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){
			response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/organizations/{organizationId}/top-files"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		if(httr::content(response) %>% class() %>% stringr::str_detect("xml") %>% any()){
			return(httr::content(response) %>% rvest::html_text() %>% jsonlite::fromJSON(simplifyVector = F, simplifyDataFrame = F, simplifyMatrix = F, flatten = F))
		}

		httr::content(response)
	}


#' Total Requests (All)
#' 
#' @description Returns a count of total requests for the given filters.
#' 
#' @param organizationId the organization id
#' @param from timestamp to filter from (or relative, e.g. -1days)
#' @param to timestamp to filter to  (or relative, e.g. now)
#' @param domains domain filter, comma delimited
#' @param urls url filter, comma delimited
#' @param categories categories filter, comma delimited ints
#' @param policycategories policy-triggering categories filter, comma delimited ints
#' @param ip ip filter
#' @param ports 81 - port filter, comma delimited ports and ranges
#' @param identityids identity filter, comma delimited
#' @param identitytypes identity type filter, comma delimited.
#' @param applicationid application id filter
#' @param verdict verdict filter, comma delimited
#' @param ruleid firewall rule ID
#' @param securityoverridden whether security was overridden for this request
#' @param bundleid proxy bundle ID
#' @param threats threat names filter, comma delimited
#' @param threattypes threat types filter, comma delimited
#' @param ampdisposition AMP disposition filter, comma delimited
#' @param sha256 sha256 filter
#' @param antivirusthreats threat names caught by antivirus, comma delimited
#' @concept ReportingAPI
#' @export
	listTotalRequests <- function(from, to, domains = NULL, urls = NULL, categories = NULL, policycategories = NULL, ip = NULL, ports = NULL, identityids = NULL, identitytypes = NULL, applicationid = NULL, verdict = NULL, ruleid = NULL, securityoverridden = NULL, bundleid = NULL, threats = NULL, threattypes = NULL, ampdisposition = NULL, sha256 = NULL, antivirusthreats = NULL, organizationId = Sys.getenv("umbrellaOrganizationId")){
	  
	  from <- formatDatetime(from)
	  to <- formatDatetime(to)
	  
		params <- as.list(environment()) %>% purrr::compact()

		queryParamNames <- "from, to, domains, urls, categories, policycategories, ip, ports, identityids, identitytypes, applicationid, verdict, ruleid, securityoverridden, bundleid, threats, threattypes, ampdisposition, sha256, antivirusthreats"
		queryParams <- params %>% purrr::keep(names(params) %in% (queryParamNames %>% stringr::str_split(", ") %>% purrr::pluck(1)))

		api <- "ReportingAPI"

		authType <- "oauth2"

		response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/organizations/{organizationId}/total-requests"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){
			response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/organizations/{organizationId}/total-requests"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		if(httr::content(response) %>% class() %>% stringr::str_detect("xml") %>% any()){
			return(httr::content(response) %>% rvest::html_text() %>% jsonlite::fromJSON(simplifyVector = F, simplifyDataFrame = F, simplifyMatrix = F, flatten = F))
		}

		httr::content(response)
	}


#' Total Requests (By Type)
#' 
#' @description Returns a count of total requests for the given filters.
#' 
#' @param organizationId the organization id
#' @param type the type of entries used to calculate the result (dns, proxy, firewall)
#' @param from timestamp to filter from (or relative, e.g. -1days)
#' @param to timestamp to filter to  (or relative, e.g. now)
#' @param domains domain filter, comma delimited
#' @param urls url filter, comma delimited
#' @param categories categories filter, comma delimited ints
#' @param policycategories policy-triggering categories filter, comma delimited ints
#' @param ip ip filter
#' @param ports 81 - port filter, comma delimited ports and ranges
#' @param identityids identity filter, comma delimited
#' @param identitytypes identity type filter, comma delimited.
#' @param applicationid application id filter
#' @param verdict verdict filter, comma delimited
#' @param ruleid firewall rule ID
#' @param securityoverridden whether security was overridden for this request
#' @param bundleid proxy bundle ID
#' @param threats threat names filter, comma delimited
#' @param threattypes threat types filter, comma delimited
#' @param ampdisposition AMP disposition filter, comma delimited
#' @param antivirusthreats threat names caught by antivirus, comma delimited
#' @concept ReportingAPI
#' @export
	getTotalRequest <- function(type, from, to, domains = NULL, urls = NULL, categories = NULL, policycategories = NULL, ip = NULL, ports = NULL, identityids = NULL, identitytypes = NULL, applicationid = NULL, verdict = NULL, ruleid = NULL, securityoverridden = NULL, bundleid = NULL, threats = NULL, threattypes = NULL, ampdisposition = NULL, antivirusthreats = NULL, organizationId = Sys.getenv("umbrellaOrganizationId")){

		params <- as.list(environment()) %>% purrr::compact()

		queryParamNames <- "from, to, domains, urls, categories, policycategories, ip, ports, identityids, identitytypes, applicationid, verdict, ruleid, securityoverridden, bundleid, threats, threattypes, ampdisposition, antivirusthreats"
		queryParams <- params %>% purrr::keep(names(params) %in% (queryParamNames %>% stringr::str_split(", ") %>% purrr::pluck(1)))

		api <- "ReportingAPI"

		authType <- "oauth2"

		response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/organizations/{organizationId}/total-requests/{type}"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){
			response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/organizations/{organizationId}/total-requests/{type}"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		if(httr::content(response) %>% class() %>% stringr::str_detect("xml") %>% any()){
			return(httr::content(response) %>% rvest::html_text() %>% jsonlite::fromJSON(simplifyVector = F, simplifyDataFrame = F, simplifyMatrix = F, flatten = F))
		}

		httr::content(response)
	}


#' Top Threats (All)
#' 
#' @description Gets top threats within timeframe. Returns both DNS and Proxy data.
#' 
#' @param organizationId the organization id
#' @param from timestamp to filter from (or relative, e.g. -1days)
#' @param to timestamp to filter to  (or relative, e.g. now)
#' @param offset offset
#' @param limit limit
#' @param domains domain filter, comma delimited
#' @param categories categories filter, comma delimited ints
#' @param policycategories policy-triggering categories filter, comma delimited ints
#' @param ip ip filter
#' @param identityids identity filter, comma delimited
#' @param identitytypes identity type filter, comma delimited.
#' @param applicationid application id filter
#' @param verdict verdict filter, comma delimited
#' @param threats threat names filter, comma delimited
#' @param threattypes threat types filter, comma delimited
#' @concept ReportingAPI
#' @export
	listTopThreats <- function(from, to, domains = NULL, categories = NULL, policycategories = NULL, ip = NULL, identityids = NULL, identitytypes = NULL, applicationid = NULL, verdict = NULL, threats = NULL, threattypes = NULL, offset = 0, limit = 100, organizationId = Sys.getenv("umbrellaOrganizationId")){
	  
	  from <- formatDatetime(from)
	  to <- formatDatetime(to)
	  
		params <- as.list(environment()) %>% purrr::compact()

		queryParamNames <- "from, to, offset, limit, domains, categories, policycategories, ip, identityids, identitytypes, applicationid, verdict, threats, threattypes"
		queryParams <- params %>% purrr::keep(names(params) %in% (queryParamNames %>% stringr::str_split(", ") %>% purrr::pluck(1)))

		api <- "ReportingAPI"

		authType <- "oauth2"

		response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/organizations/{organizationId}/top-threats"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){
			response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/organizations/{organizationId}/top-threats"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		if(httr::content(response) %>% class() %>% stringr::str_detect("xml") %>% any()){
			return(httr::content(response) %>% rvest::html_text() %>% jsonlite::fromJSON(simplifyVector = F, simplifyDataFrame = F, simplifyMatrix = F, flatten = F))
		}

		httr::content(response)
	}


#' Top Threats (By Type)
#' 
#' @description Gets top threats within timeframe for the given filters.
#' 
#' @param organizationId the organization id
#' @param type the type of entries used to calculate the result (dns, proxy, firewall)
#' @param to timestamp to filter to  (or relative, e.g. now)
#' @param offset offset
#' @param limit limit
#' @param domains domain filter, comma delimited
#' @param categories categories filter, comma delimited ints
#' @param policycategories policy-triggering categories filter, comma delimited ints
#' @param ip ip filter
#' @param identityids identity filter, comma delimited
#' @param identitytypes identity type filter, comma delimited.
#' @param applicationid application id filter
#' @param verdict verdict filter, comma delimited
#' @param threats threat names filter, comma delimited
#' @param threattypes threat types filter, comma delimited
#' @concept ReportingAPI
#' @export
	getTopThreat <- function(type, to, domains = NULL, categories = NULL, policycategories = NULL, ip = NULL, identityids = NULL, identitytypes = NULL, applicationid = NULL, verdict = NULL, threats = NULL, threattypes = NULL, offset = 0, limit = 100, organizationId = Sys.getenv("umbrellaOrganizationId")){

		params <- as.list(environment()) %>% purrr::compact()

		queryParamNames <- "to, offset, limit, domains, categories, policycategories, ip, identityids, identitytypes, applicationid, verdict, threats, threattypes"
		queryParams <- params %>% purrr::keep(names(params) %in% (queryParamNames %>% stringr::str_split(", ") %>% purrr::pluck(1)))

		api <- "ReportingAPI"

		authType <- "oauth2"

		response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/organizations/{organizationId}/top-threats/{type}"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){
			response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/organizations/{organizationId}/top-threats/{type}"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		if(httr::content(response) %>% class() %>% stringr::str_detect("xml") %>% any()){
			return(httr::content(response) %>% rvest::html_text() %>% jsonlite::fromJSON(simplifyVector = F, simplifyDataFrame = F, simplifyMatrix = F, flatten = F))
		}

		httr::content(response)
	}


#' Top Threat Types (All)
#' 
#' @description Gets top threat-types within timeframe. Returns both DNS and Proxy data.
#' 
#' @param organizationId the organization id
#' @param from timestamp to filter from (or relative, e.g. -1days)
#' @param to timestamp to filter to  (or relative, e.g. now)
#' @param offset offset
#' @param limit limit
#' @param domains domain filter, comma delimited
#' @param categories categories filter, comma delimited ints
#' @param policycategories policy-triggering categories filter, comma delimited ints
#' @param ip ip filter
#' @param identityids identity filter, comma delimited
#' @param identitytypes identity type filter, comma delimited.
#' @param applicationid application id filter
#' @param verdict verdict filter, comma delimited
#' @param threats threat names filter, comma delimited
#' @param threattypes threat types filter, comma delimited
#' @concept ReportingAPI
#' @export
	listTopThreatTypes <- function(from, to, domains = NULL, categories = NULL, policycategories = NULL, ip = NULL, identityids = NULL, identitytypes = NULL, applicationid = NULL, verdict = NULL, threats = NULL, threattypes = NULL, offset = 0, limit = 100, organizationId = Sys.getenv("umbrellaOrganizationId")){
	  
	  from <- formatDatetime(from)
	  to <- formatDatetime(to)
	  
		params <- as.list(environment()) %>% purrr::compact()

		queryParamNames <- "from, to, offset, limit, domains, categories, policycategories, ip, identityids, identitytypes, applicationid, verdict, threats, threattypes"
		queryParams <- params %>% purrr::keep(names(params) %in% (queryParamNames %>% stringr::str_split(", ") %>% purrr::pluck(1)))

		api <- "ReportingAPI"

		authType <- "oauth2"

		response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/organizations/{organizationId}/top-threat-types"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){
			response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/organizations/{organizationId}/top-threat-types"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		if(httr::content(response) %>% class() %>% stringr::str_detect("xml") %>% any()){
			return(httr::content(response) %>% rvest::html_text() %>% jsonlite::fromJSON(simplifyVector = F, simplifyDataFrame = F, simplifyMatrix = F, flatten = F))
		}

		httr::content(response)
	}


#' Top Threat Types (By Type)
#' 
#' @description Gets top threat-types within timeframe for the given filters.
#' 
#' @param organizationId the organization id
#' @param type the type of entries used to calculate the result (dns, proxy, firewall)
#' @param to timestamp to filter to  (or relative, e.g. now)
#' @param offset offset
#' @param limit limit
#' @param domains domain filter, comma delimited
#' @param categories categories filter, comma delimited ints
#' @param policycategories policy-triggering categories filter, comma delimited ints
#' @param ip ip filter
#' @param identityids identity filter, comma delimited
#' @param identitytypes identity type filter, comma delimited.
#' @param applicationid application id filter
#' @param verdict verdict filter, comma delimited
#' @param threats threat names filter, comma delimited
#' @param threattypes threat types filter, comma delimited
#' @concept ReportingAPI
#' @export
	getTopThreatType <- function(type, to, domains = NULL, categories = NULL, policycategories = NULL, ip = NULL, identityids = NULL, identitytypes = NULL, applicationid = NULL, verdict = NULL, threats = NULL, threattypes = NULL, offset = 0, limit = 100, organizationId = Sys.getenv("umbrellaOrganizationId")){

		params <- as.list(environment()) %>% purrr::compact()

		queryParamNames <- "to, offset, limit, domains, categories, policycategories, ip, identityids, identitytypes, applicationid, verdict, threats, threattypes"
		queryParams <- params %>% purrr::keep(names(params) %in% (queryParamNames %>% stringr::str_split(", ") %>% purrr::pluck(1)))

		api <- "ReportingAPI"

		authType <- "oauth2"

		response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/organizations/{organizationId}/top-threat-types/{type}"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){
			response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/organizations/{organizationId}/top-threat-types/{type}"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		if(httr::content(response) %>% class() %>% stringr::str_detect("xml") %>% any()){
			return(httr::content(response) %>% rvest::html_text() %>% jsonlite::fromJSON(simplifyVector = F, simplifyDataFrame = F, simplifyMatrix = F, flatten = F))
		}

		httr::content(response)
	}


#' Applications
#' 
#' @description Applications
#' 
#' @param organizationId the organization id
#' @param application the application name
#' @concept ReportingAPI
#' @export
	listApplications <- function(application = NULL, organizationId = Sys.getenv("umbrellaOrganizationId")){

		params <- as.list(environment()) %>% purrr::compact()

		queryParamNames <- "application"
		queryParams <- params %>% purrr::keep(names(params) %in% (queryParamNames %>% stringr::str_split(", ") %>% purrr::pluck(1)))

		api <- "ReportingAPI"

		authType <- "oauth2"

		response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/organizations/{organizationId}/applications"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){
			response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/organizations/{organizationId}/applications"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		if(httr::content(response) %>% class() %>% stringr::str_detect("xml") %>% any()){
			return(httr::content(response) %>% rvest::html_text() %>% jsonlite::fromJSON(simplifyVector = F, simplifyDataFrame = F, simplifyMatrix = F, flatten = F))
		}

		httr::content(response)
	}


#' Categories
#' 
#' @description Categories
#' 
#' @param organizationId the organization id
#' @concept ReportingAPI
#' @export
	listCategories <- function(organizationId = Sys.getenv("umbrellaOrganizationId")){

		params <- as.list(environment()) %>% purrr::compact()

		queryParams <- NULL

		api <- "ReportingAPI"

		authType <- "oauth2"

		response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/organizations/{organizationId}/categories"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){
			response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/organizations/{organizationId}/categories"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		if(httr::content(response) %>% class() %>% stringr::str_detect("xml") %>% any()){
			return(httr::content(response) %>% rvest::html_text() %>% jsonlite::fromJSON(simplifyVector = F, simplifyDataFrame = F, simplifyMatrix = F, flatten = F))
		}

		httr::content(response)
	}


#' MSP Categories
#' 
#' @description MSP Categories
#' 
#' @param msporganizationid the provider organization id (multiple orgs can live under a provider)
#' @concept ReportingAPI
#' @export
	listProviderCategories <- function(msporganizationid){

		params <- as.list(environment()) %>% purrr::compact()

		queryParams <- NULL

		api <- "ReportingAPI"

		authType <- "oauth2"

		response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/providers/{msporganizationId}/categories"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){
			response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/providers/{msporganizationId}/categories"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		if(httr::content(response) %>% class() %>% stringr::str_detect("xml") %>% any()){
			return(httr::content(response) %>% rvest::html_text() %>% jsonlite::fromJSON(simplifyVector = F, simplifyDataFrame = F, simplifyMatrix = F, flatten = F))
		}

		httr::content(response)
	}


#' Identities
#' 
#' @description Identities
#' 
#' @param organizationId the organization id
#' @param offset offset
#' @param limit limit
#' @param identitytypes identity type filter, comma delimited.
#' @param search a string to search for in the identities
#' @concept ReportingAPI
#' @export
	listIdentities <- function(identitytypes = NULL, search = NULL, offset = 0, limit = 100, organizationId = Sys.getenv("umbrellaOrganizationId")){

		params <- as.list(environment()) %>% purrr::compact()

		queryParamNames <- "offset, limit, identitytypes, search"
		queryParams <- params %>% purrr::keep(names(params) %in% (queryParamNames %>% stringr::str_split(", ") %>% purrr::pluck(1)))

		api <- "ReportingAPI"

		authType <- "oauth2"

		response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/organizations/{organizationId}/identities"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){
			response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/organizations/{organizationId}/identities"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		if(httr::content(response) %>% class() %>% stringr::str_detect("xml") %>% any()){
			return(httr::content(response) %>% rvest::html_text() %>% jsonlite::fromJSON(simplifyVector = F, simplifyDataFrame = F, simplifyMatrix = F, flatten = F))
		}

		httr::content(response)
	}


#' Identity
#' 
#' @description Identity
#' 
#' @param organizationId the organization id
#' @param identityid the identity id
#' @concept ReportingAPI
#' @export
	getIdentity <- function(identityid, organizationId = Sys.getenv("umbrellaOrganizationId")){

		params <- as.list(environment()) %>% purrr::compact()

		queryParams <- NULL

		api <- "ReportingAPI"

		authType <- "oauth2"

		response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/organizations/{organizationId}/identities/{identityid}"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){
			response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/organizations/{organizationId}/identities/{identityid}"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		if(httr::content(response) %>% class() %>% stringr::str_detect("xml") %>% any()){
			return(httr::content(response) %>% rvest::html_text() %>% jsonlite::fromJSON(simplifyVector = F, simplifyDataFrame = F, simplifyMatrix = F, flatten = F))
		}

		httr::content(response)
	}


#' Threat Types
#' 
#' @description Threat Types
#' 
#' @param organizationId the organization id
#' @concept ReportingAPI
#' @export
	listThreatTypes <- function(organizationId = Sys.getenv("umbrellaOrganizationId")){

		params <- as.list(environment()) %>% purrr::compact()

		queryParams <- NULL

		api <- "ReportingAPI"

		authType <- "oauth2"

		response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/organizations/{organizationId}/threat-types"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){
			response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/organizations/{organizationId}/threat-types"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		if(httr::content(response) %>% class() %>% stringr::str_detect("xml") %>% any()){
			return(httr::content(response) %>% rvest::html_text() %>% jsonlite::fromJSON(simplifyVector = F, simplifyDataFrame = F, simplifyMatrix = F, flatten = F))
		}

		httr::content(response)
	}


#' Threat Type
#' 
#' @description Threat Type
#' 
#' @param organizationId the organization id
#' @param threattypeid the threat-type name
#' @concept ReportingAPI
#' @export
	getThreatType <- function(threattypeid, organizationId = Sys.getenv("umbrellaOrganizationId")){

		params <- as.list(environment()) %>% purrr::compact()

		queryParams <- NULL

		api <- "ReportingAPI"

		authType <- "oauth2"

		response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/organizations/{organizationId}/threat-types/{threattypeid}"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){
			response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/organizations/{organizationId}/threat-types/{threattypeid}"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		if(httr::content(response) %>% class() %>% stringr::str_detect("xml") %>% any()){
			return(httr::content(response) %>% rvest::html_text() %>% jsonlite::fromJSON(simplifyVector = F, simplifyDataFrame = F, simplifyMatrix = F, flatten = F))
		}

		httr::content(response)
	}


#' Threat Names
#' 
#' @description Threat Names
#' 
#' @param organizationId the organization id
#' @concept ReportingAPI
#' @export
	listThreatNames <- function(organizationId = Sys.getenv("umbrellaOrganizationId")){

		params <- as.list(environment()) %>% purrr::compact()

		queryParams <- NULL

		api <- "ReportingAPI"

		authType <- "oauth2"

		response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/organizations/{organizationId}/threat-names"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){
			response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/organizations/{organizationId}/threat-names"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		if(httr::content(response) %>% class() %>% stringr::str_detect("xml") %>% any()){
			return(httr::content(response) %>% rvest::html_text() %>% jsonlite::fromJSON(simplifyVector = F, simplifyDataFrame = F, simplifyMatrix = F, flatten = F))
		}

		httr::content(response)
	}


#' Threat Name
#' 
#' @description Threat Name
#' 
#' @param organizationId the organization id
#' @param threatnameid the threat-name name
#' @concept ReportingAPI
#' @export
	getThreatName <- function(threatnameid, organizationId = Sys.getenv("umbrellaOrganizationId")){

		params <- as.list(environment()) %>% purrr::compact()

		queryParams <- NULL

		api <- "ReportingAPI"

		authType <- "oauth2"

		response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/organizations/{organizationId}/threat-names/{threatnameid}"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){
			response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/organizations/{organizationId}/threat-names/{threatnameid}"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		if(httr::content(response) %>% class() %>% stringr::str_detect("xml") %>% any()){
			return(httr::content(response) %>% rvest::html_text() %>% jsonlite::fromJSON(simplifyVector = F, simplifyDataFrame = F, simplifyMatrix = F, flatten = F))
		}

		httr::content(response)
	}


#' Top IPs Internal
#' 
#' @description Top IPs Internal
#' 
#' @param organizationId the organization id
#' @param from timestamp to filter from (or relative, e.g. -1days)
#' @param to timestamp to filter to  (or relative, e.g. now)
#' @param domains domain filter, comma delimited
#' @param categories categories filter, comma delimited ints
#' @param policycategories policy-triggering categories filter, comma delimited ints
#' @param ip ip filter
#' @param identityids identity filter, comma delimited
#' @param identitytypes identity type filter, comma delimited.
#' @param applicationid application id filter
#' @param verdict verdict filter, comma delimited
#' @param threats threat names filter, comma delimited
#' @param threattypes threat types filter, comma delimited
#' @concept ReportingAPI
#' @export
	listInternals <- function(from, to, domains = NULL, categories = NULL, policycategories = NULL, ip = NULL, identityids = NULL, identitytypes = NULL, applicationid = NULL, verdict = NULL, threats = NULL, threattypes = NULL, organizationId = Sys.getenv("umbrellaOrganizationId")){
	  
	  from <- formatDatetime(from)
	  to <- formatDatetime(to)
	  
		params <- as.list(environment()) %>% purrr::compact()

		queryParamNames <- "from, to, domains, categories, policycategories, ip, identityids, identitytypes, applicationid, verdict, threats, threattypes"
		queryParams <- params %>% purrr::keep(names(params) %in% (queryParamNames %>% stringr::str_split(", ") %>% purrr::pluck(1)))

		api <- "ReportingAPI"

		authType <- "oauth2"

		response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/organizations/{organizationId}/top-ips/internal"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){
			response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/organizations/{organizationId}/top-ips/internal"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		if(httr::content(response) %>% class() %>% stringr::str_detect("xml") %>% any()){
			return(httr::content(response) %>% rvest::html_text() %>% jsonlite::fromJSON(simplifyVector = F, simplifyDataFrame = F, simplifyMatrix = F, flatten = F))
		}

		httr::content(response)
	}


#' Summary (All)
#' 
#' @description Summary (All)
#' 
#' @param organizationId the organization id
#' @param from timestamp to filter from (or relative, e.g. -1days)
#' @param to timestamp to filter to  (or relative, e.g. now)
#' @param offset offset
#' @param limit limit
#' @param domains domain filter, comma delimited
#' @param urls url filter, comma delimited
#' @param categories categories filter, comma delimited ints
#' @param policycategories policy-triggering categories filter, comma delimited ints
#' @param ip ip filter
#' @param ports 81 - port filter, comma delimited ports and ranges
#' @param identityids identity filter, comma delimited
#' @param identitytypes identity type filter, comma delimited.
#' @param applicationid application id filter
#' @param verdict verdict filter, comma delimited
#' @param ruleid firewall rule ID
#' @param filename filename.exe - file name filter, supports wild-carding with *
#' @param securityoverridden whether security was overridden for this request
#' @param bundleid proxy bundle ID
#' @param threats threat names filter, comma delimited
#' @param threattypes threat types filter, comma delimited
#' @param ampdisposition AMP disposition filter, comma delimited
#' @param antivirusthreats threat names caught by antivirus, comma delimited
#' @concept ReportingAPI
#' @export
	listSummaries <- function(from, to, domains = NULL, urls = NULL, categories = NULL, policycategories = NULL, ip = NULL, ports = NULL, identityids = NULL, identitytypes = NULL, applicationid = NULL, verdict = NULL, ruleid = NULL, filename = NULL, securityoverridden = NULL, bundleid = NULL, threats = NULL, threattypes = NULL, ampdisposition = NULL, antivirusthreats = NULL, offset = 0, limit = 100, organizationId = Sys.getenv("umbrellaOrganizationId")){
	  
	  from <- formatDatetime(from)
	  to <- formatDatetime(to)
	  
		params <- as.list(environment()) %>% purrr::compact()

		queryParamNames <- "from, to, offset, limit, domains, urls, categories, policycategories, ip, ports, identityids, identitytypes, applicationid, verdict, ruleid, filename, securityoverridden, bundleid, threats, threattypes, ampdisposition, antivirusthreats"
		queryParams <- params %>% purrr::keep(names(params) %in% (queryParamNames %>% stringr::str_split(", ") %>% purrr::pluck(1)))

		api <- "ReportingAPI"

		authType <- "oauth2"

		response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/organizations/{organizationId}/summary"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){
			response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/organizations/{organizationId}/summary"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		if(httr::content(response) %>% class() %>% stringr::str_detect("xml") %>% any()){
			return(httr::content(response) %>% rvest::html_text() %>% jsonlite::fromJSON(simplifyVector = F, simplifyDataFrame = F, simplifyMatrix = F, flatten = F))
		}

		httr::content(response)
	}


#' Summary
#' 
#' @description Summary
#' 
#' @param organizationId the organization id
#' @param type the type of entries used to calculate the result (dns, proxy, firewall)
#' @param from timestamp to filter from (or relative, e.g. -1days)
#' @param to timestamp to filter to  (or relative, e.g. now)
#' @param offset offset
#' @param limit limit
#' @param domains domain filter, comma delimited
#' @param urls url filter, comma delimited
#' @param categories categories filter, comma delimited ints
#' @param policycategories policy-triggering categories filter, comma delimited ints
#' @param ip ip filter
#' @param ports 81 - port filter, comma delimited ports and ranges
#' @param identityids identity filter, comma delimited
#' @param identitytypes identity type filter, comma delimited.
#' @param applicationid application id filter
#' @param verdict verdict filter, comma delimited
#' @param ruleid firewall rule ID
#' @param filename filename.exe - file name filter, supports wild-carding with *
#' @param securityoverridden whether security was overridden for this request
#' @param bundleid proxy bundle ID
#' @param threats threat names filter, comma delimited
#' @param threattypes threat types filter, comma delimited
#' @param ampdisposition AMP disposition filter, comma delimited
#' @param antivirusthreats threat names caught by antivirus, comma delimited
#' @concept ReportingAPI
#' @export
	getSummary <- function(type, from, to, domains = NULL, urls = NULL, categories = NULL, policycategories = NULL, ip = NULL, ports = NULL, identityids = NULL, identitytypes = NULL, applicationid = NULL, verdict = NULL, ruleid = NULL, filename = NULL, securityoverridden = NULL, bundleid = NULL, threats = NULL, threattypes = NULL, ampdisposition = NULL, antivirusthreats = NULL, offset = 0, limit = 100, organizationId = Sys.getenv("umbrellaOrganizationId")){

		params <- as.list(environment()) %>% purrr::compact()

		queryParamNames <- "from, to, offset, limit, domains, urls, categories, policycategories, ip, ports, identityids, identitytypes, applicationid, verdict, ruleid, filename, securityoverridden, bundleid, threats, threattypes, ampdisposition, antivirusthreats"
		queryParams <- params %>% purrr::keep(names(params) %in% (queryParamNames %>% stringr::str_split(", ") %>% purrr::pluck(1)))

		api <- "ReportingAPI"

		authType <- "oauth2"

		response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/organizations/{organizationId}/summary/{type}"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){
			response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/organizations/{organizationId}/summary/{type}"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		if(httr::content(response) %>% class() %>% stringr::str_detect("xml") %>% any()){
			return(httr::content(response) %>% rvest::html_text() %>% jsonlite::fromJSON(simplifyVector = F, simplifyDataFrame = F, simplifyMatrix = F, flatten = F))
		}

		httr::content(response)
	}


#' Summaries by category (All)
#' 
#' @description Summaries by category (All)
#' 
#' @param organizationId the organization id
#' @param from timestamp to filter from (or relative, e.g. -1days)
#' @param to timestamp to filter to  (or relative, e.g. now)
#' @param offset offset
#' @param limit limit
#' @param domains domain filter, comma delimited
#' @param urls url filter, comma delimited
#' @param categories categories filter, comma delimited ints
#' @param policycategories policy-triggering categories filter, comma delimited ints
#' @param ip ip filter
#' @param identityids identity filter, comma delimited
#' @param identitytypes identity type filter, comma delimited.
#' @param applicationid application id filter
#' @param verdict verdict filter, comma delimited
#' @param ruleid firewall rule ID
#' @param filename filename.exe - file name filter, supports wild-carding with *
#' @param securityoverridden whether security was overridden for this request
#' @param bundleid proxy bundle ID
#' @param threats threat names filter, comma delimited
#' @param threattypes threat types filter, comma delimited
#' @param ampdisposition AMP disposition filter, comma delimited
#' @param antivirusthreats threat names caught by antivirus, comma delimited
#' @concept ReportingAPI
#' @export
	listSummariesByCategory <- function(from, to, domains = NULL, urls = NULL, categories = NULL, policycategories = NULL, ip = NULL, identityids = NULL, identitytypes = NULL, applicationid = NULL, verdict = NULL, ruleid = NULL, filename = NULL, securityoverridden = NULL, bundleid = NULL, threats = NULL, threattypes = NULL, ampdisposition = NULL, antivirusthreats = NULL, offset = 0, limit = 100, organizationId = Sys.getenv("umbrellaOrganizationId")){
	  
	  from <- formatDatetime(from)
	  to <- formatDatetime(to)
	  
		params <- as.list(environment()) %>% purrr::compact()

		queryParamNames <- "from, to, offset, limit, domains, urls, categories, policycategories, ip, identityids, identitytypes, applicationid, verdict, ruleid, filename, securityoverridden, bundleid, threats, threattypes, ampdisposition, antivirusthreats"
		queryParams <- params %>% purrr::keep(names(params) %in% (queryParamNames %>% stringr::str_split(", ") %>% purrr::pluck(1)))

		api <- "ReportingAPI"

		authType <- "oauth2"

		response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/organizations/{organizationId}/summaries-by-category"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){
			response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/organizations/{organizationId}/summaries-by-category"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		if(httr::content(response) %>% class() %>% stringr::str_detect("xml") %>% any()){
			return(httr::content(response) %>% rvest::html_text() %>% jsonlite::fromJSON(simplifyVector = F, simplifyDataFrame = F, simplifyMatrix = F, flatten = F))
		}

		httr::content(response)
	}


#' Summaries by category
#' 
#' @description Summaries by category
#' 
#' @param organizationId the organization id
#' @param type the type of entries used to calculate the result (dns, proxy, firewall)
#' @param from timestamp to filter from (or relative, e.g. -1days)
#' @param to timestamp to filter to  (or relative, e.g. now)
#' @param offset offset
#' @param limit limit
#' @param domains domain filter, comma delimited
#' @param urls url filter, comma delimited
#' @param categories categories filter, comma delimited ints
#' @param policycategories policy-triggering categories filter, comma delimited ints
#' @param ip ip filter
#' @param identityids identity filter, comma delimited
#' @param identitytypes identity type filter, comma delimited.
#' @param applicationid application id filter
#' @param verdict verdict filter, comma delimited
#' @param ruleid firewall rule ID
#' @param filename filename.exe - file name filter, supports wild-carding with *
#' @param securityoverridden whether security was overridden for this request
#' @param bundleid proxy bundle ID
#' @param threats threat names filter, comma delimited
#' @param threattypes threat types filter, comma delimited
#' @param ampdisposition AMP disposition filter, comma delimited
#' @param antivirusthreats threat names caught by antivirus, comma delimited
#' @concept ReportingAPI
#' @export
	getSummariesByCategory <- function(type, from, to, domains = NULL, urls = NULL, categories = NULL, policycategories = NULL, ip = NULL, identityids = NULL, identitytypes = NULL, applicationid = NULL, verdict = NULL, ruleid = NULL, filename = NULL, securityoverridden = NULL, bundleid = NULL, threats = NULL, threattypes = NULL, ampdisposition = NULL, antivirusthreats = NULL, offset = 0, limit = 100, organizationId = Sys.getenv("umbrellaOrganizationId")){

		params <- as.list(environment()) %>% purrr::compact()

		queryParamNames <- "from, to, offset, limit, domains, urls, categories, policycategories, ip, identityids, identitytypes, applicationid, verdict, ruleid, filename, securityoverridden, bundleid, threats, threattypes, ampdisposition, antivirusthreats"
		queryParams <- params %>% purrr::keep(names(params) %in% (queryParamNames %>% stringr::str_split(", ") %>% purrr::pluck(1)))

		api <- "ReportingAPI"

		authType <- "oauth2"

		response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/organizations/{organizationId}/summaries-by-category/{type}"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){
			response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/organizations/{organizationId}/summaries-by-category/{type}"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		if(httr::content(response) %>% class() %>% stringr::str_detect("xml") %>% any()){
			return(httr::content(response) %>% rvest::html_text() %>% jsonlite::fromJSON(simplifyVector = F, simplifyDataFrame = F, simplifyMatrix = F, flatten = F))
		}

		httr::content(response)
	}


#' Summaries by destination (All)
#' 
#' @description Summaries by destination (All)
#' 
#' @param organizationId the organization id
#' @param from timestamp to filter from (or relative, e.g. -1days)
#' @param to timestamp to filter to  (or relative, e.g. now)
#' @param offset offset
#' @param limit limit
#' @param domains domain filter, comma delimited
#' @param urls url filter, comma delimited
#' @param categories categories filter, comma delimited ints
#' @param policycategories policy-triggering categories filter, comma delimited ints
#' @param ip ip filter
#' @param identityids identity filter, comma delimited
#' @param identitytypes identity type filter, comma delimited.
#' @param applicationid application id filter
#' @param verdict verdict filter, comma delimited
#' @param ruleid firewall rule ID
#' @param filename filename.exe - file name filter, supports wild-carding with *
#' @param securityoverridden whether security was overridden for this request
#' @param bundleid proxy bundle ID
#' @param threats threat names filter, comma delimited
#' @param threattypes threat types filter, comma delimited
#' @param ampdisposition AMP disposition filter, comma delimited
#' @param antivirusthreats threat names caught by antivirus, comma delimited
#' @concept ReportingAPI
#' @export
	listSummariesByDestination <- function(from, to, domains = NULL, urls = NULL, categories = NULL, policycategories = NULL, ip = NULL, identityids = NULL, identitytypes = NULL, applicationid = NULL, verdict = NULL, ruleid = NULL, filename = NULL, securityoverridden = NULL, bundleid = NULL, threats = NULL, threattypes = NULL, ampdisposition = NULL, antivirusthreats = NULL, offset = 0, limit = 100, organizationId = Sys.getenv("umbrellaOrganizationId")){
	  
	  from <- formatDatetime(from)
	  to <- formatDatetime(to)
	  
		params <- as.list(environment()) %>% purrr::compact()

		queryParamNames <- "from, to, offset, limit, domains, urls, categories, policycategories, ip, identityids, identitytypes, applicationid, verdict, ruleid, filename, securityoverridden, bundleid, threats, threattypes, ampdisposition, antivirusthreats"
		queryParams <- params %>% purrr::keep(names(params) %in% (queryParamNames %>% stringr::str_split(", ") %>% purrr::pluck(1)))

		api <- "ReportingAPI"

		authType <- "oauth2"

		response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/organizations/{organizationId}/summaries-by-destination"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){
			response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/organizations/{organizationId}/summaries-by-destination"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		if(httr::content(response) %>% class() %>% stringr::str_detect("xml") %>% any()){
			return(httr::content(response) %>% rvest::html_text() %>% jsonlite::fromJSON(simplifyVector = F, simplifyDataFrame = F, simplifyMatrix = F, flatten = F))
		}

		httr::content(response)
	}


#' Summaries by destination
#' 
#' @description Summaries by destination
#' 
#' @param organizationId the organization id
#' @param type the type of entries used to calculate the result (dns, proxy, firewall)
#' @param from timestamp to filter from (or relative, e.g. -1days)
#' @param to timestamp to filter to  (or relative, e.g. now)
#' @param offset offset
#' @param limit limit
#' @param domains domain filter, comma delimited
#' @param urls url filter, comma delimited
#' @param categories categories filter, comma delimited ints
#' @param policycategories policy-triggering categories filter, comma delimited ints
#' @param ip ip filter
#' @param identityids identity filter, comma delimited
#' @param identitytypes identity type filter, comma delimited.
#' @param applicationid application id filter
#' @param verdict verdict filter, comma delimited
#' @param ruleid firewall rule ID
#' @param filename filename.exe - file name filter, supports wild-carding with *
#' @param securityoverridden whether security was overridden for this request
#' @param bundleid proxy bundle ID
#' @param threats threat names filter, comma delimited
#' @param threattypes threat types filter, comma delimited
#' @param ampdisposition AMP disposition filter, comma delimited
#' @param antivirusthreats threat names caught by antivirus, comma delimited
#' @concept ReportingAPI
#' @export
	getSummariesByDestination <- function(type, from, to, domains = NULL, urls = NULL, categories = NULL, policycategories = NULL, ip = NULL, identityids = NULL, identitytypes = NULL, applicationid = NULL, verdict = NULL, ruleid = NULL, filename = NULL, securityoverridden = NULL, bundleid = NULL, threats = NULL, threattypes = NULL, ampdisposition = NULL, antivirusthreats = NULL, offset = 0, limit = 100, organizationId = Sys.getenv("umbrellaOrganizationId")){

		params <- as.list(environment()) %>% purrr::compact()

		queryParamNames <- "from, to, offset, limit, domains, urls, categories, policycategories, ip, identityids, identitytypes, applicationid, verdict, ruleid, filename, securityoverridden, bundleid, threats, threattypes, ampdisposition, antivirusthreats"
		queryParams <- params %>% purrr::keep(names(params) %in% (queryParamNames %>% stringr::str_split(", ") %>% purrr::pluck(1)))

		api <- "ReportingAPI"

		authType <- "oauth2"

		response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/organizations/{organizationId}/summaries-by-destination/{type}"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){
			response <- httr::GET(glue::glue("https://reports.api.umbrella.com/v2/organizations/{organizationId}/summaries-by-destination/{type}"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		if(httr::content(response) %>% class() %>% stringr::str_detect("xml") %>% any()){
			return(httr::content(response) %>% rvest::html_text() %>% jsonlite::fromJSON(simplifyVector = F, simplifyDataFrame = F, simplifyMatrix = F, flatten = F))
		}

		httr::content(response)
	}
