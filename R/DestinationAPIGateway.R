

#' Retrieve all destination lists of organization
#' 
#' @description Retrieve all destination lists of organization
#' 
#' @param organizationId Organization ID
#' @concept DestinationAPIGateway
#' @export
	listDestinationLists <- function(organizationId = Sys.getenv("umbrellaOrganizationId")){

		params <- as.list(environment()) %>% purrr::compact()

		queryParams <- NULL

		api <- "DestinationAPIGateway"

		authType <- "basic"

		response <- httr::GET(glue::glue("https://management.api.umbrella.com/v1/organizations/{organizationId}/destinationlists"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){

			response <- httr::GET(glue::glue("https://management.api.umbrella.com/v1/organizations/{organizationId}/destinationlists"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		httr::content(response)
	}

#' Create a destination list
#' 
#' @description Create a destination list
#' 
#' @param name New Destination List name
#' @param access Access can be allow or block. It defines destinationlist type.
#' @param destinations Destination URLs. google.com or google.com/news or 10.10.10.10
#' @param comments Comments for each destination.
#' @param types Type for each destination. Type can be DOMAIN, URL, IPV4
#' @param isGlobal Can be true or false. There will be only one default destination list of type allow or block for an organization.
#' @param organizationId Organization ID
#' @concept DestinationAPIGateway
#' @export
	createDestinationList <- function(name, access, destinations, comments = rep('', length(destinations)), types = rep('DOMAIN', length(destinations)), isGlobal = F, organizationId = Sys.getenv("umbrellaOrganizationId")){

	  destinations <- data.frame(destination = destinations, comment = comments, type = types) %>% jsonlite::toJSON(auto_unbox = T) %>% jsonlite::fromJSON(F, F, F, F)
	  
	  body <- list(name = name, access = access, isGlobal = isGlobal, destinations = destinations)
	  
		queryParams <- NULL

		api <- "DestinationAPIGateway"

		authType <- "basic"

		response <- httr::POST(glue::glue("https://management.api.umbrella.com/v1/organizations/{organizationId}/destinationlists"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){

			response <- httr::POST(glue::glue("https://management.api.umbrella.com/v1/organizations/{organizationId}/destinationlists"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		httr::content(response)
	}


#' Rename destination list
#' 
#' @description Rename destination list
#' 
#' @param organizationId Organization ID
#' @param destinationListId Destination List ID
#' @concept DestinationAPIGateway
#' @export
	modifyDestinationList <- function(destinationListId, body = DestinationListPatch(), organizationId = Sys.getenv("umbrellaOrganizationId")){

		params <- as.list(environment()) %>% purrr::compact()

		queryParams <- NULL

		api <- "DestinationAPIGateway"

		authType <- "basic"

		response <- httr::PATCH(glue::glue("https://management.api.umbrella.com/v1/organizations/{organizationId}/destinationlists/{destinationListId}"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){

			response <- httr::PATCH(glue::glue("https://management.api.umbrella.com/v1/organizations/{organizationId}/destinationlists/{destinationListId}"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		httr::content(response)
	}


#' Delete destination list
#' 
#' @description Delete destination list
#' 
#' @param organizationId Organization ID
#' @param destinationListId Destination list ID
#' @concept DestinationAPIGateway
#' @export
	deleteDestinationList <- function(destinationListId, organizationId = Sys.getenv("umbrellaOrganizationId")){

		api <- "DestinationAPIGateway"

		authType <- "basic"

		response <- httr::DELETE(glue::glue("https://management.api.umbrella.com/v1/organizations/{organizationId}/destinationlists/{destinationListId}"), httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){

			response <- httr::DELETE(glue::glue("https://management.api.umbrella.com/v1/organizations/{organizationId}/destinationlists/{destinationListId}"), httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		httr::content(response)
	}


#' Return destination list
#' 
#' @description Return destination list
#' 
#' @param organizationId Organization ID
#' @param destinationListId Destination List ID
#' @concept DestinationAPIGateway
#' @export
	getDestinationList <- function(destinationListId, organizationId = Sys.getenv("umbrellaOrganizationId")){

		params <- as.list(environment()) %>% purrr::compact()

		queryParams <- NULL

		api <- "DestinationAPIGateway"

		authType <- "basic"

		response <- httr::GET(glue::glue("https://management.api.umbrella.com/v1/organizations/{organizationId}/destinationlists/{destinationListId}"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){

			response <- httr::GET(glue::glue("https://management.api.umbrella.com/v1/organizations/{organizationId}/destinationlists/{destinationListId}"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		httr::content(response)
	}


#' Get list of destinations related to destination list
#' 
#' @description Get list of destinations related to destination list
#' 
#' @param organizationId Organization ID
#' @param destinationListId Destination List ID
#' @param page Pagination
#' @param limit Limit for page
#' @concept DestinationAPIGateway
#' @export
	listDestinations <- function(destinationListId, page = NULL, limit = NULL, organizationId = Sys.getenv("umbrellaOrganizationId")){

		params <- as.list(environment()) %>% purrr::compact()

		queryParamNames <- "page, limit"
		queryParams <- params %>% purrr::keep(names(params) %in% (queryParamNames %>% stringr::str_split(", ") %>% purrr::pluck(1)))

		api <- "DestinationAPIGateway"

		authType <- "basic"

		response <- httr::GET(glue::glue("https://management.api.umbrella.com/v1/organizations/{organizationId}/destinationlists/{destinationListId}/destinations"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){

			response <- httr::GET(glue::glue("https://management.api.umbrella.com/v1/organizations/{organizationId}/destinationlists/{destinationListId}/destinations"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		httr::content(response)
	}


#' Add list of destinations to destination list
#' 
#' @description Add list of destinations to destination list
#' 
#' @param organizationId Organization ID
#' @param destinationListId Destination List ID
#' @param DestinationsList List of destinations
#' @concept DestinationAPIGateway
#' @export
	createDestination <- function(destinationListId, body = DestinationsList(), organizationId = Sys.getenv("umbrellaOrganizationId")){

		params <- as.list(environment()) %>% purrr::compact()

		queryParams <- NULL

		api <- "DestinationAPIGateway"

		authType <- "basic"

		response <- httr::POST(glue::glue("https://management.api.umbrella.com/v1/organizations/{organizationId}/destinationlists/{destinationListId}/destinations"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){

			response <- httr::POST(glue::glue("https://management.api.umbrella.com/v1/organizations/{organizationId}/destinationlists/{destinationListId}/destinations"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		httr::content(response)
	}


#' Delete list of destinations from destination list
#' 
#' @description Delete list of destinations from destination list
#' 
#' @param organizationId Organization ID
#' @param destinationListId Destination List ID
#' @param DestinationIdsList List of destination Id's
#' @concept DestinationAPIGateway
#' @export
	deleteDestination <- function(destinationListId, body = DestinationIdsList(), organizationId = Sys.getenv("umbrellaOrganizationId")){

		params <- as.list(environment()) %>% purrr::compact()

		queryParams <- NULL

		api <- "DestinationAPIGateway"

		authType <- "basic"

		response <- httr::DELETE(glue::glue("https://management.api.umbrella.com/v1/organizations/{organizationId}/destinationlists/{destinationListId}/destinations/remove"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){

			response <- httr::DELETE(glue::glue("https://management.api.umbrella.com/v1/organizations/{organizationId}/destinationlists/{destinationListId}/destinations/remove"), query = queryParams, body = body, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		httr::content(response)
	}
