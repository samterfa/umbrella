

#' List organizations
#' 
#' @description List Organizations available to the current API client. Currently this will return a single Organization. In the future it may be possible to access multiple Umbrella Organizations with a single set of credentials.
#' 
#' @concept UmbrellaManagementAPI
#' @export
	listOrganizations <- function(){

		api <- "UmbrellaManagementAPI"

		authType <- "basic"

		response <- httr::GET(glue::glue("https://management.api.umbrella.com/v1/organizations"), httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){

			response <- httr::GET(glue::glue("https://management.api.umbrella.com/v1/organizations"), httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		httr::content(response)
	}


#' Create a network device
#' 
#' @description Create a network device
#' 
#' @param organizationId Id of the Organization owning the resources
#' @param model The model name of the device. Must be unique to your organization.
#' @param macAddress The MAC address of the device (formatted as 12 characters, no hyphens or colons). Must be unique.
#' @param name 
#' @param serialNumber The serial number of the device
#' @param tag A text tag that describes the device or this particular origin assigned to the device. Must be unique to your organization.
#' @concept UmbrellaManagementAPI
#' @export
	createNetworkDevice <- function(model, macAddress, name, serialNumber, tag = NULL, organizationId = Sys.getenv("umbrellaOrganizationId")){

		params <- as.list(environment()) %>% purrr::compact()

		queryParams <- NULL

		bodyParamNames <- "model, macAddress, name, serialNumber, tag"
		bodyParams <- params %>% purrr::keep(names(params) %in% (bodyParamNames %>% stringr::str_split(", ") %>% purrr::pluck(1)))

		api <- "UmbrellaManagementAPI"

		authType <- "basic"

		response <- httr::POST(glue::glue("https://management.api.umbrella.com//v1/organizations/{organizationId}/networkdevices"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){

			response <- httr::POST(glue::glue("https://management.api.umbrella.com//v1/organizations/{organizationId}/networkdevices"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		httr::content(response)
	}


#' Retrieve all network devices for the current organization
#' 
#' @description Retrieve all network devices for the current organization
#' 
#' @param organizationId Id of the Organization owning the resources
#' @concept UmbrellaManagementAPI
#' @export
	listNetworkDevices <- function(organizationId = Sys.getenv("umbrellaOrganizationId")){

		api <- "UmbrellaManagementAPI"

		authType <- "basic"

		response <- httr::GET(glue::glue("https://management.api.umbrella.com/v1/organizations/{organizationId}/networkdevices"), httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){

			response <- httr::GET(glue::glue("https://management.api.umbrella.com/v1/organizations/{organizationId}/networkdevices"), httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		httr::content(response)
	}


#' Update a network device
#' 
#' @description Update a network device
#' 
#' @param organizationId Id of the Organization owning the resources
#' @param originId originID of the identity to remove from the DNS or web policy
#' @param name 
#' @concept UmbrellaManagementAPI
#' @export
	modifyNetworkDevice <- function(originId, name, organizationId = Sys.getenv("umbrellaOrganizationId")){

		params <- as.list(environment()) %>% purrr::compact()

		queryParams <- NULL

		bodyParamNames <- "name"
		bodyParams <- params %>% purrr::keep(names(params) %in% (bodyParamNames %>% stringr::str_split(", ") %>% purrr::pluck(1)))

		api <- "UmbrellaManagementAPI"

		authType <- "basic"

		response <- httr::PATCH(glue::glue("https://management.api.umbrella.com//v1/organizations/{organizationId}/networkdevices/{originId}"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){

			response <- httr::PATCH(glue::glue("https://management.api.umbrella.com//v1/organizations/{organizationId}/networkdevices/{originId}"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		httr::content(response)
	}


#' Retrieve an existing network device
#' 
#' @description Retrieve an existing network device
#' 
#' @param organizationId Id of the Organization owning the resources
#' @param originId originID of the identity to remove from the DNS or web policy
#' @concept UmbrellaManagementAPI
#' @export
	getNetworkDevice <- function(originId, organizationId = Sys.getenv("umbrellaOrganizationId")){

		api <- "UmbrellaManagementAPI"

		authType <- "basic"

		response <- httr::GET(glue::glue("https://management.api.umbrella.com/v1/organizations/{organizationId}/networkdevices/{originId}"), httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){

			response <- httr::GET(glue::glue("https://management.api.umbrella.com/v1/organizations/{organizationId}/networkdevices/{originId}"), httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		httr::content(response)
	}


#' Remove an existing network device
#' 
#' @description Remove an existing network device
#' 
#' @param organizationId Id of the Organization owning the resources
#' @param originId originID of the identity to remove from the DNS or web policy
#' @concept UmbrellaManagementAPI
#' @export
	deleteNetworkDevice <- function(originId, organizationId = Sys.getenv("umbrellaOrganizationId")){

		api <- "UmbrellaManagementAPI"

		authType <- "basic"

		response <- httr::DELETE(glue::glue("https://management.api.umbrella.com/v1/organizations/{organizationId}/networkdevices/{originId}"), httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){

			response <- httr::DELETE(glue::glue("https://management.api.umbrella.com/v1/organizations/{organizationId}/networkdevices/{originId}"), httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		httr::content(response)
	}


#' List the Policies associated with a Network Device
#' 
#' @description List the Policies associated with a Network Device
#' 
#' @param organizationId Id of the Organization owning the resources
#' @param originId originID of the identity to remove from the DNS or web policy
#' @concept UmbrellaManagementAPI
#' @export
	listPoliciesForDevice <- function(originId, organizationId = Sys.getenv("umbrellaOrganizationId")){

		api <- "UmbrellaManagementAPI"

		authType <- "basic"

		response <- httr::GET(glue::glue("https://management.api.umbrella.com/v1/organizations/{organizationId}/networkdevices/{originId}/policies"), httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){

			response <- httr::GET(glue::glue("https://management.api.umbrella.com/v1/organizations/{organizationId}/networkdevices/{originId}/policies"), httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		httr::content(response)
	}


#' List Policies
#' 
#' @description List DNS and web policies, if no filters are supplied then only DNS policies accessible with the current credentials will be returned.
#' 
#' @param organizationId Id of the Organization owning the resources
#' @param page Start of the page
#' @param limit Number of items to return
#' @param type The type of policies to be returned (either DNS or web)
#' @concept UmbrellaManagementAPI
#' @export
	listOrganizationPolicies <- function(page = 1, limit = 100, type = 'DNS', organizationId = Sys.getenv("umbrellaOrganizationId")){

		params <- as.list(environment()) %>% purrr::compact()

		queryParamNames <- "page, limit, type"
		queryParams <- params %>% purrr::keep(names(params) %in% (queryParamNames %>% stringr::str_split(", ") %>% purrr::pluck(1)))

		api <- "UmbrellaManagementAPI"

		authType <- "basic"

		response <- httr::GET(glue::glue("https://management.api.umbrella.com/v1/organizations/{organizationId}/policies"), query = queryParams, httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){

			response <- httr::GET(glue::glue("https://management.api.umbrella.com/v1/organizations/{organizationId}/policies"), query = queryParams, httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		httr::content(response)
	}


#' Add an Identity to a directly applied DNS or web policy
#' 
#' @description Policy changes can take up to 20 minutes to take affect globally. Additionally, for DNS policies, TTLs, caching, and session re-use may cause some devices and domains to appear to take longer to update.
#' 
#' @param organizationId Id of the Organization owning the resources
#' @param originId originID of the identity to remove from the DNS or web policy
#' @param policyId ID of the DNS or web policy to act upon
#' @concept UmbrellaManagementAPI
#' @export
	addIdentityToPolicy <- function(originId, policyId, organizationId = Sys.getenv("umbrellaOrganizationId")){

		api <- "UmbrellaManagementAPI"

		authType <- "basic"

		response <- httr::PUT(glue::glue("https://management.api.umbrella.com/v1/organizations/{organizationId}/policies/{policyId}/identities/{originId}"), httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){

			response <- httr::PUT(glue::glue("https://management.api.umbrella.com/v1/organizations/{organizationId}/policies/{policyId}/identities/{originId}"), httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		httr::content(response)
	}


#' Remove an Identity from a directly applied DNS or web policy
#' 
#' @description Policy changes can take up to 20 minutes to take affect globally. Additionally, for DNS policies, TTLs, caching, and session re-use may cause some devices and domains to appear to take longer to update.
#' 
#' @param organizationId Id of the Organization owning the resources
#' @param originId originID of the identity to remove from the DNS or web policy
#' @param policyId ID of the DNS or web policy to act upon
#' @concept UmbrellaManagementAPI
#' @export
	removeIdentityFromPolicy <- function(originId, policyId, organizationId = Sys.getenv("umbrellaOrganizationId")){

		api <- "UmbrellaManagementAPI"

		authType <- "basic"

		response <- httr::DELETE(glue::glue("https://management.api.umbrella.com/v1/organizations/{organizationId}/policies/{policyId}/identities/{originId}"), httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){

			response <- httr::DELETE(glue::glue("https://management.api.umbrella.com/v1/organizations/{organizationId}/policies/{policyId}/identities/{originId}"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		httr::content(response)
	}
