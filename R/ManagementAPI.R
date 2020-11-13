

#' Return a Service Provider
#' 
#' @description Return a Service Provider
#' 
#' @param serviceProviderId The ServiceProviderID related to this Service Provider
#' @concept ManagementAPI
#' @export
	getServiceprovider <- function(serviceProviderId){

		params <- as.list(environment()) %>% purrr::compact()

		queryParams <- NULL

		bodyParams <- NULL

		api <- "ManagementAPI"

		authType <- "basic"

		response <- httr::GET(glue::glue("https://management.api.umbrella.com/v1/serviceproviders/{serviceProviderId}"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){

			response <- httr::GET(glue::glue("https://management.api.umbrella.com/v1/serviceproviders/{serviceProviderId}"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		httr::content(response)
	}


#' Create a new Customer
#' 
#' @description Create a new Customer
#' 
#' @param serviceProviderId The ServiceProviderID related to this Customer
#' @param customerName customer organization name
#' @param customerType customer organization type (applicable only for MSSP):   `term` = term customer   `msla` = msla customer 
#' @param isTrial applicable only for MSSP with SPLA (MSLA) licensing
#' @param seats number of users
#' @param streetAddress street address
#' @param streetAddress2 street address2
#' @param city city
#' @param state state
#' @param countryCode country code
#' @param zipCode zip code
#' @param packageId package id
#' @param dealId deal id
#' @param adminEmails array of admin emails
#' @param ccwDealOwnerEmails array of CCW deal owner emails
#' @concept ManagementAPI
#' @export
	createCustomer <- function(serviceProviderId, customerName, customerType = NULL, isTrial = NULL, seats, streetAddress, streetAddress2 = NULL, city, state, countryCode, zipCode = NULL, packageId, dealId = NULL, adminEmails, ccwDealOwnerEmails = NULL){

		params <- as.list(environment()) %>% purrr::compact()

		queryParams <- NULL

		bodyParamNames <- "customerName, customerType, isTrial, seats, streetAddress, streetAddress2, city, state, countryCode, zipCode, packageId, dealId, adminEmails, ccwDealOwnerEmails"
		bodyParams <- params %>% purrr::keep(names(params) %in% (bodyParamNames %>% stringr::str_split(", ") %>% purrr::pluck(1)))

		api <- "ManagementAPI"

		authType <- "basic"

		response <- httr::POST(glue::glue("https://management.api.umbrella.com/v1/serviceproviders/{serviceProviderId}/customers"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){

			response <- httr::POST(glue::glue("https://management.api.umbrella.com/v1/serviceproviders/{serviceProviderId}/customers"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		httr::content(response)
	}


#' Return a list of Customer
#' 
#' @description Return a list of Customer
#' 
#' @param serviceProviderId The ServiceProviderID related to this Customer
#' @param limit Number of results to return for the given page number.
#' @param page Page number of results.
#' @concept ManagementAPI
#' @export
	listCustomers <- function(serviceProviderId, limit = 100, page = 1){

		params <- as.list(environment()) %>% purrr::compact()

		queryParamNames <- "limit, page"
		queryParams <- params %>% purrr::keep(names(params) %in% (queryParamNames %>% stringr::str_split(", ") %>% purrr::pluck(1)))

		bodyParams <- NULL

		api <- "ManagementAPI"

		authType <- "basic"

		response <- httr::GET(glue::glue("https://management.api.umbrella.com/v1/serviceproviders/{serviceProviderId}/customers"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){

			response <- httr::GET(glue::glue("https://management.api.umbrella.com/v1/serviceproviders/{serviceProviderId}/customers"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		httr::content(response)
	}


#' Return a Customer
#' 
#' @description Return a Customer
#' 
#' @param serviceProviderId The ServiceProviderID related to this Customer
#' @param customerId The customerID related to this Customer
#' @concept ManagementAPI
#' @export
	getCustomer <- function(serviceProviderId, customerId){

		params <- as.list(environment()) %>% purrr::compact()

		queryParams <- NULL

		bodyParams <- NULL

		api <- "ManagementAPI"

		authType <- "basic"

		response <- httr::GET(glue::glue("https://management.api.umbrella.com/v1/serviceproviders/{serviceProviderId}/customers/{customerId}"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){

			response <- httr::GET(glue::glue("https://management.api.umbrella.com/v1/serviceproviders/{serviceProviderId}/customers/{customerId}"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		httr::content(response)
	}


#' Delete a Customer
#' 
#' @description Delete a Customer
#' 
#' @param serviceProviderId The ServiceProviderID related to this Customer
#' @param customerId The customerID related to this Customer
#' @concept ManagementAPI
#' @export
	deleteCustomer <- function(serviceProviderId, customerId){

		params <- as.list(environment()) %>% purrr::compact()

		queryParams <- NULL

		bodyParams <- NULL

		api <- "ManagementAPI"

		authType <- "basic"

		response <- httr::DELETE(glue::glue("https://management.api.umbrella.com/v1/serviceproviders/{serviceProviderId}/customers/{customerId}"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){

			response <- httr::DELETE(glue::glue("https://management.api.umbrella.com/v1/serviceproviders/{serviceProviderId}/customers/{customerId}"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		httr::content(response)
	}


#' Update a Customer
#' 
#' @description Update a Customer
#' 
#' @param serviceProviderId The ServiceProviderID related to this Customer
#' @param customerId The customerID related to this Customer
#' @param customerName customer organization name
#' @param seats number of users
#' @param streetAddress street address
#' @param streetAddress2 street address2
#' @param city city
#' @param state state
#' @param countryCode country code
#' @param zipCode zip code
#' @param packageId package id
#' @param dealId deal id
#' @param adminEmails array of admin emails
#' @param ccwDealOwnerEmails array of CCW deal owner emails
#' @concept ManagementAPI
#' @export
	modifyCustomer <- function(serviceProviderId, customerId, customerName, seats, streetAddress, streetAddress2 = NULL, city, state, countryCode, zipCode = NULL, packageId, dealId = NULL, adminEmails, ccwDealOwnerEmails = NULL){

		params <- as.list(environment()) %>% purrr::compact()

		queryParams <- NULL

		bodyParamNames <- "customerName, seats, streetAddress, streetAddress2, city, state, countryCode, zipCode, packageId, dealId, adminEmails, ccwDealOwnerEmails"
		bodyParams <- params %>% purrr::keep(names(params) %in% (bodyParamNames %>% stringr::str_split(", ") %>% purrr::pluck(1)))

		api <- "ManagementAPI"

		authType <- "basic"

		response <- httr::PUT(glue::glue("https://management.api.umbrella.com/v1/serviceproviders/{serviceProviderId}/customers/{customerId}"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){

			response <- httr::PUT(glue::glue("https://management.api.umbrella.com/v1/serviceproviders/{serviceProviderId}/customers/{customerId}"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		httr::content(response)
	}


#' Return a list of management API Keys related to the customer
#' 
#' @description Return a list of management API Keys related to the customer
#' 
#' @param serviceProviderId The ServiceProviderID related to this Customer
#' @param customerId The customerID related to this Customer
#' @concept ManagementAPI
#' @export
	listApikeys <- function(serviceProviderId, customerId){

		params <- as.list(environment()) %>% purrr::compact()

		queryParams <- NULL

		bodyParams <- NULL

		api <- "ManagementAPI"

		authType <- "basic"

		response <- httr::GET(glue::glue("https://management.api.umbrella.com/v1/serviceproviders/{serviceProviderId}/customers/{customerId}/apikeys"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){

			response <- httr::GET(glue::glue("https://management.api.umbrella.com/v1/serviceproviders/{serviceProviderId}/customers/{customerId}/apikeys"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		httr::content(response)
	}


#' Create a management API Key for the customer
#' 
#' @description Create a management API Key for the customer
#' 
#' @param serviceProviderId The ServiceProviderID related to this Customer
#' @param customerId The customerID related to this Customer
#' @concept ManagementAPI
#' @export
	createApikey <- function(serviceProviderId, customerId){

		params <- as.list(environment()) %>% purrr::compact()

		queryParams <- NULL

		bodyParams <- NULL

		api <- "ManagementAPI"

		authType <- "basic"

		response <- httr::POST(glue::glue("https://management.api.umbrella.com/v1/serviceproviders/{serviceProviderId}/customers/{customerId}/apikeys"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){

			response <- httr::POST(glue::glue("https://management.api.umbrella.com/v1/serviceproviders/{serviceProviderId}/customers/{customerId}/apikeys"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		httr::content(response)
	}


#' Return a management API Key of the customer
#' 
#' @description Return a management API Key of the customer
#' 
#' @param serviceProviderId The ServiceProviderID related to this Customer
#' @param customerId The customerID related to this Customer
#' @param keyId The API key id
#' @concept ManagementAPI
#' @export
	getApikey <- function(serviceProviderId, customerId, keyId){

		params <- as.list(environment()) %>% purrr::compact()

		queryParams <- NULL

		bodyParams <- NULL

		api <- "ManagementAPI"

		authType <- "basic"

		response <- httr::GET(glue::glue("https://management.api.umbrella.com/v1/serviceproviders/{serviceProviderId}/customers/{customerId}/apikeys/{keyId}"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){

			response <- httr::GET(glue::glue("https://management.api.umbrella.com/v1/serviceproviders/{serviceProviderId}/customers/{customerId}/apikeys/{keyId}"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		httr::content(response)
	}


#' Update to generate a fresh management API key-secret for the customer
#' 
#' @description Update to generate a fresh management API key-secret for the customer
#' 
#' @param serviceProviderId The ServiceProviderID related to this Customer
#' @param customerId The customerID related to this Customer
#' @param keyId The existing API key id
#' @concept ManagementAPI
#' @export
	modifyApikey <- function(serviceProviderId, customerId, keyId){

		params <- as.list(environment()) %>% purrr::compact()

		queryParams <- NULL

		bodyParams <- NULL

		api <- "ManagementAPI"

		authType <- "basic"

		response <- httr::PUT(glue::glue("https://management.api.umbrella.com/v1/serviceproviders/{serviceProviderId}/customers/{customerId}/apikeys/{keyId}"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){

			response <- httr::PUT(glue::glue("https://management.api.umbrella.com/v1/serviceproviders/{serviceProviderId}/customers/{customerId}/apikeys/{keyId}"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		httr::content(response)
	}


#' Delete a management API key of the customer
#' 
#' @description Delete a management API key of the customer
#' 
#' @param serviceProviderId The ServiceProviderID related to this Customer
#' @param customerId The customerID related to this Customer
#' @param keyId The API key id
#' @concept ManagementAPI
#' @export
	deleteApikey <- function(serviceProviderId, customerId, keyId){

		params <- as.list(environment()) %>% purrr::compact()

		queryParams <- NULL

		bodyParams <- NULL

		api <- "ManagementAPI"

		authType <- "basic"

		response <- httr::DELETE(glue::glue("https://management.api.umbrella.com/v1/serviceproviders/{serviceProviderId}/customers/{customerId}/apikeys/{keyId}"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){

			response <- httr::DELETE(glue::glue("https://management.api.umbrella.com/v1/serviceproviders/{serviceProviderId}/customers/{customerId}/apikeys/{keyId}"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		httr::content(response)
	}


#' Create a new MspCustomer
#' 
#' @description Create a new MspCustomer
#' 
#' @param mspId The mspID related to this MspCustomer
#' @param customerName customer organization name
#' @param seats number of users
#' @concept ManagementAPI
#' @export
	createCustomerCustomers <- function(mspId, customerName, seats){

		params <- as.list(environment()) %>% purrr::compact()

		queryParams <- NULL

		bodyParamNames <- "customerName, seats"
		bodyParams <- params %>% purrr::keep(names(params) %in% (bodyParamNames %>% stringr::str_split(", ") %>% purrr::pluck(1)))

		api <- "ManagementAPI"

		authType <- "basic"

		response <- httr::POST(glue::glue("https://management.api.umbrella.com/v1/msps/{mspId}/customers"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){

			response <- httr::POST(glue::glue("https://management.api.umbrella.com/v1/msps/{mspId}/customers"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		httr::content(response)
	}


#' Return a list of MspCustomer
#' 
#' @description Return a list of MspCustomer
#' 
#' @param mspId The mspID related to this MspCustomer
#' @param limit Number of results to return for the given page number.
#' @param page Page number of results.
#' @concept ManagementAPI
#' @export
	listMspCustomers <- function(mspId, limit = 100, page = 1){

		params <- as.list(environment()) %>% purrr::compact()

		queryParamNames <- "limit, page"
		queryParams <- params %>% purrr::keep(names(params) %in% (queryParamNames %>% stringr::str_split(", ") %>% purrr::pluck(1)))

		bodyParams <- NULL

		api <- "ManagementAPI"

		authType <- "basic"

		response <- httr::GET(glue::glue("https://management.api.umbrella.com/v1/msps/{mspId}/customers"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){

			response <- httr::GET(glue::glue("https://management.api.umbrella.com/v1/msps/{mspId}/customers"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		httr::content(response)
	}


#' Return a MspCustomer
#' 
#' @description Return a MspCustomer
#' 
#' @param mspId The mspID related to this MspCustomer
#' @param customerId The customerID related to this MspCustomer
#' @concept ManagementAPI
#' @export
	getMspCustomers <- function(mspId, customerId){

		params <- as.list(environment()) %>% purrr::compact()

		queryParams <- NULL

		bodyParams <- NULL

		api <- "ManagementAPI"

		authType <- "basic"

		response <- httr::GET(glue::glue("https://management.api.umbrella.com/v1/msps/{mspId}/customers/{customerId}"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){

			response <- httr::GET(glue::glue("https://management.api.umbrella.com/v1/msps/{mspId}/customers/{customerId}"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		httr::content(response)
	}


#' Delete a MspCustomer
#' 
#' @description Delete a MspCustomer
#' 
#' @param mspId The mspID related to this MspCustomer
#' @param customerId The customerID related to this MspCustomer
#' @concept ManagementAPI
#' @export
	deleteMspCustomers <- function(mspId, customerId){

		params <- as.list(environment()) %>% purrr::compact()

		queryParams <- NULL

		bodyParams <- NULL

		api <- "ManagementAPI"

		authType <- "basic"

		response <- httr::DELETE(glue::glue("https://management.api.umbrella.com/v1/msps/{mspId}/customers/{customerId}"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){

			response <- httr::DELETE(glue::glue("https://management.api.umbrella.com/v1/msps/{mspId}/customers/{customerId}"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		httr::content(response)
	}


#' Update a MspCustomer
#' 
#' @description Update a MspCustomer
#' 
#' @param mspId The mspID related to this MspCustomer
#' @param customerId The customerID related to this MspCustomer
#' @param customerName customer organization name
#' @param seats number of users
#' @concept ManagementAPI
#' @export
	modifyMspCustomers <- function(mspId, customerId, customerName, seats){

		params <- as.list(environment()) %>% purrr::compact()

		queryParams <- NULL

		bodyParamNames <- "customerName, seats"
		bodyParams <- params %>% purrr::keep(names(params) %in% (bodyParamNames %>% stringr::str_split(", ") %>% purrr::pluck(1)))

		api <- "ManagementAPI"

		authType <- "basic"

		response <- httr::PUT(glue::glue("https://management.api.umbrella.com/v1/msps/{mspId}/customers/{customerId}"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){

			response <- httr::PUT(glue::glue("https://management.api.umbrella.com/v1/msps/{mspId}/customers/{customerId}"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		httr::content(response)
	}


#' Return a list of management API Keys related to the MspCustomer
#' 
#' @description Return a list of management API Keys related to the MspCustomer
#' 
#' @param mspId The mspID related to this MspCustomer
#' @param customerId The customerID related to this MspCustomer
#' @concept ManagementAPI
#' @export
	listCustomerApikeys <- function(mspId, customerId){

		params <- as.list(environment()) %>% purrr::compact()

		queryParams <- NULL

		bodyParams <- NULL

		api <- "ManagementAPI"

		authType <- "basic"

		response <- httr::GET(glue::glue("https://management.api.umbrella.com/v1/msps/{mspId}/customers/{customerId}/apikeys"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){

			response <- httr::GET(glue::glue("https://management.api.umbrella.com/v1/msps/{mspId}/customers/{customerId}/apikeys"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		httr::content(response)
	}


#' Create a management API Key for the MspCustomer
#' 
#' @description Create a management API Key for the MspCustomer
#' 
#' @param mspId The mspID related to this MspCustomer
#' @param customerId The customerID related to this MspCustomer
#' @concept ManagementAPI
#' @export
	createCustomerApikeys <- function(mspId, customerId){

		params <- as.list(environment()) %>% purrr::compact()

		queryParams <- NULL

		bodyParams <- NULL

		api <- "ManagementAPI"

		authType <- "basic"

		response <- httr::POST(glue::glue("https://management.api.umbrella.com/v1/msps/{mspId}/customers/{customerId}/apikeys"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){

			response <- httr::POST(glue::glue("https://management.api.umbrella.com/v1/msps/{mspId}/customers/{customerId}/apikeys"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		httr::content(response)
	}


#' Return a management API Key of the MspCustomer
#' 
#' @description Return a management API Key of the MspCustomer
#' 
#' @param mspId The mspID related to this MspCustomer
#' @param customerId The customerID related to this MspCustomer
#' @param keyId The API key id
#' @concept ManagementAPI
#' @export
	getCustomerApikeys <- function(mspId, customerId, keyId){

		params <- as.list(environment()) %>% purrr::compact()

		queryParams <- NULL

		bodyParams <- NULL

		api <- "ManagementAPI"

		authType <- "basic"

		response <- httr::GET(glue::glue("https://management.api.umbrella.com/v1/msps/{mspId}/customers/{customerId}/apikeys/{keyId}"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){

			response <- httr::GET(glue::glue("https://management.api.umbrella.com/v1/msps/{mspId}/customers/{customerId}/apikeys/{keyId}"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		httr::content(response)
	}


#' Update to generate a fresh management API key-secret for the MspCustomer
#' 
#' @description Update to generate a fresh management API key-secret for the MspCustomer
#' 
#' @param mspId The mspID related to this MspCustomer
#' @param customerId The customerID related to this MspCustomer
#' @param keyId The existing API key id
#' @concept ManagementAPI
#' @export
	modifyCustomerApikeys <- function(mspId, customerId, keyId){

		params <- as.list(environment()) %>% purrr::compact()

		queryParams <- NULL

		bodyParams <- NULL

		api <- "ManagementAPI"

		authType <- "basic"

		response <- httr::PUT(glue::glue("https://management.api.umbrella.com/v1/msps/{mspId}/customers/{customerId}/apikeys/{keyId}"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){

			response <- httr::PUT(glue::glue("https://management.api.umbrella.com/v1/msps/{mspId}/customers/{customerId}/apikeys/{keyId}"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		httr::content(response)
	}


#' Delete a management API key of the MspCustomer
#' 
#' @description Delete a management API key of the MspCustomer
#' 
#' @param mspId The mspID related to this MspCustomer
#' @param customerId The customerID related to this MspCustomer
#' @param keyId The API key id
#' @concept ManagementAPI
#' @export
	deleteCustomerApikeys <- function(mspId, customerId, keyId){

		params <- as.list(environment()) %>% purrr::compact()

		queryParams <- NULL

		bodyParams <- NULL

		api <- "ManagementAPI"

		authType <- "basic"

		response <- httr::DELETE(glue::glue("https://management.api.umbrella.com/v1/msps/{mspId}/customers/{customerId}/apikeys/{keyId}"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){

			response <- httr::DELETE(glue::glue("https://management.api.umbrella.com/v1/msps/{mspId}/customers/{customerId}/apikeys/{keyId}"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		httr::content(response)
	}


#' Create a new Network. You must first contact support to get your IP range verified before using the POST action to create a network. Reference this API when contacting support.
#' 
#' @description Create a new Network. You must first contact support to get your IP range verified before using the POST action to create a network. Reference this API when contacting support.
#' 
#' @param organizationId The organizationID related to this Network
#' @param name network name
#' @param ipAddress network ip address
#' @param prefixLength prefix length, must be greater than 28
#' @param isDynamic is dynamic ip
#' @param status status
#' @concept ManagementAPI
#' @export
	createNetwork <- function(name, ipAddress = NULL, prefixLength, isDynamic, status, organizationId = Sys.getenv("umbrellaOrganizationId")){

		params <- as.list(environment()) %>% purrr::compact()

		queryParams <- NULL

		bodyParamNames <- "name, ipAddress, prefixLength, isDynamic, status"
		bodyParams <- params %>% purrr::keep(names(params) %in% (bodyParamNames %>% stringr::str_split(", ") %>% purrr::pluck(1)))

		api <- "ManagementAPI"

		authType <- "basic"

		response <- httr::POST(glue::glue("https://management.api.umbrella.com/v1/organizations/{organizationId}/networks"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){

			response <- httr::POST(glue::glue("https://management.api.umbrella.com/v1/organizations/{organizationId}/networks"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		httr::content(response)
	}


#' Return a list of Networks
#' 
#' @description Return a list of Networks
#' 
#' @param organizationId The organizationID related to this Network
#' @param limit Number of results to return for the given page number.
#' @param page Page number of results.
#' @concept ManagementAPI
#' @export
	listNetworks <- function(limit = 100, page = 1, organizationId = Sys.getenv("umbrellaOrganizationId")){

		params <- as.list(environment()) %>% purrr::compact()

		queryParamNames <- "limit, page"
		queryParams <- params %>% purrr::keep(names(params) %in% (queryParamNames %>% stringr::str_split(", ") %>% purrr::pluck(1)))

		bodyParams <- NULL

		api <- "ManagementAPI"

		authType <- "basic"

		response <- httr::GET(glue::glue("https://management.api.umbrella.com/v1/organizations/{organizationId}/networks"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){

			response <- httr::GET(glue::glue("https://management.api.umbrella.com/v1/organizations/{organizationId}/networks"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		httr::content(response)
	}


#' Return a Network
#' 
#' @description Return a Network
#' 
#' @param organizationId The organizationID related to this Network
#' @param networkId The network id related to this Network
#' @concept ManagementAPI
#' @export
	getNetwork <- function(networkId, organizationId = Sys.getenv("umbrellaOrganizationId")){

		params <- as.list(environment()) %>% purrr::compact()

		queryParams <- NULL

		bodyParams <- NULL

		api <- "ManagementAPI"

		authType <- "basic"

		response <- httr::GET(glue::glue("https://management.api.umbrella.com/v1/organizations/{organizationId}/networks/{networkId}"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){

			response <- httr::GET(glue::glue("https://management.api.umbrella.com/v1/organizations/{organizationId}/networks/{networkId}"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		httr::content(response)
	}


#' Update a Network
#' 
#' @description Update a Network
#' 
#' @param organizationId The organizationID related to this Network
#' @param networkId The network id related to this Network
#' @param name network name
#' @param ipAddress network ip address
#' @param prefixLength prefix length, must be greater than 28
#' @param isDynamic is dynamic ip
#' @param status status
#' @concept ManagementAPI
#' @export
	modifyNetwork <- function(networkId, name, ipAddress = NULL, prefixLength = NULL, isDynamic, status, organizationId = Sys.getenv("umbrellaOrganizationId")){

		params <- as.list(environment()) %>% purrr::compact()

		queryParams <- NULL

		bodyParamNames <- "name, ipAddress, prefixLength, isDynamic, status"
		bodyParams <- params %>% purrr::keep(names(params) %in% (bodyParamNames %>% stringr::str_split(", ") %>% purrr::pluck(1)))

		api <- "ManagementAPI"

		authType <- "basic"

		response <- httr::PUT(glue::glue("https://management.api.umbrella.com/v1/organizations/{organizationId}/networks/{networkId}"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){

			response <- httr::PUT(glue::glue("https://management.api.umbrella.com/v1/organizations/{organizationId}/networks/{networkId}"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		httr::content(response)
	}


#' Delete a Network
#' 
#' @description Delete a Network
#' 
#' @param organizationId The organizationID related to this Network
#' @param networkId The network id related to this Network
#' @concept ManagementAPI
#' @export
	deleteNetwork <- function(networkId, organizationId = Sys.getenv("umbrellaOrganizationId")){

		params <- as.list(environment()) %>% purrr::compact()

		queryParams <- NULL

		bodyParams <- NULL

		api <- "ManagementAPI"

		authType <- "basic"

		response <- httr::DELETE(glue::glue("https://management.api.umbrella.com/v1/organizations/{organizationId}/networks/{networkId}"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){

			response <- httr::DELETE(glue::glue("https://management.api.umbrella.com/v1/organizations/{organizationId}/networks/{networkId}"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		httr::content(response)
	}


#' Return a list of Policies for a Network
#' 
#' @description Return a list of Policies for a Network
#' 
#' @param organizationId The organization id
#' @param networkId The network id
#' @param type 
#' @concept ManagementAPI
#' @export
	listPolicies <- function(networkId, type = NULL, organizationId = Sys.getenv("umbrellaOrganizationId")){

		params <- as.list(environment()) %>% purrr::compact()

		queryParamNames <- "type"
		queryParams <- params %>% purrr::keep(names(params) %in% (queryParamNames %>% stringr::str_split(", ") %>% purrr::pluck(1)))

		bodyParams <- NULL

		api <- "ManagementAPI"

		authType <- "basic"

		response <- httr::GET(glue::glue("https://management.api.umbrella.com/v1/organizations/{organizationId}/networks/{networkId}/policies"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){

			response <- httr::GET(glue::glue("https://management.api.umbrella.com/v1/organizations/{organizationId}/networks/{networkId}/policies"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		httr::content(response)
	}


#' Return a list of RoamingComputers
#' 
#' @description Return a list of RoamingComputers
#' 
#' @param organizationId The organizationID related to this RoamingComputer
#' @param limit Number of results to return for the given page number.
#' @param page Page number of results.
#' @param name 
#' @param status 
#' @param lastSyncBefore 
#' @param lastSyncAfter 
#' @concept ManagementAPI
#' @export
	listRoamingcomputers <- function(limit = 100, page = 1, name = NULL, status = NULL, lastSyncBefore = NULL, lastSyncAfter = NULL, organizationId = Sys.getenv("umbrellaOrganizationId")){

		params <- as.list(environment()) %>% purrr::compact()

		queryParamNames <- "limit, page, name, status, lastSyncBefore, lastSyncAfter"
		queryParams <- params %>% purrr::keep(names(params) %in% (queryParamNames %>% stringr::str_split(", ") %>% purrr::pluck(1)))

		bodyParams <- NULL

		api <- "ManagementAPI"

		authType <- "basic"

		response <- httr::GET(glue::glue("https://management.api.umbrella.com/v1/organizations/{organizationId}/roamingcomputers"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){

			response <- httr::GET(glue::glue("https://management.api.umbrella.com/v1/organizations/{organizationId}/roamingcomputers"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		httr::content(response)
	}


#' Return a RoamingComputer
#' 
#' @description Return a RoamingComputer
#' 
#' @param organizationId The organizationID related to this RoamingComputer
#' @param roamingComputerId The roamingComputerID related to this RoamingComputer
#' @concept ManagementAPI
#' @export
	getRoamingcomputer <- function(roamingComputerId, organizationId = Sys.getenv("umbrellaOrganizationId")){

		params <- as.list(environment()) %>% purrr::compact()

		queryParams <- NULL

		bodyParams <- NULL

		api <- "ManagementAPI"

		authType <- "basic"

		response <- httr::GET(glue::glue("https://management.api.umbrella.com/v1/organizations/{organizationId}/roamingcomputers/{roamingComputerId}"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){

			response <- httr::GET(glue::glue("https://management.api.umbrella.com/v1/organizations/{organizationId}/roamingcomputers/{roamingComputerId}"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		httr::content(response)
	}


#' Update a RoamingComputer
#' 
#' @description Update a RoamingComputer
#' 
#' @param organizationId The organizationID related to this RoamingComputer
#' @param roamingComputerId The roamingComputerID related to this RoamingComputer
#' @param name roaming computer name
#' @concept ManagementAPI
#' @export
	modifyRoamingcomputer <- function(roamingComputerId, name, organizationId = Sys.getenv("umbrellaOrganizationId")){

		params <- as.list(environment()) %>% purrr::compact()

		queryParams <- NULL

		bodyParamNames <- "name"
		bodyParams <- params %>% purrr::keep(names(params) %in% (bodyParamNames %>% stringr::str_split(", ") %>% purrr::pluck(1)))

		api <- "ManagementAPI"

		authType <- "basic"

		response <- httr::PUT(glue::glue("https://management.api.umbrella.com/v1/organizations/{organizationId}/roamingcomputers/{roamingComputerId}"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){

			response <- httr::PUT(glue::glue("https://management.api.umbrella.com/v1/organizations/{organizationId}/roamingcomputers/{roamingComputerId}"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		httr::content(response)
	}


#' Delete a RoamingComputer
#' 
#' @description Delete a RoamingComputer
#' 
#' @param organizationId The organizationID related to this RoamingComputer
#' @param roamingComputerId The roamingComputerID related to this RoamingComputer
#' @concept ManagementAPI
#' @export
	deleteRoamingcomputer <- function(roamingComputerId, organizationId = Sys.getenv("umbrellaOrganizationId")){

		params <- as.list(environment()) %>% purrr::compact()

		queryParams <- NULL

		bodyParams <- NULL

		api <- "ManagementAPI"

		authType <- "basic"

		response <- httr::DELETE(glue::glue("https://management.api.umbrella.com/v1/organizations/{organizationId}/roamingcomputers/{roamingComputerId}"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){

			response <- httr::DELETE(glue::glue("https://management.api.umbrella.com/v1/organizations/{organizationId}/roamingcomputers/{roamingComputerId}"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		httr::content(response)
	}


#' Create a new InternalNetwork
#' 
#' @description Create a new InternalNetwork
#' 
#' @param organizationId The organizationID related to this InternalNetwork
#' @param name internal network label
#' @param ipAddress internal network ipv4 address
#' @param prefixLength prefix length
#' @param siteId For DNS policies: ID of Site to be associated with Internal Network. (Provide only one of siteId, networkId, or tunnelId)
#' @param networkId For Web policies via proxy chaining: ID of Network to be associated with Internal Network. (Provide only one of siteId, networkId, or tunnelId)
#' @param tunnelId For Web policies via IPsec tunnel: ID of Tunnel to be associated with Internal Network. (Provide only one of siteId, networkId, or tunnelId)
#' @concept ManagementAPI
#' @export
	createInternalnetwork <- function(name, ipAddress, prefixLength, siteId = NULL, networkId = NULL, tunnelId = NULL, organizationId = Sys.getenv("umbrellaOrganizationId")){

		params <- as.list(environment()) %>% purrr::compact()

		queryParams <- NULL

		bodyParamNames <- "name, ipAddress, prefixLength, siteId, networkId, tunnelId"
		bodyParams <- params %>% purrr::keep(names(params) %in% (bodyParamNames %>% stringr::str_split(", ") %>% purrr::pluck(1)))

		api <- "ManagementAPI"

		authType <- "basic"

		response <- httr::POST(glue::glue("https://management.api.umbrella.com/v1/organizations/{organizationId}/internalnetworks"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){

			response <- httr::POST(glue::glue("https://management.api.umbrella.com/v1/organizations/{organizationId}/internalnetworks"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		httr::content(response)
	}


#' Return a list of InternalNetworks
#' 
#' @description Return a list of InternalNetworks
#' 
#' @param organizationId The organizationID related to this InternalNetwork
#' @param limit Number of results to return for the given page number.
#' @param page Page number of results.
#' @param name 
#' @concept ManagementAPI
#' @export
	listInternalnetworks <- function(limit = 100, page = 1, name = NULL, organizationId = Sys.getenv("umbrellaOrganizationId")){

		params <- as.list(environment()) %>% purrr::compact()

		queryParamNames <- "limit, page, name"
		queryParams <- params %>% purrr::keep(names(params) %in% (queryParamNames %>% stringr::str_split(", ") %>% purrr::pluck(1)))

		bodyParams <- NULL

		api <- "ManagementAPI"

		authType <- "basic"

		response <- httr::GET(glue::glue("https://management.api.umbrella.com/v1/organizations/{organizationId}/internalnetworks"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){

			response <- httr::GET(glue::glue("https://management.api.umbrella.com/v1/organizations/{organizationId}/internalnetworks"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		httr::content(response)
	}


#' Return an InternalNetwork
#' 
#' @description Return an InternalNetwork
#' 
#' @param organizationId The organizationID related to this InternalNetwork
#' @param internalNetworkId The internal network id related to this InternalNetwork
#' @concept ManagementAPI
#' @export
	getInternalnetwork <- function(internalNetworkId, organizationId = Sys.getenv("umbrellaOrganizationId")){

		params <- as.list(environment()) %>% purrr::compact()

		queryParams <- NULL

		bodyParams <- NULL

		api <- "ManagementAPI"

		authType <- "basic"

		response <- httr::GET(glue::glue("https://management.api.umbrella.com/v1/organizations/{organizationId}/internalnetworks/{internalNetworkId}"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){

			response <- httr::GET(glue::glue("https://management.api.umbrella.com/v1/organizations/{organizationId}/internalnetworks/{internalNetworkId}"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		httr::content(response)
	}


#' Update an InternalNetwork
#' 
#' @description Update an InternalNetwork
#' 
#' @param organizationId The organizationID related to this InternalNetwork
#' @param internalNetworkId The internal network id related to this InternalNetwork
#' @param name internal network label
#' @param ipAddress internal network ipv4 address
#' @param prefixLength prefix length
#' @param siteId For DNS policies: ID of Site to be associated with Internal Network. (Provide only one of siteId, networkId, or tunnelId)
#' @param networkId For Web policies via proxy chaining: ID of Network to be associated with Internal Network. (Provide only one of siteId, networkId, or tunnelId)
#' @param tunnelId For Web policies via IPsec tunnel: ID of Tunnel to be associated with Internal Network. (Provide only one of siteId, networkId, or tunnelId)
#' @concept ManagementAPI
#' @export
	modifyInternalnetwork <- function(internalNetworkId, name, ipAddress, prefixLength, siteId = NULL, networkId = NULL, tunnelId = NULL, organizationId = Sys.getenv("umbrellaOrganizationId")){

		params <- as.list(environment()) %>% purrr::compact()

		queryParams <- NULL

		bodyParamNames <- "name, ipAddress, prefixLength, siteId, networkId, tunnelId"
		bodyParams <- params %>% purrr::keep(names(params) %in% (bodyParamNames %>% stringr::str_split(", ") %>% purrr::pluck(1)))

		api <- "ManagementAPI"

		authType <- "basic"

		response <- httr::PUT(glue::glue("https://management.api.umbrella.com/v1/organizations/{organizationId}/internalnetworks/{internalNetworkId}"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){

			response <- httr::PUT(glue::glue("https://management.api.umbrella.com/v1/organizations/{organizationId}/internalnetworks/{internalNetworkId}"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		httr::content(response)
	}


#' Delete an internalNetwork
#' 
#' @description Delete an internalNetwork
#' 
#' @param organizationId The organizationID related to this InternalNetwork
#' @param internalNetworkId The internal network id related to this InternalNetwork
#' @concept ManagementAPI
#' @export
	deleteInternalnetwork <- function(internalNetworkId, organizationId = Sys.getenv("umbrellaOrganizationId")){

		params <- as.list(environment()) %>% purrr::compact()

		queryParams <- NULL

		bodyParams <- NULL

		api <- "ManagementAPI"

		authType <- "basic"

		response <- httr::DELETE(glue::glue("https://management.api.umbrella.com/v1/organizations/{organizationId}/internalnetworks/{internalNetworkId}"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){

			response <- httr::DELETE(glue::glue("https://management.api.umbrella.com/v1/organizations/{organizationId}/internalnetworks/{internalNetworkId}"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		httr::content(response)
	}


#' Return a list of Policies for an Internal Network
#' 
#' @description Return a list of Policies for an Internal Network
#' 
#' @param organizationId The organization id
#' @param internalNetworkId The internal network id
#' @param type 
#' @concept ManagementAPI
#' @export
	listInternalnetworkPolicies <- function(internalNetworkId, type = NULL, organizationId = Sys.getenv("umbrellaOrganizationId")){

		params <- as.list(environment()) %>% purrr::compact()

		queryParamNames <- "type"
		queryParams <- params %>% purrr::keep(names(params) %in% (queryParamNames %>% stringr::str_split(", ") %>% purrr::pluck(1)))

		bodyParams <- NULL

		api <- "ManagementAPI"

		authType <- "basic"

		response <- httr::GET(glue::glue("https://management.api.umbrella.com/v1/organizations/{organizationId}/internalnetworks/{internalNetworkId}/policies"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){

			response <- httr::GET(glue::glue("https://management.api.umbrella.com/v1/organizations/{organizationId}/internalnetworks/{internalNetworkId}/policies"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		httr::content(response)
	}


#' Create a new InternalDomain
#' 
#' @description Create a new InternalDomain
#' 
#' @param organizationId The organization id related to this InternalDomain
#' @param domain internal domain
#' @param description internal domain description
#' @param includeAllVAs apply internal domain to all virtual appliances
#' @param includeAllMobileDevices apply internal domain to all mobile devices
#' @concept ManagementAPI
#' @export
	createInternaldomain <- function(domain, description = NULL, includeAllVAs = NULL, includeAllMobileDevices = NULL, organizationId = Sys.getenv("umbrellaOrganizationId")){

		params <- as.list(environment()) %>% purrr::compact()

		queryParams <- NULL

		bodyParamNames <- "domain, description, includeAllVAs, includeAllMobileDevices"
		bodyParams <- params %>% purrr::keep(names(params) %in% (bodyParamNames %>% stringr::str_split(", ") %>% purrr::pluck(1)))

		api <- "ManagementAPI"

		authType <- "basic"

		response <- httr::POST(glue::glue("https://management.api.umbrella.com/v1/organizations/{organizationId}/internaldomains"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){

			response <- httr::POST(glue::glue("https://management.api.umbrella.com/v1/organizations/{organizationId}/internaldomains"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		httr::content(response)
	}


#' Return a list of InternalDomains
#' 
#' @description Return a list of InternalDomains
#' 
#' @param organizationId The organization id related to this InternalDomain
#' @param limit Number of results to return for the given page number.
#' @param page Page number of results.
#' @concept ManagementAPI
#' @export
	listInternaldomains <- function(limit = 100, page = 1, organizationId = Sys.getenv("umbrellaOrganizationId")){

		params <- as.list(environment()) %>% purrr::compact()

		queryParamNames <- "limit, page"
		queryParams <- params %>% purrr::keep(names(params) %in% (queryParamNames %>% stringr::str_split(", ") %>% purrr::pluck(1)))

		bodyParams <- NULL

		api <- "ManagementAPI"

		authType <- "basic"

		response <- httr::GET(glue::glue("https://management.api.umbrella.com/v1/organizations/{organizationId}/internaldomains"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){

			response <- httr::GET(glue::glue("https://management.api.umbrella.com/v1/organizations/{organizationId}/internaldomains"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		httr::content(response)
	}


#' Return an InternalDomain
#' 
#' @description Return an InternalDomain
#' 
#' @param organizationId The organization id related to this InternalDomain
#' @param internalDomainId The internal domain id related to this InternalDomain
#' @concept ManagementAPI
#' @export
	getInternaldomain <- function(internalDomainId, organizationId = Sys.getenv("umbrellaOrganizationId")){

		params <- as.list(environment()) %>% purrr::compact()

		queryParams <- NULL

		bodyParams <- NULL

		api <- "ManagementAPI"

		authType <- "basic"

		response <- httr::GET(glue::glue("https://management.api.umbrella.com/v1/organizations/{organizationId}/internaldomains/{internalDomainId}"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){

			response <- httr::GET(glue::glue("https://management.api.umbrella.com/v1/organizations/{organizationId}/internaldomains/{internalDomainId}"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		httr::content(response)
	}


#' Update an InternalDomain
#' 
#' @description Update an InternalDomain
#' 
#' @param organizationId The organization id related to this InternalDomain
#' @param internalDomainId The internal domain id related to this InternalDomain
#' @param domain internal domain
#' @param description internal domain description
#' @param includeAllVAs apply internal domain to all virtual appliances
#' @param includeAllMobileDevices apply internal domain to all mobile devices
#' @concept ManagementAPI
#' @export
	modifyInternaldomain <- function(internalDomainId, domain, description = NULL, includeAllVAs = NULL, includeAllMobileDevices = NULL, organizationId = Sys.getenv("umbrellaOrganizationId")){

		params <- as.list(environment()) %>% purrr::compact()

		queryParams <- NULL

		bodyParamNames <- "domain, description, includeAllVAs, includeAllMobileDevices"
		bodyParams <- params %>% purrr::keep(names(params) %in% (bodyParamNames %>% stringr::str_split(", ") %>% purrr::pluck(1)))

		api <- "ManagementAPI"

		authType <- "basic"

		response <- httr::PUT(glue::glue("https://management.api.umbrella.com/v1/organizations/{organizationId}/internaldomains/{internalDomainId}"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){

			response <- httr::PUT(glue::glue("https://management.api.umbrella.com/v1/organizations/{organizationId}/internaldomains/{internalDomainId}"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		httr::content(response)
	}


#' Delete an internalDomain
#' 
#' @description Delete an internalDomain
#' 
#' @param organizationId The organization id related to this InternalDomain
#' @param internalDomainId The internal domain id related to this InternalDomain
#' @concept ManagementAPI
#' @export
	deleteInternaldomain <- function(internalDomainId, organizationId = Sys.getenv("umbrellaOrganizationId")){

		params <- as.list(environment()) %>% purrr::compact()

		queryParams <- NULL

		bodyParams <- NULL

		api <- "ManagementAPI"

		authType <- "basic"

		response <- httr::DELETE(glue::glue("https://management.api.umbrella.com/v1/organizations/{organizationId}/internaldomains/{internalDomainId}"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){

			response <- httr::DELETE(glue::glue("https://management.api.umbrella.com/v1/organizations/{organizationId}/internaldomains/{internalDomainId}"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		httr::content(response)
	}


#' Return a list of VirtualAppliances
#' 
#' @description Return a list of VirtualAppliances
#' 
#' @param organizationId The organizationID related to this VirtualAppliance
#' @param limit Number of results to return for the given page number.
#' @param page Page number of results.
#' @concept ManagementAPI
#' @export
	listVirtualappliances <- function(limit = 100, page = 1, organizationId = Sys.getenv("umbrellaOrganizationId")){

		params <- as.list(environment()) %>% purrr::compact()

		queryParamNames <- "limit, page"
		queryParams <- params %>% purrr::keep(names(params) %in% (queryParamNames %>% stringr::str_split(", ") %>% purrr::pluck(1)))

		bodyParams <- NULL

		api <- "ManagementAPI"

		authType <- "basic"

		response <- httr::GET(glue::glue("https://management.api.umbrella.com/v1/organizations/{organizationId}/virtualappliances"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){

			response <- httr::GET(glue::glue("https://management.api.umbrella.com/v1/organizations/{organizationId}/virtualappliances"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		httr::content(response)
	}


#' Return a VirtualAppliance
#' 
#' @description Return a VirtualAppliance
#' 
#' @param organizationId The organizationID related to this VirtualAppliance
#' @param virtualApplianceId The virtual appliance id related to this VirtualAppliance
#' @concept ManagementAPI
#' @export
	getVirtualappliance <- function(virtualApplianceId, organizationId = Sys.getenv("umbrellaOrganizationId")){

		params <- as.list(environment()) %>% purrr::compact()

		queryParams <- NULL

		bodyParams <- NULL

		api <- "ManagementAPI"

		authType <- "basic"

		response <- httr::GET(glue::glue("https://management.api.umbrella.com/v1/organizations/{organizationId}/virtualappliances/{virtualApplianceId}"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){

			response <- httr::GET(glue::glue("https://management.api.umbrella.com/v1/organizations/{organizationId}/virtualappliances/{virtualApplianceId}"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		httr::content(response)
	}


#' Update a VirtualAppliance
#' 
#' @description Update a VirtualAppliance
#' 
#' @param organizationId The organizationID related to this VirtualAppliance
#' @param virtualApplianceId The virtual appliance id related to this VirtualAppliance
#' @param siteId site id
#' @concept ManagementAPI
#' @export
	modifyVirtualappliance <- function(virtualApplianceId, siteId, organizationId = Sys.getenv("umbrellaOrganizationId")){

		params <- as.list(environment()) %>% purrr::compact()

		queryParams <- NULL

		bodyParamNames <- "siteId"
		bodyParams <- params %>% purrr::keep(names(params) %in% (bodyParamNames %>% stringr::str_split(", ") %>% purrr::pluck(1)))

		api <- "ManagementAPI"

		authType <- "basic"

		response <- httr::PUT(glue::glue("https://management.api.umbrella.com/v1/organizations/{organizationId}/virtualappliances/{virtualApplianceId}"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){

			response <- httr::PUT(glue::glue("https://management.api.umbrella.com/v1/organizations/{organizationId}/virtualappliances/{virtualApplianceId}"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		httr::content(response)
	}


#' Delete a VirtualAppliance
#' 
#' @description Delete a VirtualAppliance
#' 
#' @param organizationId The organizationID related to this VirtualAppliance
#' @param virtualApplianceId The virtual appliance id related to this VirtualAppliance
#' @concept ManagementAPI
#' @export
	deleteVirtualappliance <- function(virtualApplianceId, organizationId = Sys.getenv("umbrellaOrganizationId")){

		params <- as.list(environment()) %>% purrr::compact()

		queryParams <- NULL

		bodyParams <- NULL

		api <- "ManagementAPI"

		authType <- "basic"

		response <- httr::DELETE(glue::glue("https://management.api.umbrella.com/v1/organizations/{organizationId}/virtualappliances/{virtualApplianceId}"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){

			response <- httr::DELETE(glue::glue("https://management.api.umbrella.com/v1/organizations/{organizationId}/virtualappliances/{virtualApplianceId}"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		httr::content(response)
	}


#' Create a new Site
#' 
#' @description Create a new Site
#' 
#' @param organizationId The organizationID related to this Site
#' @param name site name
#' @concept ManagementAPI
#' @export
	createSite <- function(name, organizationId = Sys.getenv("umbrellaOrganizationId")){

		params <- as.list(environment()) %>% purrr::compact()

		queryParams <- NULL

		bodyParamNames <- "name"
		bodyParams <- params %>% purrr::keep(names(params) %in% (bodyParamNames %>% stringr::str_split(", ") %>% purrr::pluck(1)))

		api <- "ManagementAPI"

		authType <- "basic"

		response <- httr::POST(glue::glue("https://management.api.umbrella.com/v1/organizations/{organizationId}/sites"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){

			response <- httr::POST(glue::glue("https://management.api.umbrella.com/v1/organizations/{organizationId}/sites"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		httr::content(response)
	}


#' Return a list of Sites
#' 
#' @description Return a list of Sites
#' 
#' @param organizationId The organizationID related to this Site
#' @param limit Number of results to return for the given page number.
#' @param page Page number of results.
#' @concept ManagementAPI
#' @export
	listSites <- function(limit = 100, page = 1, organizationId = Sys.getenv("umbrellaOrganizationId")){

		params <- as.list(environment()) %>% purrr::compact()

		queryParamNames <- "limit, page"
		queryParams <- params %>% purrr::keep(names(params) %in% (queryParamNames %>% stringr::str_split(", ") %>% purrr::pluck(1)))

		bodyParams <- NULL

		api <- "ManagementAPI"

		authType <- "basic"

		response <- httr::GET(glue::glue("https://management.api.umbrella.com/v1/organizations/{organizationId}/sites"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){

			response <- httr::GET(glue::glue("https://management.api.umbrella.com/v1/organizations/{organizationId}/sites"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		httr::content(response)
	}


#' Return a Site
#' 
#' @description Return a Site
#' 
#' @param organizationId The organizationID related to this Site
#' @param siteId The siteID related to this Site
#' @concept ManagementAPI
#' @export
	getSite <- function(siteId, organizationId = Sys.getenv("umbrellaOrganizationId")){

		params <- as.list(environment()) %>% purrr::compact()

		queryParams <- NULL

		bodyParams <- NULL

		api <- "ManagementAPI"

		authType <- "basic"

		response <- httr::GET(glue::glue("https://management.api.umbrella.com/v1/organizations/{organizationId}/sites/{siteId}"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){

			response <- httr::GET(glue::glue("https://management.api.umbrella.com/v1/organizations/{organizationId}/sites/{siteId}"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		httr::content(response)
	}


#' Update a Site
#' 
#' @description Update a Site
#' 
#' @param organizationId The organizationID related to this Site
#' @param siteId The siteID related to this Site
#' @param name site name
#' @concept ManagementAPI
#' @export
	modifySite <- function(siteId, name, organizationId = Sys.getenv("umbrellaOrganizationId")){

		params <- as.list(environment()) %>% purrr::compact()

		queryParams <- NULL

		bodyParamNames <- "name"
		bodyParams <- params %>% purrr::keep(names(params) %in% (bodyParamNames %>% stringr::str_split(", ") %>% purrr::pluck(1)))

		api <- "ManagementAPI"

		authType <- "basic"

		response <- httr::PUT(glue::glue("https://management.api.umbrella.com/v1/organizations/{organizationId}/sites/{siteId}"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){

			response <- httr::PUT(glue::glue("https://management.api.umbrella.com/v1/organizations/{organizationId}/sites/{siteId}"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		httr::content(response)
	}


#' Delete a Site
#' 
#' @description Delete a Site
#' 
#' @param organizationId The organizationID related to this Site
#' @param siteId The siteID related to this Site
#' @concept ManagementAPI
#' @export
	deleteSite <- function(siteId, organizationId = Sys.getenv("umbrellaOrganizationId")){

		params <- as.list(environment()) %>% purrr::compact()

		queryParams <- NULL

		bodyParams <- NULL

		api <- "ManagementAPI"

		authType <- "basic"

		response <- httr::DELETE(glue::glue("https://management.api.umbrella.com/v1/organizations/{organizationId}/sites/{siteId}"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){

			response <- httr::DELETE(glue::glue("https://management.api.umbrella.com/v1/organizations/{organizationId}/sites/{siteId}"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		httr::content(response)
	}


#' Create a new User
#' 
#' @description Create a new User
#' 
#' @param organizationId The organizationID related to this User
#' @param firstname user first name
#' @param lastname user last name
#' @param email user email
#' @param password user password
#' @param roleId role id
#' @param timezone user timezone name
#' @concept ManagementAPI
#' @export
	createUser <- function(firstname, lastname, email, password, roleId, timezone, organizationId = Sys.getenv("umbrellaOrganizationId")){

		params <- as.list(environment()) %>% purrr::compact()

		queryParams <- NULL

		bodyParamNames <- "firstname, lastname, email, password, roleId, timezone"
		bodyParams <- params %>% purrr::keep(names(params) %in% (bodyParamNames %>% stringr::str_split(", ") %>% purrr::pluck(1)))

		api <- "ManagementAPI"

		authType <- "basic"

		response <- httr::POST(glue::glue("https://management.api.umbrella.com/v1/organizations/{organizationId}/users"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){

			response <- httr::POST(glue::glue("https://management.api.umbrella.com/v1/organizations/{organizationId}/users"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		httr::content(response)
	}


#' Return a list of Users
#' 
#' @description Return a list of Users
#' 
#' @param organizationId The organizationID related to this User
#' @param limit Number of results to return for the given page number.
#' @param page Page number of results.
#' @concept ManagementAPI
#' @export
	listUsers <- function(limit = 100, page = 1, organizationId = Sys.getenv("umbrellaOrganizationId")){

		params <- as.list(environment()) %>% purrr::compact()

		queryParamNames <- "limit, page"
		queryParams <- params %>% purrr::keep(names(params) %in% (queryParamNames %>% stringr::str_split(", ") %>% purrr::pluck(1)))

		bodyParams <- NULL

		api <- "ManagementAPI"

		authType <- "basic"

		response <- httr::GET(glue::glue("https://management.api.umbrella.com/v1/organizations/{organizationId}/users"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){

			response <- httr::GET(glue::glue("https://management.api.umbrella.com/v1/organizations/{organizationId}/users"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		httr::content(response)
	}


#' Return a User
#' 
#' @description Return a User
#' 
#' @param organizationId The organizationID related to this User
#' @param userId The userID related to this User
#' @concept ManagementAPI
#' @export
	getUser <- function(userId, organizationId = Sys.getenv("umbrellaOrganizationId")){

		params <- as.list(environment()) %>% purrr::compact()

		queryParams <- NULL

		bodyParams <- NULL

		api <- "ManagementAPI"

		authType <- "basic"

		response <- httr::GET(glue::glue("https://management.api.umbrella.com/v1/organizations/{organizationId}/users/{userId}"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){

			response <- httr::GET(glue::glue("https://management.api.umbrella.com/v1/organizations/{organizationId}/users/{userId}"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		httr::content(response)
	}


#' Delete a User
#' 
#' @description Delete a User
#' 
#' @param organizationId The organizationID related to this User
#' @param userId The userID related to this User
#' @concept ManagementAPI
#' @export
	deleteUser <- function(userId, organizationId = Sys.getenv("umbrellaOrganizationId")){

		params <- as.list(environment()) %>% purrr::compact()

		queryParams <- NULL

		bodyParams <- NULL

		api <- "ManagementAPI"

		authType <- "basic"

		response <- httr::DELETE(glue::glue("https://management.api.umbrella.com/v1/organizations/{organizationId}/users/{userId}"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){

			response <- httr::DELETE(glue::glue("https://management.api.umbrella.com/v1/organizations/{organizationId}/users/{userId}"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		httr::content(response)
	}


#' Return a list of Roles
#' 
#' @description Return a list of Roles
#' 
#' @param organizationId The organizationID related to this Role
#' @concept ManagementAPI
#' @export
	listRoles <- function(organizationId = Sys.getenv("umbrellaOrganizationId")){

		params <- as.list(environment()) %>% purrr::compact()

		queryParams <- NULL

		bodyParams <- NULL

		api <- "ManagementAPI"

		authType <- "basic"

		response <- httr::GET(glue::glue("https://management.api.umbrella.com/v1/organizations/{organizationId}/roles"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = F)))

		if(response$status_code == 401){

			response <- httr::GET(glue::glue("https://management.api.umbrella.com/v1/organizations/{organizationId}/roles"), query = queryParams, body = bodyParams, encode = "json", httr::content_type_json(), httr::add_headers(.headers = getAuthHeader(api, authType, refresh = T)))
		}

		if(response$status_code > 300) return(response)

		httr::content(response)
	}
