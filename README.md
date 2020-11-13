# umbrella

This R package contains scripts for consuming Cisco Umbrella's Destination Gateway, Management, Reporting, and Umbrella Management APIs.

## Install

```r
remotes::install_github("samterfa/umbrella")
```

## Getting Started

Navigate to https://login.umbrella.com/ to login to your Umbrella dashboard. Your Organization Id will be the number in the URL of the landing page. (e.g. https://dashboard.umbrella.com/o/{YourOrganizationId}/#/overview)

Next, navigate to https://dashboard.umbrella.com/o/{YourOrganizationId}/#/admin/apikeys to obtain API keys and secrets for use with Umbrella's APIs.

## Usage

Begin by setting relevant environmental variables including your Organization ID and API Key and Secret for each API you want to use.

```r
Sys.setenv(umbrellaOrganizationId = {YourOrganizationId})

Sys.setenv(umbrellaManagementAPIkey = {YourManagementAPIkey})
Sys.setenv(umbrellaManagementAPIsecret = {YourManagementAPIsecret})

Sys.setenv(umbrellaDestinationAPIGatewayKey = {YourDestinationAPIGatewayKey})
Sys.setenv(umbrellaDestinationAPIGatewaySecret = {YourDestinationAPIGatewaySecret})

Sys.setenv(umbrellaUmbrellaManagementAPIkey = {YourUmbrellaManagementAPIkey})
Sys.setenv(umbrellaUmbrellaManagementAPIsecret = {YourUmbrellaManagementAPIsecret})

Sys.setenv(umbrellaReportingAPIkey = {YourReportingAPIkey})
Sys.setenv(umbrellaReportingAPIsecret = {YourReportingAPIsecret})
```

Or, even better, place the above in a .Renviron file for use in an R project.

Then make a call!

```r
umbrella::listOrganizations()

#>[[1]]
#>[[1]]$organizationId
#> [1] "1234567"
#>
#>[[1]]$name
#> [1] "ACME Inc."

umbrella::listIdentities() %>% purrr::pluck('data') %>% purrr::map(~.x$type$type) %>% unlist() %>% table()

#> ad_connector directory_computer   directory_domain    directory_group     directory_user  domain_controller 
#>       1              1210                1                  258                1105              6 
#>  internal_network      mobile_device            network            roaming               site 
#>        15                  1427                    2                  72                   1 
```

## Resources

Function documentation for this package is available at https://samterfa.github.io/umbrella/reference/index.html.

See [Cisco Umbrella API Documentation](https://docs.umbrella.com/umbrella-api/reference) for details on the various APIs available.
