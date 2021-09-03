# BNP apartment geocoding 
library(urltools)
library(httr)
library(ggmap)
library(gmapsdistance)
library(tidyverse)
library(stringr)
library(data.table)
library(ggplot2)
library(jsonlite)
library(htmltools)
library(rvest)

load("keys.data")

urlgeo <- "https://maps.googleapis.com/maps/api/geocode/json?address=embassy%20haven%20rt%20nagar%20bengaluru&key=ABC"


# for a single address
# pass a browser ready address with + sign separators

build_addr_url <- function(x,baseurl=urlgeo){
    url_shell <- parse_url(urlgeo)
    url_shell$query <-  list(address = x,key = apikey)
    build_url(url_shell)
}

# pass an compressed address vector - uses the above function for multiple inputs
build_all_googleapi_urls <- function(addr_vect) addr_vect %>% map(build_addr_url)

# pass a vector of urls as obtained from above
fire_api <- function(x) x %>% map(~GET(url = .x))

    
# pass the list of json (API output) content downloaded from the google api
pullstuffout <- function(apioutput){
pullone <- function(x){
    cont <- content(x)
    maxlen <- length(cont$results[[1]]$address_components)
    data.table(
        placetype=cont$results[[1]]$types %>% as.character() %>% paste(collapse = "/"),
        name = cont$results[[1]]$address_components[[1]]$long_name,
        lat = cont$results[[1]]$geometry$location$lat %>% as.numeric(),
        lng =  cont$results[[1]]$geometry$location$lng %>% as.numeric(),
        loctype = cont$geometry$location_type,
        subloc1 = cont$results[[1]]$address_components[[3]]$long_name,
        subloc2 = cont$results[[1]]$address_components[[2]]$long_name,
        landmark = cont$results[[1]]$address_components %>% 
            map_chr(~.x$types %>% paste(collapse = "/")) %>% 
            str_detect("landmark") %>% 
            {cont$results[[1]]$address_components %>% 
                    map_chr(~.x$long_name) %>% ifelse(length(.)==0,NA,.)}[.],
        pincode = cont$results[[1]]$address_components[[maxlen]]$long_name,
        formatted   = cont$results[[1]]$formatted_address,
        addrlen = maxlen
        )
}
#loop the above function across each json object
apioutput %>%  map(pullone) %>% rbindlist 
}

# pull out ward number from BPAC site

which_ward <- function(lat=77,lon=12){
    base <- 'https://bpac.in/address.php?logi=77.6060171&lati=12.9344168'
    ulist <- parse_url(url = base)
    ulist$query = list(logi = lon,lati=lat)
    x1 <- GET(build_url(ulist))
    content(x1,type = 'text/xml',encoding = 'UTF-8') %>% xml2::as_list(.) -> x1.list
    x1.list[[1]][[2]]$p[[1]]
}

output96

