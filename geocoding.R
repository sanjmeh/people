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
library(googlesheets4)
library(readxl)

source('addrclust.R')

# Prerequisites

# Create a google cloud platform project and obtain an API key
# Enable the API for geocodes on googleconsole  https://console.cloud.google.com/
# assign the API KEY in a variable named apikey and save it in a file named 'keys.data' using save() function.


load("keys.data")

# aptdt <-  fread("community_names.csv")
data1 <- read_excel('Data_1.xlsx')
setnames(data1,c("sn","area","address"))

# Please ensure the function compress_addr is sourced from another source file
# this till add several columns to the input file. If the column name for apartment name is different paste it in the argument
# aptdt %<>% compress_addr(colname = 'Community Name')

#data1 %>% compress_addr(colname = 'addr') -> data1c


# pass the compressed address vector to get as output the urls ready for firing
build_googleapi_urls <- function(addr_vect = aptdt$google_addr){
    build_addr_url <- function(x = aptdt$google_addr){
        urlgeo <- "https://maps.googleapis.com/maps/api/geocode/json?address=embassy%20haven%20rt%20nagar%20bengaluru&key=ABC"
        url_shell <- parse_url(urlgeo)
        url_shell$query <-  list(address = x,key = apikey)
        build_url(url_shell)
    }
    # Loop the function across all address vetors
    addr_vect %>% map(build_addr_url)  
} 

# this is the main function
# pass a vector of urls as obtained from above - this will take time (~ 1 second per url), so restrict to 100 urls at a time
# save the output in a variable
fire_api <- function(x) x %>% map(~GET(url = .x))

    
# pass the list of json objects (API output from fire_api() -  content downloaded from google maps and save the output in another variable
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

# pull out ward number from BPAC site - this is pretty fast - save the output in a variable
which_ward <- function(lat=77,lon=12){
    base <- 'https://bpac.in/address.php?logi=77.6060171&lati=12.9344168'
    ulist <- parse_url(url = base)
    ulist$query = list(logi = lon,lati=lat)
    x1 <- GET(build_url(ulist))
    content(x1,type = 'text/xml',encoding = 'UTF-8') %>% xml2::as_list(.) -> x1.list
    x1.list[[1]][[2]]$p[[1]]
}



# add the wards obtained from the function which_ward() as a new column in the data.table variable and that is the final output
# ensure that the length of the ward vector is the same as nrow() of the data.table
