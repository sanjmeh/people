# places API
library(data.table)
library(httr)
library(ggmap)
library(tidyverse)
library(tidyr)
library(splitstackshape)
library(geosphere)
library(RColorBrewer)
library(magrittr)
library(lubridate)
library(googlesheets4)
library(googledrive)
library(janitor)
library(sf)
ward_kmlfile <- "BBMP-Wards-Map.kml"
apikey <- read_lines("apikey_srikanth.txt")
register_google(apikey)
list_of_keywords <- c("Appartment","Layout","Apartment", 
                      "Apartment+Complex","Apartment+Building","Enclave","Housing",
                      "Villa","Residential","Residences","Homes","Condo")
allwards <- read_lines("prio_wards.txt") %>% str_split_fixed(pattern = ",",n = 20) %>% as.numeric()

cvr <- c(12.97068, 77.70612)
salarp <- c(12.991515009730783, 77.65998578609168)
cvrpo <- c(12.985763261256892, 77.66217924286023)
pin87 <- c(12.924203515818235, 77.7414835253186)
belandur <- c(12.92287,77.68021)
keerti <- c(12.991906401224941, 77.6590958208496)
bbox_cvraman <- c(12.958557, 77.644929, 12.995609,77.701148) # bounding box SW and NE
cvr <- c(12.97068, 77.70612)
salarp <- c(12.991515009730783, 77.65998578609168)
salarp1 <- c(12.99142115509311, 77.6596855918011)
cvrpo <- c(12.985763261256892, 77.66217924286023)
perkm_lng <- (bbox_cvraman[4] - bbox_cvraman[2])/6.1
perkm_lat <- (bbox_cvraman[3] - bbox_cvraman[1])/4.1
belandur <- c(12.92287,77.68021)
keerti <- c(12.991906401224941, 77.6590958208496)

s1 <- st_read("BBMP-Wards-Map.kml")
s1_geom <- st_geometry(s1)
dt_centroids <- 1:198 %>% 
    map(~s1_geom[[.x]] %>% 
            st_centroid %>% 
            as.numeric) %>% 
    unlist %>% matrix(ncol=2,byrow = T) %>% as.data.table  %>% setnames(c("lng","lat"))

dt_centroids[,ward:=find_wards(dt_centroids)]

cen <- function(wardno = 174) dt_centroids[ward == wardno,.(lat,lng)] %>% as.numeric()

# operator to check inclusion of a real number in another set of real numbers with a small tolerance
`%~%` <- function(x,y) {
    out <- logical(length(x))
    for(i in 1:length(x)) out[i] <- any(abs(x[i] - y) <= 1e-6)
    out
}

# another function to check match of a real number vector with any element of
# another real number vector with a 10 decimal rounding, to account for
# tolerance
fselect_in <- function(x, ref, d = 10){
    round(x, digits=d) %in% round(ref, digits=d)
}

# load static maps: run this function only once to conserve API calls
load_static_maps <- function(){
    map_full_blr11 <<- get_map(location = "Bangalore", zoom = 11)
    map_full_blr12 <<- get_map(location = "Bangalore", zoom = 12)
    map_blr174 <<- get_map(location = rev(cen(174)), zoom = 14)
    # map66_15 <<- get_map(location = "Subramanya nagar, Bengaluru",zoom = 15)
    # map_beland_14 <<- get_map(location = "Bellandur Benaglore India", zoom = 14)
    # map_beland_13 <<- get_map(location = "Bellandur Benaglore India", zoom = 13)
    # map_keerti <<- get_map(location = rev(keerti), zoom = 15)
    # map_cvr_po <<- get_map(location = rev(cvrpo), zoom = 15)
    # map_pin87_15 <<- get_map(location = rev(pin87), zoom = 15)
    # map_pin87_14 <<- get_map(location = rev(pin87), zoom = 14)
    # map_cvr_14 <<- get_map(location = rev(cvr), zoom = 14)
    # map_cvrm_13 <<- get_map(location = rev(centre_cvraman), zoom = 13)
    # map_cvrm_12 <<- get_map(location = rev(centre_cvraman), zoom = 12)
    # map_blndr_12 <<- get_map(location = rev(belandur), zoom = 12)
}


urls_places <- c(
    normal = 'https://maps.googleapis.com/maps/api/place/findplacefromtext/json?input=Museum%20of%20Contemporary%20Art%20Australia&inputtype=textquery&fields=formatted_address%2Cname%2Crating%2Copening_hours%2Cgeometry&key=YOUR_API_KEY',
    loc_bias = 'https://maps.googleapis.com/maps/api/place/findplacefromtext/json?input=mongolian&inputtype=textquery&locationbias=circle%3A2000%4047.6918452%2C-122.2226413&fields=formatted_address%2Cname%2Crating%2Copening_hours%2Cgeometry&key=YOUR_API_KEY',
    phone = 'https://maps.googleapis.com/maps/api/place/findplacefromtext/json?input=%2B16502530000&inputtype=phonenumber&key=YOUR_API_KEY',
    nearby = 'https://maps.googleapis.com/maps/api/place/nearbysearch/json?location=-33.8670522%2C151.1957362&radius=1500&type=restaurant&keyword=cruise&key=YOUR_API_KEY',
    details = 'https://maps.googleapis.com/maps/api/place/details/json?fields=name%2Crating%2Cformatted_phone_number&place_id=ChIJN1t_tDeuEmsRUsoyG83frY4&key=YOUR_API_KEY'
)

build_urls_normal <- function(addr_vect){
    cr_url <- function(x = addr_vect){
        urlplaces <- urls_places["normal"]
        url_shell <- parse_url(urlplaces)
        url_shell$query <-  
            list(input = x,inputtype = 'textquery',
                 fields = "formatted_address,name,geometry,place_id,type",
                 key = apikey)
        build_url(url_shell)
    }
    # Loop the function across all address vectors
    addr_vect %>% map_chr(cr_url)  
} 

# pass a DT with 3 cols: lat, ,lng and keyword
# output is a list of URLs for nearby search with the keywords
build_urls_nearby <- function(dt_latlng,rad = 500){
    cr_url <- function(lat,lng,word){
        urlplaces <- urls_places["nearby"]
        url_shell <- parse_url(urlplaces)
        url_shell$query <-  
            list(location = paste(lat,",",lng),
                 radius = rad,
                 keyword = word,
                 #rankby = "distance",
                 fields = "formatted_address,name,geometry,place_id,type",
                 key = apikey)
        build_url(url_shell)
    }
    # Loop the function across all address vectors
    setcolorder(dt_latlng,c("lat","lng"))
    dt_latlng %>% pmap_chr(~cr_url(lat = ..1,lng = ..2, word = ..3))  
} 

# 
build_url_details <- function(placeids){
    cr_url <- function(placeid){
        url <- urls_places["details"]
        url_shell <- parse_url(url)
        url_shell$query <-  
            list(placeid = placeid,
                 key = apikey)
        build_url(url_shell)
    }
    placeids %>% map_chr(cr_url)
}

# seems formatted_address is not returned by nearby API, so you will get a NULL
extr_formatted_addr <- function(resp){
    x2 <- content(resp)
    if(x2$status == "OK"){
        x2$result$formatted_address
    } else "Error"
}

extr_names <- function(resp){
    x2 <- resp %>% content
    result_length <- length(x2$results)
    seq_len(result_length) %>% 
        map_chr(~x2$results[[.x]]$name)
}

extr_vicinity <- function(resp){
    x2 <- resp %>% content
    result_length <- length(x2$results)
    seq_len(result_length) %>% 
        map_chr(~x2$results[[.x]]$vicinity)
}

extr_placeid <- function(resp){
    x2 <- resp %>% content
    result_length <- length(x2$results)
    seq_len(result_length) %>% 
        map_chr(~x2$results[[.x]]$place_id)
}

extr_geo <- function(resp){
    x2 <- resp %>% content
    result_length <- length(x2$results)
    # lat <- seq_len(result_length) %>% 
    #     map_dbl(~x2$results[[.x]]$geometry$location$lat)
    # lng <- seq_len(result_length) %>% 
    #     map_dbl(~x2$results[[.x]]$geometry$location$lng)
    #return(matrix(c(lat,lng),ncol = 2) %>% as.data.table() %>% setnames(c("lat","lng")))

        # simplified now
    return(seq_len(result_length) %>% map(~x2$results[[.x]]$geometry$location %>% as.data.table()) %>% rbindlist())
}
# doesnt work on the places/details api results.. need to be tested again
extract_details <- function(x2){
    # candidates_searched <- x2$candidates %>% length()
    # if(candidates_searched > 0){
    #     cat(candidates_searched,",",sep = "")
    # } else 
    #     return( data.table(name = character(),lat = numeric(),lng = numeric(),faddr = character(), placeid = character(), subloc = character()))
    # 
    extr_n <- function(n){
        data.table(name = content$candidates[[n]]$name,
                   lat = content$candidates[[n]]$geometry$location$lat,
                   lng = content$candidates[[n]]$geometry$location$lng,
                   faddr = content$candidates[[n]]$formatted_address, 
                   placeid = content$candidates[[n]]$place_id, 
                   subloc = content$candidates[[n]]$types %>% unlist %>% paste(collapse = ",")
        )
    }
    content(x2) -> content
    N <- content$candidates %>% length()
    if(N > 0 )
    seq_len(N) %>% map(extr_n) %>% rbindlist(fill = T,idcol = "ID") %>% .[,N:=N] 
    else data.table()
}

# pass a vector of GET contents
cr_dt_from_GET_content <- function(x3){
    x3 %>% map(extract_details) %>% rbindlist()
}

# pass a dt with lat,lon only and a keyword char vector, output will be same DT with tall rows expanded with all keywords attached
add_keyword <- function(dt,keywordvect = list_of_keywords){
    stopifnot("lon" %in% names(dt) | "lng" %in% names(dt))
    if(!"lng" %in% names(dt)){
        message("Renaming column to lng")
            setnames(dt,"lon","lng")
        }
    dt[,.(lat,lng)][,k:=paste(keywordvect,collapse = ",")] %>% cSplit("k",sep = ",",direction = "tall")
}



gen_bbox <- function(sw,dx,dy){
    ne <- c(sw[1] + dx,sw[2] + dy)
    return(c(sw,ne))
}

gen_bbx_vect <- function(bbox,cols=10,rows=5){
    arr = array(rep(NA,times = cols * rows * 4),dim = c(rows,4,cols))
    dy <- bbox[3] - bbox[1]
    dx <- bbox[4] - bbox[2]
    cat("dx =", dx)
    cat("...dy =", dy,"\n")
    func <- function(c,r){
        {
            arr[r,1,c] <<- c(bbox[1] + (r - 1) * dx)
            arr[r,2,c] <<- c(bbox[2] + (c - 1) * dy)
            arr[r,3,c] <<- c(bbox[3] + (r) * dx)
            arr[r,4,c] <<- c(bbox[4] + (c ) * dy)
        }
        #print(arr)
        return(arr)
    }
    
    walk(seq_len(rows),~map(seq_len(cols),func,.x))
    return(arr)
}

gen_path <- function(bbox){
    matrix( c(
        bbox[c(2,1)],
        bbox[c(2,3)],
        bbox[c(4,3)],
        bbox[c(4,1)],
        bbox[c(2,1)]
    ),
    ncol = 2,
    byrow = T
    )  
}


########## Solution from Stackoverflow
# https://stackoverflow.com/a/34187454/1972786 : create circles data frame from
# the centers data frame remember radius is in Kms
make_circles <- function(centers, radius, nPoints = 100){
    if(!"lat" %in% names(centers)) message("Input Dt does not contain mandatory column: lat")
    if("lon" %in% names(centers)){
        message("Found longitude column with name 'lon' - renaming to 'lng' as needed by make_circles()")
        centers %>% setnames("lon","lng")
    } 
    if(!"ID" %in% names(centers)){
        message("Did not find unique ID; adding a unique ID column by name ID to original DT")
        centers[,ID:=rownames(centers)]
    } 
    # centers: the data frame of centers with ID
    # radius: radius measured in kilometer
    #
    meanLat <- mean(centers$lat)
    # length per longitude changes with lattitude, so need correction
    radiusLon <- radius /111 / cos(meanLat/57.3) 
    radiusLat <- radius / 111
    circleDF <- data.table(ID = rep(centers$ID, each = nPoints))
    angle <- seq(0,2*pi,length.out = nPoints)
    
    circleDF[,lat:= unlist(lapply(centers$lat, function(x) x + radiusLat * sin(angle)))]
    circleDF[,lng:= unlist(lapply(centers$lng, function(x) x + radiusLat * cos(angle)))]
    #circleDF$lat <- unlist(lapply(centers$lat, function(x) x + radiusLat * sin(angle)))
    return(circleDF)
}
##################################################################################

# here is the data frame for all circles
#myCircles <- make_circles(data, 0.45)


plot_circles <- function(myCircles,map = staticmap,circolor = "red"){
    if("lon" %in% names(myCircles)) setnames(myCircles,"lon","lng")
    RL = geom_point(aes(x = lng, y = lat), data = myCircles, color = "#ff0000",size = 0.2)
    ggmap(map) + RL + 
        # scale_x_continuous(limits = c(77.620, 77.75), expand = c(0, 0)) +
        # scale_y_continuous(limits = c(12.92, 13.02), expand = c(0, 0)) +
        
        ########### add circles
        
        geom_polygon(data = myCircles, aes(lng, lat, group = ID), color = circolor, alpha = 0,size=0.5)
}


# pass one GET output from nearby places API (if list of responses use map)
extr_all_places <- function(x1){
    extr_place_n <- function(n){
        
        r <- content$results
        data.table(
            name = r[[n]]$name,
            keyword = x1$url %>% parse_url() %>% .$query %>% .$keyword,
            lat = r[[n]]$geometry$location$lat,
            lng = r[[n]]$geometry$location$lng,
            placeid = r[[n]]$place_id,
            types = r[[n]]$types %>% paste(collapse = ","),
            vicinity = r[[n]]$vicinity,
            busstatus = r[[n]]$business_status,
            bbne = r[[n]]$geometry$viewport$northeast %>% paste(collapse = ","),
            bbsw = r[[n]]$geometry$viewport$southwest %>% paste(collapse = ","),
            date = x1$date %>% ymd_hms(tz = "Asia/Kolkata"),
            centre_point = x1$url %>% parse_url() %>% .$query %>% .$location
        )
    }
    content(x1) -> content
    N <- content$results %>% length()
    tok <- content[['next_page_token']]
    if(N==20 & is.null(tok))
        message("\nNext page token missing, even though 20 establishments returned")
    dt1 <- 
        seq_len(N) %>% 
        map(extr_place_n) %>% 
        rbindlist(fill = T,idcol = "ID") %>% 
        .[,N:=N] 
    
    if(!is.null(tok))
        dt1[,nextpage:=tok]
    
    if("centre_point" %in% names(dt1))
        dt2 <- 
        dt1 %>% 
        cSplit(splitCols = "centre_point",sep = ",",stripWhite = T) %>% 
        rename(cp_lat = centre_point_1,cp_lng = centre_point_2) %>% 
        mutate(dist= distGeo(p1 = matrix(c(lng,lat),ncol = 2),
                             p2 = matrix(c(cp_lng,cp_lat),ncol = 2))) else 
                                 dt2 <- dt1
    return(dt2)
}

# pass one starting coordinate (2 element vector, lat, lon), and the distance in metres to the next circle centre
gen_circle_centres <- function(startcoord,incrby = 500,totalcircles=10,direction = c(1,1)){
    incr_lat <- perkm_lat * incrby / 1000 * direction[1]
    incr_lng <- perkm_lng * incrby / 1000 * direction[2]
    lat_vect <- (startcoord[1] + seq(0, by = incr_lat, length.out = totalcircles))
    lng_vect <- (startcoord[2] + seq(0, by = incr_lng, length.out = totalcircles))
    outer(lng_vect,lat_vect,paste,sep = ",") %>% 
        as.vector() %>% 
        as.data.table() %>% 
        setnames("latlng") %>% 
        cSplit(splitCols = "latlng",direction = "wide",sep = ",") %>% 
        setnames(c("lng","lat"))
}


gen_circle_centres4 <- function(cp = centre_cvraman,m = 1000, n = 5){
    list(
        c(1,1),
        c(-1,1),
        c(-1,-1),
        c(1,-1)
    ) %>% 
    map( ~ gen_circle_centres(startcoord = cp,incrby = m ,totalcircles = n,direction = .x)) %>% 
        rbindlist(fill = T) %>% unique
}


######## MAIN scrape function to GET neraby locations ###########
verbose_GET <- function(urlvector){
    resp <- list()
    GET2 <- function(x){
        r <- GET(x)
        if(r$status_code == 200) cat(content(r)$result %>% length(),",",sep = "") else cat("E,")
        return(r)
    }
for(i in urlvector){
    # sleep for a variable number of seconds with a mean of 2 seconds
    Sys.sleep(rnorm(1,mean = 5,sd = 20) %>% max(0,.))
    resp <- c(resp,list(GET2(i))) # join the response to the master array of responses
}
    resp
}

build_nextpage <- function(token){
   baseurl <-  urls_places["nearby"] %>% parse_url()
   baseurl$query <- list(
       pagetoken = token,
        key = apikey
   )
   build_url(baseurl)
}

# pass one response of nearby search to extract next page token if it exists:
# this is not so useful as we get nextpage token from the final DT also,
extr_nextpage_tok <- function(response){
    if("next_page_token" %in% names(content(response[[1]])))
        return(content(response[[1]])$next_page_token) else return(NULL)
}

build_nextpage_urls <- function(tokens){
    tokens %>% map_chr(build_nextpage)
}

#####################
##### Faster version of ward identification.
##### Pass a datatable with two columns (lng, lat)
##### Pass the KML file of wards read as a spatial object with st_read()
##### Output will be a numeic vector of ward numbers
######################
find_wards <- function(dt,blr = s1){
    if("lon" %in% names(dt) & !"lng" %in% names(dt))
        setnames(dt,"lon","lng")
    sf1 <- dt[,c("lng","lat")] %>% pmap(~st_point(c(.x,.y))) %>% st_as_sfc
    st_crs(sf1) <- 4326
    sf1 %>% st_within(blr) %>% as.numeric
}



# plot ward borders and shade them basis apartment counts. Pass the scraped data
# till now across all wards (read from RDS if needed)
plot_prio_wards <- function(mapobj = map_full_blr11,scrapeddt){
    
    ward.index <- seq_along(allwards)
    names(ward.index) <- allwards
    warddata <- scrapeddt[,.(countapp = .N),ward] # summarise on count of appts per ward
    borders <- function(n) s1[allwards[n],] %>% st_coordinates() %>% as.data.table()
    wardborders <- ward.index %>% map(borders) %>% rbindlist(idcol = "ward") %>% mutate(ward = as.numeric(ward))
    data <- warddata[wardborders,on= "ward"]
    ggmap(mapobj) +  
        geom_polygon(aes(X,Y,group = ward, fill = countapp),alpha = 0.5,data = data)  + 
        scale_fill_binned(type = "viridis") +
        geom_path(aes(X,Y,group = ward),data = data) +
        geom_label(aes(lng,lat,label = ward),data = dt_centroids[ward %in% allwards])
        #geom_point(aes(lng,lat,col = ward),data = dtcen[sample(seq_len(nrow(dtcen)),size = 1000)],alpha = 0.5)
}

plot_communities <- function(placedt = places1,mapobj = map_full_blr11){
    borders <- function(n) s1[allwards[n],] %>% st_coordinates() %>% as.data.table()
    nborders <- 1:20 %>% map(borders) %>% rbindlist(idcol = "index")
    polyborders <- 1:20 %>% map(borders) %>% rbindlist(idcol = "index")
    ggmap(mapobj) +  
         geom_polygon(aes(X,Y,group = index),alpha = 0.5,data = polyborders)  + 
        geom_path(aes(X,Y,group = index),data = nborders) +
        geom_label(aes(lng,lat,label = ward),data = dt_centroids[ward %in% allwards]) +
        geom_point(aes(lng,lat,col = factor(ward)),data = placedt,alpha = 0.5)
}

filter_prio_wards <-  function(dt){
    if(!"ward" %in% names(dt) & "lng" %in% names(dt) & "lat" %in% names(dt)){
        message("ward column missing, being calculated and added")   
        dt[,ward:=find_wards(dt)]
    }
    dt[ward %in% allwards]
}

get_cen_residue <- function(dtscrape,dtcen){
    dtcen %>% unique(by= c("lat","lng","k")) %>% rename(cp_lat = "lat",cp_lng = "lng",keyword = "k") ->  dtcen.u
    dtscrape  %>% filter(!is.na(name)) %>% select(cp_lat,cp_lng,keyword,name) %>% unique(by = c("cp_lat","cp_lng","keyword")) -> dtscrape.u
    dtscrape.u %>% 
        right_join(dtcen.u,by = c("cp_lat", "cp_lng","keyword")) %>% 
        filter(is.na(name)) %>% 
        select(cp_lat,cp_lng,keyword) 
}

# Generate single ward centers by uncommenting

#gen_circle_centres4(cp = cen(174),m = 200,n = 25) -> dtcen1
#dtcen174 <- dtcen1[,ward:=find_wards(dtcen1)][ward==174]

# dtcen174 %>% build_urls_nearby(rad = 100) %>% verbose_GET() -> r174
# saveRDS(r174,"resp174")


