source("~/bnp_people/places.R")
load_static_maps()
s1[66,] %>% st_coordinates()
1:198 %>% map(~s1_geom[[.x]] %>% st_centroid %>% as.numeric) %>% unlist %>% matrix(ncol=2,byrow = T) %>% as.data.table  %>% setnames(c("lng","lat")) -> dt_centroids
dt_centroids[,ward:=find_wards(dt_centroids,blr = s1)]
s1_geom <- st_geometry(s1)
s1_geom[[66]]
s1_geom[[66]] %>% st_centroid()
s1_geom[[66]] %>% st_centroid() %>% as.numeric
dt_centroids[ward ==66]
gen_circle_centres4( c(13.00704,77.55711),m = 100,n = 6) -> dtcen2
### Generate larger circles
gen_circle_centres4( c(13.00704,77.55711),m = 200,n = 4) -> dtcen2
ggmap(map66_15) + geom_polygon(aes(X,Y,group = L1),data = s1[66,] %>% st_coordinates() %>% as.data.table(),col = "grey51",alpha = 0.3) + geom_point(aes(lng,lat),data = dtcen2)
ggmap(map66_15) + geom_polygon(aes(X,Y,group = L1),data = s1[66,] %>% st_coordinates() %>% as.data.table(),col = "grey51",alpha = 0.3) + geom_point(aes(lng,lat),data = dtcen2)
dtcen2 %>% find_wards()
dtcen2[,ward:=find_wards(dtcen2)]
dtcen2
dtcen2[,INWRD:=ifelse(ward==66,T,F)]
gen_circle_centres4( c(13.00704,77.55711),m = 100,n = 10) -> dtcen2
ggmap(map66_15) + geom_polygon(aes(X,Y,group = L1),data = s1[66,] %>% st_coordinates() %>% as.data.table(),col = "grey51",alpha = 0.3) + geom_point(aes(lng,lat,col = INWRD),data = dtcen2) + scale_color_manual(values = c("grey51","blue"))
dtcen2[INWRD==T]
make_circles(centers = dtcen2[INWRD==T],radius = 0.1) -> cirdt1
ggmap(ggmap = map66_15) + geom_point(aes(lng,lat),data = dtcen2[INWRD==T]) + geom_point(aes(lng,lat),data = cirdt1,col = "red")
make_circles(centers = dtcen2[INWRD==T],radius = 0.05) -> cirdt1
ggmap(ggmap = map66_15) + geom_point(aes(lng,lat),data = dtcen2[INWRD==T]) + geom_point(aes(lng,lat),data = cirdt1,col = "red",size = 0.5)
ggmap(map66_15) + geom_polygon(aes(X,Y,group = L1),data = s1[66,] %>% st_coordinates() %>% as.data.table(),col = "grey51",alpha = 0.3) + geom_point(aes(lng,lat),data = cirdt1,col = "red",size = 0.2)
dtcen2[INWRD==T] %>% add_keyword()

##### ACTUAL SCRAPE STARTS HERE - sample 3 center points
dtcen2[INWRD==T][sample(1:23,3)] %>% add_keyword() %>% build_urls_nearby(rad = 50) %>% verbose_GET() -> r1
r1 %>% map(extr_all_places) %>% compact %>% rbindlist(fill = T) -> places1
##### DONE #########3

places1
places1$name
places1$name
places1[is.na(name),.N]
places1[,.(name,lat,lng,types,N,dist)]
places1[,.(str_sub(name,1,50),lat,lng,types,N,dist)]
places1[,.(str_sub(name,1,50),keyword,lat,lng,types,N,dist)]
places1[,.(str_sub(name,1,40),keyword,lat,lng,types,N,dist)]

ggmap(map66_15) + geom_polygon(aes(X,Y,group = L1),data = s1[66,] %>% st_coordinates() %>% as.data.table(),col = "grey51",alpha = 0.3) + geom_point(aes(lng,lat),data = places1,col = "red",size = 0.2)
ggmap(map66_15) + geom_polygon(aes(X,Y,group = L1),data = s1[66,] %>% st_coordinates() %>% as.data.table(),col = "grey51",alpha = 0.3) + geom_point(aes(lng,lat),data = places1,col = "red",size = 1,col = "blue")

