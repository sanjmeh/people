# baf membership data management
source("gfunctions.R")

# Download BAF membership latest googlesheet into a DT
load_members <- function(dt=NULL,ss=bafmembsheet){
        if(is.null(dt)) {
                drive_download(file = ss,type = "csv",overwrite = T)
                dt1 <- fread("BAF Member Database.csv")
                setnames(dt1,qc(bafno,appt_long,status,appt_baf,nblks,flatcount,appt_addr,
                                locty,city,pin,ph1,ph2,ward,acno,pcno,clustno,totrep,doj,fsales,
                                maplink,clust_name,zonlead,target,ward_name,zone,assly))
        }
        else
                dt1 <- dt
        
        dt1 <- 
                map_at(dt1,.at = qc(bafno,status,appt_baf,locty,city,status,ward,
                                    clustno,ward_name,zone,assly),  as.factor) %>% 
                as.data.table
        dt1[,pin:= as.numeric(pin)]
        dt1[,appt_baf:= repl_fullnames(appt_baf)]
        dt1[,clean_name := str_replace_all(appt_baf,"[:punct:]"," ") %>% str_squish]
        dt1[,clean_appt_addr :=
                    ifelse(!grepl("apart",clean_name,ig=T),
                           paste(clean_name,"Appartments"),
                           clean_name
                    ) %>% paste(appt_addr)
        ]
        # below lines need to be identical in both : process BBMP addresses as well as process BAF data
        compress_addr(dt1,"clean_appt_addr") # remmeber this adds additional columns to dt1, all compressed addresses.
        1:3 %>% as.character() %>% walk2(c("I","II","III"),~replace_numeric(dt1,.x,.y))
        dt1[,google_addr:= prep_addr(paste(clean_appt_addr,pin))]
        dt1
}

# pass a DT with columns bafno and appt_name and geo codes of the appts will be binded as new columns
merge_geocodes <- function(dt=bafmembs,file=geocodesfile){
        gc <- fread(file,stringsAsFactors = T)
        gc[dt,on=.(bafno,appt_baf)]
}


merge_zonal_into_baf <- function(voldt=vol2,bafdt=bafmembs){
        unique(voldt[,.(bafno,appt_baf,cluster,flatcount,zonlead,wardno)],by=c("bafno"))[bafdt,on=.(bafno,appt_baf)]
}

merge_bafmaster_into_vol <- function(voldt=vol2,bafdt=bafmembs){
        bafdt[,-c("locty","clean_name","city")][voldt,on=c("bafno","appt_baf")]
}

# pass a DT that has unique bafnos and appt_baf
# output is an index on each baf appt that is referred for running apartment string checks in addresses to match to these strings
add_appt_indx <- function(dt,indx=0.6,size=5){
        dt[,appt_dbl := str_action(appt_baf,"dou")]
        dt[,appt_vow := str_action(appt_baf,"vow")] #  keep it independent - double and vowel separately calculated. Donot cumulate.
        dt[,ncvow:=str_split(appt_baf," ",simplify = T) %>%  str_action("vow") %>% nchar %>% paste(collapse =","),by=bafno] # no. of characters in each word
        dt[,ncdbl:=str_split(appt_baf," ",simplify = T) %>%  str_action("dou") %>% nchar %>% paste(collapse =","),by=bafno]
        dt[,ncappt:=str_split(appt_baf," ",simplify = T) %>%  nchar %>% paste(collapse =","),by=bafno] # total charcharacter count
        dt[,ivwl:=(str_split(ncvow,",",simplify = T) %>% as.numeric() / 
                           str_split(ncappt,",",simplify = T) %>% as.numeric())
           %>% round(3) %>% paste(collapse =",") ,by=bafno]
        dt[,idbl:=(str_split(ncdbl,",",simplify = T) %>% as.numeric() / 
                           str_split(ncappt,",",simplify = T) %>% as.numeric()) %>% 
                   round(3) %>% paste(collapse =","),by=bafno]
        dt[,passv:=ifelse(str_split(ncvow,",",simplify=T) >= size | str_split(ivwl,",",simplify = T) > indx, T,F) %>% all,by=bafno]
        dt[,passd:=ifelse(str_split(ncdbl,",",simplify=T) >= size | str_split(idbl,",",simplify = T) > indx, T,F) %>% all,by=bafno]
        dt
}

edit_appt_indx <- function(dt,indx=0.6,size=5){
        dt[,passv:=ifelse(str_split(ncvow,",",simplify=T) >= size | str_split(ivwl,",",simplify = T) >= indx, T,F) %>% all,by=bafno]
        dt[,passd:=ifelse(str_split(ncdbl,",",simplify=T) >= size | str_split(idbl,",",simplify = T) >= indx, T,F) %>% all,by=bafno]
        dt
}



# check cache before firing for identical address
fire_geocode <- function(addr_str,gmast=geomaster,exact_match=T,dist=1){
        mast_addr <- tolower(gmast$gaddr) # grep will be cap insensitive
        names(addr_str) <- addr_str
        addr_str <- tolower(addr_str)
        if(exact_match==T) 
                ex_geo <-addr_str %>%  map(~gmast[grepl(.x,gaddr,ig=T)]) %>% rbindlist
        else
        {
                match_mast_lgl <- addr_str %>% map_lgl(~grepl(.x,mast_addr) %>% any)
                ex_geo <- addr_str %>% imap(~{cbind(str=.y,present= find_gmast(.x,gmast = gmast,distance = dist))} ) %>% rbindlist()
        }
        return(ex_geo)
        # message("detected ",nrow(ex_geo)," addresses existing in cache .. pulling them in")
        # new_addr <- setdiff(addr_str,gmast$gaddr)
        # new_geo <- geocode(new_addr) %>% cbind(data.table(gaddr=new_addr),.)
        # rbind(ex_geo,new_geo)
}

# Search a single word (sword) in the google adddress maze (gaddr is a char array)
# distance is now dynamically taken based on length of the single word
srch1 <- function(sword,gaddr){
        sword <- tolower(sword)
        distance <- case_when (
                nchar(sword) <5 ~ 0,
                nchar(sword) <10 ~ 1,
                nchar(sword) <15 ~ 2,
                TRUE ~ 3
        )
        (gaddr %>% str_split("\\+") %>% 
                        map_depth(.depth = 1,~stringdist(sword,tolower(.x))) %>% 
                        map_dbl(min) <= distance) %>% gaddr[.]
}

# call srch1 as many times and intersect the matches for all words independently
# runs very slow.. likely to be abanodoned
shortlist_gaddr <- function(words,gaddr){
        superset <- str_split(words,"\\s+|\\+",simplify = T) %>% 
                imap(~srch1(.x,gaddr))
        if(length(superset) > 1) do.call(intersect,superset) else 
                superset
}

# not used
get_geo <- function(lmtch=lmtch,gmast=geomaster){
        lmtch %>% imap(~gmast[gaddr %in% .x,.(lon,lat)][,str:=.y]) %>% rbindlist
}


# increment the lat-lon database on googlesheet with new BAF members.
# step 1 : download latest geocodes
# step 2: download latest BAF members
# step 3: Identify new BAF members
# step 4: Pull google lat lon for new BAF addresses
# step 5: Appnend the geocodes back on the google sheet
# the function will save global variables on thw way
# edit the argument flags: add string for the steps to be executed and remove for steps to be skipped

refresh_baf <- function(flags="memb+geo+api+upload"){
       if(grepl("memb",flags))  bafmembs <<- load_members()
       if(grepl("geo",flags)) bafgeo <<- read_sheet(bafmembsheet,sheet = "geocodes")
       new_addr <- bafgeo[!bafno %in% bafmembs$bafno,google_addr]
       if(grepl("api",flags)) incr_gcodes <- new_addr %>% geocode()
       new_membs <- bafmembs[!bafno %in% bafgeo$bafno,.(bafno,appt_baf,clean_appt_addr,pin,lat=incr_gcodes$lat,lon=incr_gcodes$lon)]
       if(grepl("upload",flags))  new_membs %>% sheet_append(ss=bafmembsheet,data = .,sheet = "geocodes")
}





