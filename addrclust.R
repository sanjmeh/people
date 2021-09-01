library(tidyverse)
library(janitor)
library(readxl)
library(magrittr)
library(stringr)
library(stringdist)
library(splitstackshape)
library(data.table)
library(googlesheets4)
library(googledrive)
library(lubridate)
library(wrapr)
library(gmapsdistance)
library(geosphere)
library(ggmap)
library(ggrepel)
library(htmlTable)
load("keys.data")

#helper
`%notin%` <- function(lhs, rhs) !(lhs %in% rhs)


# input an address vector to get a address vector with + signs in between
prep_addr <- function(addstr,bangalore=T){
  str2 <- addstr %>% str_replace_all("[#,&]"," ") %>% 
    str_trim %>% str_replace_all("\\s+","+") %>% 
    {ifelse(grepl("Bangalore|Bengaluru", .,ig=T), paste(.,"Karnataka",sep="+"),paste(.,"Bangalore+Karnataka",sep="+"))}
  str2 %>% strsplit("\\s+") %>% map(paste,collapse="+") %>% unlist
}


getmap <- function(cen=center_ward46,zoom=15){
  get_googlemap(center = cen,
                zoom=zoom,
                size=c(640,640),
                format="png8"
                )
}

# returns a DT
load_covid_data <- function(file="kaggle_data/covid_19_india.csv"){
  x1 <- fread(file)
  x1[,Date:=dmy(Date)]
  setnames(x1,qc(sn,Date,Time,state,indnat,fornat,cured,dead,confm))
  x1[,mth:=month(Date)]
}

plot_top_n <- function(dt,n=5){
  dt[,{topstates <- .SD[order(-confm),unique(state)][seq_len(n)]
  .SD[state %in% topstates]}] %>% 
    ggplot(aes(Date,dead)) + 
    geom_line(aes(col=state),size=2) + 
    facet_wrap(~state,scales = "free_y")
}

states_top_n <- function(dt,n=5){
  dt[,{
    topstates <- .SD[order(-confm),unique(state)][seq_len(n)]
    .(topstates)
    }] 
}

# pass covid dataset DT with unique string of one state only. Output will have two new columns with doubling rate of deaths and confirmed cases.
# this function is used by the next function, and not used independently
dble_days <- function(dt,statestr="karnat"){
  dt <- dt[grepl(statestr,state,ig=T)]
  diffdays_dead <- dt$Date %>% map(~ dt[first(which(dt$dead>=dt[Date==.x,2*dead])),Date - .x]) %>% unlist
  diffdays_conf <- dt$Date %>% map(~ dt[first(which(dt$confm>=dt[Date==.x,2*confm])),Date - .x]) %>% unlist
  dt[seq_along(diffdays_dead),dbldays_dead:=diffdays_dead]
  dt[seq_along(diffdays_conf),dbldays_cases:=diffdays_conf]
  dt[,dbldays_dead:=ifelse(dbldays_dead<0,NA,dbldays_dead)]
  dt[,dbldays_cases:=ifelse(dbldays_cases<0,NA,dbldays_cases)]
  dt[,futDate_dead:=Date+dbldays_dead]
  dt[,futDate_cases:=Date+dbldays_cases]
}

# pass the covid dataset and value of n, to return a DT with just states, futureDate vals and doubling rate (mean)
dbl_data_all <- function(dt,n=5,type="dead"){
  dtdbl <- states_top_n(dt,n = n)$topstates %>% map(~dble_days(dt,.x)) %>% rbindlist
  if(type=="dead") 
    dtdbl2 <- 
      dtdbl[!is.na(futDate_dead),.(Days=mean(dbldays_dead,na.rm = T)),by=.(state,Date=futDate_dead)] else
        dtdbl2 <- dtdbl[!is.na(futDate_cases),.(Days=mean(dbldays_cases,na.rm = T)),by=.(state,Date=futDate_cases)]
  
      dtdbl2[order(Date)]
}

# generate n samples for all the curves for unclutterred labeling on the line chart
gen_label_samples <- function(dt,n=3){
  dtchunks <- split(dt,f = dt$state)
  model_list <- dtchunks %>% map(~pam(.x$Days,n)) # using Partitioning around Mediods (better than kmeans)
  select_samples <- function(dt,model) split(dt,model$clustering) %>% map(~.x[sample(nrow(.x),1)]) %>% rbindlist # select 1 sample per partition
  model_list %>% map2(.y = dtchunks, .f = ~select_samples(.y,.x)) %>% rbindlist
}

# most frequent string actions coded in one function
str_action <- function(x,what="punct"){
  x <- as.character(x)
  case_when(
    grepl("punct",what,ig=T) ~ str_remove_all(x,"[\\s[:punct:]]") %>% tolower,
    grepl("vow",what,ig=T) ~ str_remove_all(x,regex("[aeiou]",ig=T)),
    grepl("dou|dbl",what,ig=T) ~ str_replace_all(x, regex("([a-z])\\1{1,}",ig=T),regex("\\1",ig=T)),
    grepl("near|next",what,ig=T) ~ str_replace(x,regex("(near|opp[.]*|behind|opp to|next to|beside) [a-z]{1,20}",ig=T),"BLACKLIST"),
    TRUE ~ x
  )
}

# add new compressed columns of address
compress_addr <- function(dt,colname="ADDRESS"){
  dt[,addr_compr:=str_action(get(colname),"punct")] 
  dt[,addr_rm_doub:= str_action(addr_compr,"dbl")] # remove doubles before removing vowels
  dt[,addr_rm_vow:= str_action(addr_compr,"vowel")] # remove vowels
  dt[,addr_rm_vow_and_doub:=  str_action(addr_rm_doub,"vowel")] # remove doubles as well as vowels
  dt[,google_addr:=prep_addr(get(colname))]
}

load_bbmp_raw <- function(file="baf/Qwatch Data Dump Bangalore Urban & Rural districts as on 06082020 1900.csv",retainnames=F){
  x1 <- fread(file)
  # setnames(x1,qc(qwid,name,gender,mob,email,rem1,rem2,porto,porta,addr1,addr2,addr3,age,
  #                addrty,district,taluk,panchyt,ward,city,bbmpzone,qtype,postcode,state,DateQ,DateEndQ))
  x1 %<>%  map_if(is.character, ~ifelse(.x=="NULL" | .x=="" | .x==".",NA,.x) ) %>% as.data.table
  x1 %<>% map_at(.at = c(3,8:9,14:23), as.factor) %>% as.data.table 
  x1 %<>% map_at(.at = c(6:7), dmy) %>% as.data.table 
  x1 %<>% map_at(.at = c(13), as.numeric) %>% as.data.table 
  if(retainnames==F) {
    setnames(x1,qc(qwid,name,gender,mob,email,dateQ,endQ,porto,porta,addr1,addr2,addr3,
                 age,region,distt,taluk,panch,ward,city,
                 bbmpzone,qtype,pin,state))
  
  }
return(x1)  
}

# load the new excel file sent by BBMP daily.
load_bbmp_file <- function(f="BAF.xlsx",colsize=13){
  ctype <- rep("text",30) # max columns
  #ctype[c(9)] <- "numeric" # failed the 23rd file hence reverted to text
  ctype[7] <- "date" # Qurantine date is always at 7th position
  x1 <- read_excel(f,range = cell_cols(seq_len(colsize)), col_types = "text")
  setnames(x1,qc(ptype,qdays,qwid,name,gender,mob,dateQ,ADDRESS,age,bbmpzone,ward,state,region))
  x1 <- x1 %>% map_at(.at = c(1,5,10:13), as.factor) %>% as.data.table
  x1 <- x1 %>% map_at(.at = c(2,9), as.numeric) %>% as.data.table
  x1 <- x1 %>% map_at(.at = c(7), convert_to_date) %>% as.data.table # new function from janitor
  x1 %<>%  map_if(is.character, ~ifelse(.x=="NULL" | .x=="" | .x==".",NA,.x) ) %>% as.data.table()
  x1[,dateQ:=as.Date(dateQ)] # because convert_to_date brings it into Posixct Date. We need pure Date format
  
  
}

# pass the output of reading the file
proc_bbmp_raw <- function(dt){
  dt[,mob:=str_action(mob,"punct")]
  #dt[,ADDRESS := str_remove_all(ADDRESS,",?\\bNULL\\b,?") ]
  if("qdays" %in% names(dt)  ){
    dt[,qdays:=as.numeric(qdays)]
    dt[qdays>44000,dateB := as.Date(qdays,or="1900-01-01")]
    dt[qdays>44000,qdays:=NA]
  }
  if("ptype" %in% names(dt))
    suppressWarnings(dt[,dateB:= ptype %>% as.character() %>% str_extract("(?<=\\().+(?=\\))") %>% 
                          paste(2020) %>% dmy()]) # extract the hand entered date from brackets
  if(grepl("addr1",names(dt)) %>% any){
    dt[,ADDRESS:=paste(unique(c(addr1,addr2,addr3)),collapse = " ") %>% str_squish,by=qwid][,c("addr1","addr2","addr3"):=NULL]
  }
  dt[,ADDRESS := str_remove_all(ADDRESS,",?\\bNULL\\b,?") ]
  dt[,mob:=str_action(mob,"punct")]
  dt[,valid_mob := mob %>% str_detect("^[1-5]|^\\d{1,9}$|^.{11,}$") %>% not]
  dt[,junk_addr:= nchar(ADDRESS)<20]
  dt[,isappt := str_detect(ADDRESS,regex("\\bflat|\\bapart|\\bapp\\b|\\bappart|society|\\bdoor\\b",ig=T))]
  dt[,addr_black:=str_action(ADDRESS,"near")]
  compress_addr(dt,"addr_black")
  dt[,google_addr:=prep_addr(ADDRESS)]
}


# Change the digit with a roman number string. Useful for apartment names ending with 1,2,3.
replace_numeric <- function(dt,from="1",to="I") {
  dt[str_detect(appt_baf,"\\d"), clean_name:= clean_name %>% str_replace(from,to)]
}

# input  : DT with googlesheet4 read_sheet of BAF volunteer googlesheet
# output : processed output DT with cleaned BAF apartment names and addition of a google address column ready for running geocode()
proc_volunteers <- function(dt=vol1,setnames=T){
  setDT(dt)
  if(setnames==T){
  setnames(dt,old=c(1:9,13:26),new = c("dttim", "email", "name", "age", "mob", "society", "door", 
                                         "isbaf", "bafno", "appt_baf", "flatcount", "vol_address",
                                          "locty", "city", "pin", "cluster", "zonlead", "wardno", "wardname", 
                                         "subzone", "zone", "seqno","identfier" 
                                         ))
  }
  newcols <- names(dt) %>% str_subset("^[a-z_]+$")
  dt1 <- dt[,.SD,.SDcols=newcols]
  dt1[,mob:=as.double(mob)]
  dt1[,dttim := parse_date_time(dttim,orders=c("mdyHMS"),tz = "Asia/Kolkata")] 
  dt1[,appt_baf:= repl_fullnames(appt_baf)]
  dt1 <- map_at(dt1,.at = qc(society,isbaf,bafno,appt_baf,locty,city,cluster,zonlead,wardno,wardname,subzone,zone,identifier), as.factor) %>% as.data.table 
  dt1 <- map_at(dt1,.at = qc(age,flatcount,pin), as.numeric) %>% as.data.table
  map_if(dt1,is.character, ~ifelse(.x=="NULL" | .x=="" | .x==".",NA,.x) ) %>% setDT
  dt1[,clean_name := str_replace_all(appt_baf,"[:punct:]"," ")]
  
  compress_addr(dt1,"appt_baf") # replaced repetitive lines by a function
  1:3 %>% as.character() %>% walk2(c("I","II","III"),~replace_numeric(dt1,.x,.y))
  dt1[,google_addr:=ifelse(!is.na(clean_name), 
                          prep_addr(paste(clean_name,
                                          ifelse(is.na(locty),"",as.character(locty)),
                                          ifelse(is.na(vol_address),"",as.character(vol_address)),
                                          ifelse(is.na(pin),"",pin))),
                          prep_addr(paste(society,
                                          ifelse(is.na(vol_address),"",
                                                 as.character(vol_address)),
                                          ifelse(is.na(pin),"",pin)))),
by=email]
}

repl_fullnames <- function(x){
x <- as.character(x)
 case_when(
        x=="SPP" ~ "Sai Poorna Premier",
        x=="SMR Vinay" ~ "SMR Vinay Endeavour",
        x=="ZZZ" ~ "ZZZ: dummy",
        TRUE ~ x
        ) 
}

# old function - no longer used
# be careful as few variables are had coded inside ;: bbmp_trunc is nothing but bbmp subset data that has likely flat/apartment addresses
# donot forget to re index the data once new bbmp data received
map_bafno <- function(indx,baf_names,n=3,bafnos){
  stopifnot(uniqueN(baf_names)==length(baf_names))
  stopifnot(length(indx)==length(baf_names))
  addr3 <- indx %>% map2(baf_names,~.x %>% intersect_3(n=n,appt_name=.y) %>% bbmp_trunc[.])
  names(addr3) <- baf_names
  addr3
}

# main function to merge bbmp data to baf member data
merge_baf <- function(bbmp,baf,volunt=T){
  
  # pass the two DTs and a variable: 
  # generalized on which variable we use for search string as well as pattern string. Pass the variable one of :  "addr_compr", "addr_rm_vow", "addr_rm_doub"
  get_match_index <- function(var, x=bbmp,y=baf,base_data=baf_base_data){
    appt_indx <- y[!is.na(appt_baf),eval(var),with=F] %>% unique() %>% unlist %>% map(~str_which(x[[var]],regex(.x,ig=T)) %>% x[.,qwid])
    cols <- c(var,"bafno") # prepare the two columns for creating a unique bafno list (may be smaller in length due to compression)
    names(appt_indx) <- y[!is.na(appt_baf),.SD,.SDcols=cols] %>% unique(by=var) %>% .[,bafno] # extract the BAFnos against each compressed appt_name
    appt_indx_nz <- appt_indx %>% compact
    qwatch_ids <- appt_indx_nz %>% map(~data.table(qwatch=.x)) # list of bafno with matching qwatchIDs
    bafcases <- qwatch_ids %>% rbindlist(idcol = "BAFno")
    bafcases_wide <- base_data[bafcases,on=.(bafno=BAFno),nomatch=0]
    x1 <- x[!is.na(qwid)][bafcases_wide,on=.(qwid=qwatch),nomatch=0]
    if (!"ptype" %in% names(x1)) x1[,ptype:=NA]
    if (!"qdays" %in% names(x1)) x1[,qdays:=NA]
    if (!"dateB" %in% names(x1)) x1[,dateB:=NA]
    x1[,qc(bafno,appt_baf,appt_addr,lon,lat,qwid,ptype,qdays,name,age,gender,mob,dateQ,dateB,ADDRESS,google_addr,addr_compr,addr_rm_vow,addr_rm_doub,addr_rm_vow_and_doub,bbmpzone,ward,region,state,valid_mob,flatcount,locty,cluster,zonlead,wardno),with=F]
    }
  # get rid of junk first
  if(volunt==T) 
    baf_base_data <- baf[!is.na(appt_baf) & !grepl("Dup",bafno,ig=T),.(bafno,appt_baf,flatcount,locty,cluster,zonlead,wardno)] %>% unique 
  else
    baf_base_data <- baf
    
  baf_base_data[,addr_compr:=str_action(appt_baf,what = "punct")]
  baf_base_data[,addr_rm_doub:=str_action(addr_compr,what = "doub")]
  baf_base_data[,addr_rm_vow:=str_action(addr_compr,what = "vow")]
  
  # take unique bafnos
  # bafnos <- baf_base_data[!is.na(appt_baf),bafno] %>% unique
  
  # main step of matching : slice index lists 3 times: 
  indx_compr <- get_match_index("addr_compr")
  indx_novow <- get_match_index("addr_rm_vow")
  indx_nodb <- get_match_index("addr_rm_doub")
  
  list(indx_compr=indx_compr, 
       indx_novow = indx_novow,
       indx_nodb = indx_nodb
       )
}

# pipe function for removing surplus address columns - not fit for printing
remove_addr <- function(dt){
 dt[,.SD,.SDcols= str_subset(names(dt),pattern = "addr",negate = T)][]
}

# a roundabout way to reducing column width of ADDRESS without transforming the DT by reference
narrow_addr <- function(dt,colwid=40){
  colnames<- names(dt)
  names2 <- setdiff(colnames,"ADDRESS")
  dt[,.SD,.SDcols=names2][,ADDRESS:=str_sub(dt$ADDRESS,1,colwid)][]
}

# process google forms of volunteer feedback
proc_vol_qwforms <- function(dt = volgf){
  dt <- dt[,c(1:12)]
  setnames(dt,qc(ts,idvol,secret,qwid,success,mode,proof,tm_cont,feeling,mention,applicable,comments))
  dt <- map_at(dt,.at = qc(secret), as.numeric) %>% as.data.table
  dt <- map_at(dt,.at = qc(ts,tm_cont), parse_date_time,orders = "mdyHMS",tz="Asia/Kolkata") %>% as.data.table
  dt <- map_at(dt,.at = qc(dateQ), dmy) %>% as.data.table
  dt <- map_at(dt,.at = qc(success,mode,proof,feeling,mention,applicable),  as.factor) %>% as.data.table
  dt
}


# compact a list of data.tables: it's a general function - can be used anywhere
rm_z_nrows <- function(lofdts){
  allcounts <- lofdts %>% map_dbl(nrow)
  nzpos <- which(allcounts > 0)
  lofdts[nzpos]
}

# Prepare a list of data.tables for uploading to volunteer googlesheet
prep_list_patients <- function(cases = cases_31){
 cases %>% remove_addr %>% 
    #cbind(data.table(success="",mode="",proof="",time="",feeling="",mention="")) %>% 
    as_tibble %>% split(cases$cluster) %>% 
    rm_z_nrows()
}

# pass a named list of data tables to be loaded in separate worksheet tabs of a googlesheet
upload_vol_sheets <- function(sp=sp25,k=allocation_sheet){
    for(i in names(sp)){
      write_sheet(sp[[i]],ss = k,sheet = i)
    }
}

# now index calculation over rided the hard coding of short names: donot use this. Use apptindex
rm_shortnames <- function(dt,shortnames="^(ittin|rose|satya|alpine|aoane|opal)|tree"){
  dt[!grepl(shortnames,appt_baf,ig=T)]
}

cr_allposs_match <- function(baflist=baflist26,aindex=apptindx,expand_addr=F){
  dt_novow <- aindex[,.(bafno,passv)][baflist$indx_novow,on="bafno"][passv==T][,passv:=NULL]
  dt_nodbl <- aindex[,.(bafno,passd)][baflist$indx_nodb,on="bafno"][passd==T][,passd:=NULL]
  
  dt_strict <- baflist$indx_compr  %>% {if(expand_addr==F) remove_addr(.) else .} 
  dt_novowel <- baflist$indx_compr[,c(1:27)] %>% fsetdiff(dt_novow,.) %>% {if(expand_addr==F) remove_addr(.) else .}
  dt_nodouble <- baflist$indx_compr[,c(1:27)] %>% fsetdiff(dt_nodbl,.) %>% {if(expand_addr==F) remove_addr(.) else .}
  
  dt_strict[,data:="STRICT"]
  dt_novowel[,data:="NOVOWEL"]
  dt_nodouble[,data:="NODOUBLE"]
  
  rbind(dt_strict,dt_novowel,dt_nodouble) %>% unique(by=c("bafno","qwid")) %>% .[order(bafno)]
}

summ_counts <- function(allposs, voldt=vol2){
  allposs[,TOTCASES:=.N,appt_baf]
  cases <- allposs[,.N,by=.(appt_baf,TOTCASES,data)] %>% dcast(appt_baf + TOTCASES ~ data,fill=0)
  result <- voldt[,.N,by=.(appt_baf,flatcount)][cases,on="appt_baf"] %>% setnames(c("N"),c("Volunteers"))
  result
}

min_dist <- function(bafdt=bafgeo,bbdt=bb27_gcodes_isappt){
  seq_len(nrow(bbdt)) %>% map_dbl(~distGeo(as.matrix(bafdt[,.(lon,lat)]), bbdt[.x,.(lon,lat)] ) %>% min())
}


# Load BAF membership file into a DT :now direct from the dynamic googlesheet
load_members <- function(dt=NULL,ss=bafmembsheet){
  #dt1 <-read_excel(f, range=cell_cols(c(1,8)), col_types = "text") %>% setDT # only first 8 columns relevant.
  if(is.null(dt)) {
    drive_download(file = ss,type = "csv",overwrite = T)
    dt1 <- fread("BAF Member Database.csv")
    setnames(dt1,qc(bafno,appt_long,status,appt_baf,nblks,nflats,appt_addr,locty,city,pin,ph1,ph2,ward,acno,pcno,clustno,totrep,doj,fsales,maplink,clust_name,zonlead,target,ward_name,zone,assly))
  }
  else
    dt1 <- dt
  
  dt1 <- map_at(dt1,.at = qc(bafno,status,appt_baf,locty,city,status,ward,clustno,ward_name,zone,assly),  as.factor) %>% as.data.table
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
  compress_addr(dt1,"clean_appt_addr")
  1:3 %>% as.character() %>% walk2(c("I","II","III"),~replace_numeric(dt1,.x,.y))
  # dt1[,google_addr:= prep_addr(paste(clean_appt_addr,
  #                                  ifelse(is.na(locty),"",as.character(locty)),
  #                                  ifelse(is.na(pin),"",pin))),
      # by=clean_appt_addr]
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
add_appt_indx <- function(dt,indx=0.6,size=5){
  dt[,appt_vow := str_action(appt_baf,"vow")]
  dt[,appt_dbl := str_action(appt_baf,"dou")]
  dt[,ncvow:=str_split(appt_baf," ",simplify = T) %>%  str_action("vow") %>% nchar %>% paste(collapse =","),by=bafno]
  dt[,ncdbl:=str_split(appt_baf," ",simplify = T) %>%  str_action("dou") %>% nchar %>% paste(collapse =","),by=bafno]
  dt[,ncappt:=str_split(appt_baf," ",simplify = T) %>%  nchar %>% paste(collapse =","),by=bafno]
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

newcolorder <- function(dt){
  setcolorder(dt,c("qwid", "ptype", "name", "mob", "gender", "age", "dateQ","qdays", 
                   "appt_baf", "bafno", "ADDRESS", "bbmpzone", "ward", "region", "state", 
                   "valid_mob", "flatcount", "locty", "cluster", "zonlead", "wardno" 
                   ))
}

# check cache before firing for identical address
fire_geocode <- function(addr_str,gmast=geomaster){
  ex_geo <- gmast[gaddr %in% addr_str]
  message("detected ",nrow(ex_geo)," addresses existing in cache .. pulling them in")
  new_addr <- setdiff(addr_str,gmast$gaddr)
  new_geo <- geocode(new_addr) %>% cbind(data.table(gaddr=new_addr),.)
  rbind(ex_geo,new_geo)
}

# Read the latest EIDs of BAF volunteers
read_eid <- function(gsheet=volsh){
  eid <- read_sheet(gsheet,sheet = 2,range = "A:D") %>% setDT
  setnames(eid,qc(bafid,vol_fulid,volid,bafno))
  eid[,volid:=as.character(volid)]
}

# Download from latest paperform google sheet. Switch off (download=F) to just read a copy
read_paperform <- function(gsheet=paperformsheet,download=T){
  if(download) drive_download(file = paperformsheet,"paperform.csv",type = "csv",overwrite = T)
  fread("paperform.csv")
}

proc_paperform <- function(dt){
  setnames(dt[,c(1:22)],qc(subm,cqsid,attby,mode,ttype,hqid,breached,reason,fir,sympt,distt,zone_taluk,ward_panch,
                 comments,hq_addr_chg,new_addr,mob_chg,new_mob,distt_chg,zone_chg,ward_chg,photo_rem))
  dt[,hqid_upper:=toupper(hqid)]
  dt[,date_submitted:=parse_date_time(subm,orders = c("dmy","ymd HMS")) %>% as.Date()]
  dt[,cqcode:=str_sub(cqsid,-5)]
}

# this is the master merging function, of 4 databases: paperform, baf membership, covid cases, citizen volunteers, electronic ids to volunteers
# cases must have columns: bbmpzone; member must gave columns clust_name, cluster, 
merge_databases <- function(paperdt, member=bafmembs,cases=cases_10_aug,volntr=vol2,eid=eid_old,from=20200806,html=T){
  cqid <- eid[paperdt,on=.(volid=cqcode),nomatch=0]
  case_cnt <- cases[,.(bafno,qwid)][,.N,by=bafno]
  cq_u <- cqid[,.(volid,bafno)] %>% unique
  cq_u[,.N,bafno] -> cq_u_cnt
  names(cq_u_cnt) <- qc(bafno,Active)
  volcounts <- volntr[,.N,by=bafno]
  setnames(volcounts,qc(bafno,volnts))
  zonedt <- cases[,.(bafno,bbmpzone)] %>% unique
  zonedt <- zonedt[,.(bbmpzone=first(bbmpzone)),by=bafno] # select one bbmpzone, since many times same baf appt is mapped to a different bbmpzone
  x1 <- zonedt[cq_u_cnt,on="bafno"
         ][volcounts,on="bafno"
           ][case_cnt,on="bafno"
             ][member,on="bafno"
               ][cqid,on=.(bafno)][date_submitted>=ymd(from)
                                   ][,dat_rev:=fct_rev(format(date_submitted,"%b %d"))
                                     ][,bbmp_master:=as.character(N)] %>% 
  
    dcast(bafno + appt_baf + clust_name + ward_name + bbmpzone + volnts + Active + bbmp_master  ~ dat_rev,fill=NA) %>% 
    adorn_totals(where = c("row","col"),,,,contains("Aug")) %>% 
    {
      if(html==T)
        addHtmlTableStyle(.,align="llllr",col.columns= c(rep("none", 8),rep("#F0F0F0",25))) %>%  
        htmlTable(rnames=F,cgroup=c("","VOLUNTEERS","CASES","DATES","TOTAL"),n.cgroup=c(5,2,1,9,1),total = T) # make these numbers more robust by using total days columns
      else .
    }
}
