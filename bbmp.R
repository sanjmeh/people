# covid citizen quarantine BBMP data management
# covers functions to process BAF volunteers, paperform output and daily assignment to volunteers

load("keys.data")
source("gfunctions.R")


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

# main function to merge bbmp data to baf member data
merge_baf <- function(bbmp,baf,volunt=F){
  
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
    if ("gender" %in% names(x1))
      x1[,.(bafno,appt_baf,appt_addr,lon,lat,qwid,ptype,qdays,name,age,gender,mob,dateQ,dateB,ADDRESS,
            google_addr,addr_compr,addr_rm_vow,addr_rm_doub,bbmpzone,ward,region,state,valid_mob,flatcount,locty,cluster,zonlead,wardno)]
    else
      x1[,.(bafno,appt_baf,appt_addr,lon,lat,qwid,ptype=NA,qdays=NA,name,age=NA,gender=NA,mob,dateQ=NA,dateB=NA,ADDRESS,
            google_addr,addr_compr,addr_rm_vow,addr_rm_doub,bbmpzone=NA,ward=sheet,region=NA,state=NA,valid_mob,flatcount,locty,cluster,zonlead,wardno)]
    }
  # get rid of junk first
  if(volunt==T) 
    baf_base_data <- baf[!is.na(appt_baf) & !grepl("Dup",bafno,ig=T),.(bafno,appt_baf,flatcount,locty,cluster,zonlead,wardno)] %>% unique 
  else
    baf_base_data <- baf
    
  baf_base_data[,addr_compr:=str_action(appt_baf,what = "punct")]
  baf_base_data[,addr_rm_doub:=str_action(addr_compr,what = "doub")]
  baf_base_data[,addr_rm_vow:=str_action(addr_compr,what = "vow")]
  
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

# process google forms of volunteer feedback - this was discontinued
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
  dt_novowel <- baflist$indx_compr %>% fsetdiff(dt_novow,.) %>% {if(expand_addr==F) remove_addr(.) else .}
  dt_nodouble <- baflist$indx_compr %>% fsetdiff(dt_nodbl,.) %>% {if(expand_addr==F) remove_addr(.) else .}
  
  dt_strict[,data:="STRICT"]
  dt_novowel[,data:="NOVOWEL"]
  dt_nodouble[,data:="NODOUBLE"]
  
  rbind(dt_strict,dt_novowel,dt_nodouble) %>% unique(by=c("bafno","qwid")) %>% .[order(bafno)]
}

# pipe final bbmp cases through this colorder for Arun's googlesheet
newcolorder <- function(dt){
  setcolorder(dt,c("qwid", "ptype", "name", "mob", "gender", "age", "dateQ","qdays", 
                   "appt_baf", "bafno", "ADDRESS", "bbmpzone", "ward", "region", "state", 
                   "valid_mob", "flatcount", "locty", "cluster", "zonlead", "wardno" 
  ))
}

# input is DT with cases, and DT with volunteers
summ_counts <- function(allposs, voldt=vol2){
  allposs[,TOTCASES:=.N,appt_baf]
  cases <- allposs[,.N,by=.(appt_baf,TOTCASES,data)] %>% dcast(appt_baf + TOTCASES ~ data,fill=0)
  result <- voldt[,.N,by=.(appt_baf,flatcount)][cases,on="appt_baf"] %>% setnames(c("N"),c("Volunteers"))
  result
}

min_dist <- function(bafdt=bafgeo,bbdt=bb27_gcodes_isappt){
  seq_len(nrow(bbdt)) %>% map_dbl(~distGeo(as.matrix(bafdt[,.(lon,lat)]), bbdt[.x,.(lon,lat)] ) %>% min())
}

# Read the latest EIDs of BAF volunteers
read_eid <- function(gsheet=volsh){
  eid <- read_sheet(gsheet,sheet = 2,range = "A:D") %>% setDT
  setnames(eid,qc(bafid,vol_fulid,volid,bafno))
  eid[,volid:=as.character(volid)]
}
#----- PAPER FORM ------
# Download from latest paperform google sheet. Switch off (download=F) to just read a copy
read_paperform <- function(gsheet=paperformsheet,download=T){
  if(download) drive_download(file = paperformsheet,"paperform.csv",type = "csv",overwrite = T)
  fread("paperform.csv")
}

proc_paperform <- function(dt){
  dt <- dt[-1,c(1:22)]
  setnames(dt,qc(subm,cqsid,attby,mode,ttype,hqid,breached,reason,fir,sympt,distt,zone_taluk,ward_panch,
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
  case_cnt[zonedt,on="bafno"][cq_u_cnt,on="bafno" # we start with case counts as we donot want case counts as the master - we want paperform calls as master
  ][volcounts,on="bafno"
  ][
  ][member,on="bafno"
  ][cqid,on=.(bafno)][date_submitted>=ymd(from)
  ][date_submitted>=ymd(from)
  ][,dat_rev:=fct_rev(format(date_submitted,"%b %d")) # new forcats function to reverse factors
  ][,bbmp_master:=as.character(N)] %>% 
    dcast(bafno + appt_baf + clust_name + ward_name + bbmpzone + volnts + Active + bbmp_master  ~ dat_rev,fill=NA) %>% 
    .[,c(1:17)] %>% 
    adorn_totals(where = c("row","col"),,,,contains("Aug")) %>% 
    {
      if(html==T)
        addHtmlTableStyle(.,align="llllr",col.columns= c(rep("none", 8),rep("#F0F0F0",25))) %>%
        htmlTable(rnames=F,cgroup=c("","VOLUNTEERS","CASES","DATES","TOTAL"),n.cgroup=c(5,2,1,9,1),total = T) # make these numbers more robust by using total days columns
      else .
    }
}


#-------- Processing of individual files------------
bindsheets <- function(file){
  sheets <- excel_sheets(file)
  names(sheets) <- sheets
  sheets %>% imap(~read_excel(file,sheet=.x)) %>% rbindlist(idcol = "sheet",fill=T)
}

# called on a group of files in a dir
# this may take some time: donot forget to save the output in a variable
read_files <- function(dir="baf/cqs"){
  files <- list.files(dir,full.names = T)
  names(files) <- list.files("baf/cqs",full.names = F)
  x1 <- files %>% imap(~bindsheets(.x)) %>% rbindlist(idcol = "file",fill = T)
  setnames(x1,qc(file,sheet,sno,qwid,name,mob,hqaddr,dt_raw,nvisits))
  x1[,dateQE := fifelse(is.na(as.numeric(dt_raw)), # date quarantine ends
                        parse_date_time(dt_raw,"dmy") %>% as.Date,
                        excel_numeric_to_date(dt_raw %>% as.numeric))]
  x1[,dt_raw:=NULL]
  
}

# equivalent of proc_bbmp_raw that was used for single master excel files read using load_bbmp_file()
# instead pass the DT that was obtained after reading all excel files using read_files()
proc_bbmp <- function(dt){
  dt[,ADDRESS := str_remove_all(hqaddr,",?\\bNULL\\b,?") ]
  dt[,mob:=str_action(mob,"punct")]
  dt[,valid_mob := mob %>% str_detect("^[1-5]|^\\d{1,9}$|^.{11,}$") %>% not]
  dt[,junk_addr:= nchar(ADDRESS)<20]
  dt[,isappt := str_detect(ADDRESS,regex("\\bflat|\\bapart|\\bapp\\b|\\bappart|society|\\bdoor\\b",ig=T))]
  dt[,addr_black:=str_action(ADDRESS,"near")]
  compress_addr(dt,"addr_black")
  dt[,google_addr:=prep_addr(ADDRESS)]
}




