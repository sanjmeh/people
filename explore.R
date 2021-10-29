# voda and airtel spreadsheet analysis
library(data.table)
library(tidyverse)
library(magrittr)
library(readxl)
library(googlesheets4)
library(googledrive)

#w85 <- read_excel("~/Downloads/W-85.xlsx")
# x1 <- fread("~/Downloads/Voda 37.csv")
# x2 <- fread("~/Downloads/Airtel 37.csv")
# validmobs <- w85[nchar(`HEADOF HHD MOBILENO`)==10,`HEADOF HHD MOBILENO`] %>% unique %>% str_subset("^[6-9]") 

#x1 <- fread("select_add_W-85_level4.csv")

read_belandur_files <- function(){
   x1 <- list.files("Bellandur",pattern = ".csv",full.names = T) %>% 
        {set_names(.,value = .)} %>% 
        map(fread,colClasses = list("character" = c("phone_number","alternate_phone") ))
   x2 <- rbindlist(x1,fill=T,idcol = "filepath")
    x2[,pin:=str_extract(local_address,"5\\d{5}")]
   x2[,inside := pin %in% c(560087,560037,560035,560103)]
   x2[,date_of_birth:=parse_date_time(date_of_birth,orders = c("dmy","mdy","ymd")) %>% as_date]
x2
}

comb_lvls <- function(files){
    files %>% map(~fread(.x)[,file:=.x]) %>% rbindlist(fill = T)
}


combine_files <- function(dirpath){
dt1 <- list.files(path = dirpath,pattern = "lev\\d.csv",full.names = T) %>% comb_lvls()
dt1[,lev:=str_extract(file,"\\d.csv") %>% str_extract("\\d")]
# Once the operator column is added by name "oper" in the initial clustering we can remove this line
dt1[,oper:=str_extract(file,"(?<=/).*(?=_)")]
dt1[,mobile:=as.character(`Phone number`)]
}



# add a new column to the combined dt that is apartment name with the sequence ready for dedupe
# output will be for a certan level (4 is preferred)
add_clust_seq <- function(dt,lv=4){
    dt2 <- dt[lev==lv]
    # Generate the sequence of apartments in ascending order of rows
    layoutseq <- dt2[lev==lv,.N,`Apartment name`][order(N)][,`Apartment name`]
    layoutseq %<>% factor(ordered = T,levels = layoutseq)
    names(layoutseq) <- layoutseq
    # create a new column that is a factor variable with intrinsic ordering in the order of count of records
    dt2[,aptfact:=layoutseq[`Apartment name`]]
}

dedupe_oper <- function(dt){
    if("aptfact" %in% names(dt)){
    dt %>% unique(by=c("mobile","aptfact"))
    } else message("Please run add_clust_seq() first on the combined dt")
}

# Remove duplicated rows across many clusters in the correct sequence established in the add_clust_seq() step
dedupe_cluster <- function(dt){
    if("aptfact" %in% names(dt)){
        dt[order(mobile,aptfact)] %>% unique(by=c("mobile"))
    } else message("Please run add_clust_seq() first on the combined dt")
}

# pass the deduped cluster wise dt along with the row binded operator DT deduped on mobile number, taking any operator at the top.
bind_subs_det <- function(dt,operatordt){
    if(nrow(operatordt) != uniqueN(operatordt,by="mobile")) {
        message("The operator DT has duplicates.. cleaning them")
        operatordt2 <- unique(operatordt,by="mobile")
    } else operatordt2 <- operatordt
    
    if( ! "name" %in% names(dt)){
        dt %>% left_join(operatordt2,by=c("mobile"))
    } else {
        message("Your combined cluster dt already has a name column.. hence aborting")
    }
}

# extract the rows from operator dt that are not in the cluster DT
extr_residue <- function(operatordt,clustdt){
    anti_join(operatordt,clustdt[,.(mobile)],by="mobile")
}

# pass the combined DT output from combine_files(); this function will create separate worksheets with name of apartment
# for uploading dryrun=F should be passed
upload_sheets <- function(dt,lv=4,ssid,dryrun=T){
    if(any(grepl("oper",names(dt)))){
        opervar <- grep("oper",names(dt),value = T)[1]
    } else stopifnot("Operator variable missing"= any(grepl("oper",names(dt))))
    dt1 <- dt[lev==lv,.SD,.SDcols=c("mobile","name","gender","Original address",'aptfact',opervar)][order(aptfact,name,mobile)]
    list1 <- dt1 %>% split(f = dt1$aptfact)
    if(dryrun==F) list1 %>% iwalk(~sheet_write(data = .x,ss = ssid,sheet = .y )) else
        list1
}

