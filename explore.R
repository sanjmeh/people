# voda and airtel spreadsheet analysis
library(data.table)
library(tidyverse)
library(magrittr)
library(readxl)
library(googlesheets4)
library(googledrive)
library(stringr)
if(!interactive()) print("exlore.R loaded in background")
#w85 <- read_excel("~/Downloads/W-85.xlsx")
# x1 <- fread("~/Downloads/Voda 37.csv")
# x2 <- fread("~/Downloads/Airtel 37.csv")
# validmobs <- w85[nchar(`HEADOF HHD MOBILENO`)==10,`HEADOF HHD MOBILENO`] %>% unique %>% str_subset("^[6-9]") 

#x1 <- fread("select_add_W-85_level4.csv")

# After loading all telecom csv files in a subfolder in current dir run this
# code to pick each file and read them into a common data.table that is the
# output
read_telecom_files <- function(subdir = "w66"){
    x2 <- list.files(path = subdir,pattern = ".csv",full.names = T) %>% 
        {set_names(.,value = .)} %>% 
        map(fread,colClasses = "character") %>% 
        rbindlist(x1,fill=T,idcol = "filepath")
}

# pass the output of the unified database through this function to flag records
# on the desired pincodes and also type converts a few columns
clean_dt_telecom <- function(x2,flagpin = c(560010,560021,560055)){
    x2[,pin:=str_extract(local_address,"5\\d{5}")]
    x2[,inside := pin %in% flagpin]
    x2[,date_of_birth:=parse_date_time(date_of_birth,orders = c("dmy","mdy","ymd")) %>% as_date]
    x2
}

# another way to read several files with file column that does not rely on rbindlist idcol.
comb_lvls <- function(files){
    files %>% map(~fread(.x)[,file:=.x]) %>% rbindlist(fill = T)
}


combine_files <- function(dirpath){
dt1 <- list.files(path = dirpath,pattern = "lev\\d.csv",full.names = T) %>% comb_lvls()
dt1[,lev:=str_extract(file,"\\d.csv") %>% str_extract("\\d")]
# Once the operator column is added by name "oper" in the initial clustering we
# can remove this line
dt1[,oper:=str_extract(file,"(?<=/).*(?=_)")]
dt1[,mobile:=as.character(`Phone number`)]
}



# add a new column to the combined dt that is apartment name with the sequence
# ready for dedupe output will be for a certan level (4 is preferred)
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

# run and save all 4 matches - this will take time
batch_exec <- function(){
    bld$addr_compr %>% {set_names(.,.)} %>% map(~str_subset(peop1$addr_compr,.x)) %>% compact %>% saveRDS("tmp/x1_compr.RDS")
    bld$addr_rm_vow %>% {set_names(.,.)} %>% map(~str_subset(peop1$addr_rm_vow,.x)) %>% compact %>% saveRDS("tmp/x1_novo.RDS")
    bld$addr_rm_doub %>% {set_names(.,.)} %>% map(~str_subset(peop1$addr_rm_doub,.x)) %>% compact %>% saveRDS("tmp/x1_nodbl.RDS")
    bld$addr_rm_vow_and_doub %>% {set_names(.,.)} %>% map(~str_subset(peop1$addr_rm_vow_and_doub,.x)) %>% compact %>% saveRDS("tmp/x1_noboth.RDS")
}

# Pass the google drive path from where to download all csv files into the dest
# subdirectory which should be empty at this time
download_matched_files <- function(drivepath = "https://drive.google.com/drive/folders/1mBNPzKsMNb85IELi1k7Eas9O_aQvRWo-", dest = "matchedcsv"){
    setDT(lsraw)
    if(!grepl(dest,getwd())) setwd(dest)
    cat("Starting to read all files in google drive link...")
    lsraw <- googledrive::drive_ls(drivepath)
    stopifnot(list.files(".") %>% length() == 0)
    cat("done")
    tryCatch({
    csvfile_ids <- lsraw[grepl("csv$",name),id]
    csvfile_ids %>% map(drive_download)
    },finally = setwd(".."))
    setwd("..")
}

# change into the sub directory containing csv files with the matched people
# database downloaded in community level csv files. On executing this function,
# a zip file will be created int he current directory with the whatsApp template
# ( as saved in ~/Downloads) and the same number of csv files.
to_whatsapp_template <- function(n = "ALL"){
    wcols = fread("~/Downloads/new_contacts_bnp_template.csv") %>% names 
    x1 <- 
        list.files(pattern = ".csv$") %>% {if(n!= "ALL") .[seq_len(n)] else .} %>% 
        set_names(.,.) %>% 
        map(fread) %>% 
        imap(~rename(.x, mobile_number = phone_number) %>% 
                 mutate(groups = .y,salutation = map(name,str_to_name)) %>% 
                 select(wcols))
    if(dir.exists("tmp")){
        message("Existing tmp directory will be overwritten if any filename is repeated")
        if(readline("Proceed y/n?: ") =="n")
             stop("Aborting")
    } else
        dir.create("tmp")
    cat("Started processng..")
    x1 %>% imap(~fwrite(.x,file = paste("tmp/WA",.y,sep = "_")))
    cat("DONE")
    cat("\nzipping files...")
    zip(zipfile = "whatsapp.zip",files = list.files(path = "tmp",full.names = T))
    cat("zipped and saved in current directory")
}

# search a string vector v1 in another vector v2 and if v1 found in v2 prefer that else prefer the first element of v1
takefirst <- function(v1,v2){
    if(any(v1 %in% v2)) return(v1[v1 %in% v2][1]) else return(v1[1])
}

# transforms a string vector into a name (single word) and comma and outputs a
# table with two columns : original name and the name to be used for whatsapp
# messages
str_to_name <- function(x,msize = 3){
    y <- str_remove_all(x,"[:punct:]*") %>% str_trim # remove all punctuation and trim of leading and trailing spaces
    words <- str_split(y,"\\s+") # Split the name string on white spaces
    lc_words <- words %>% map(tolower) # Convert all to lower case
    
    # remove all likely salutation strings
    clean_lc_words <- 
        lc_words %>% 
        map(
            ~ { 
                if(.x[1] %in%  c("mr","mrs","ms","dr", "prof","smt", "shri","sri") )
                return(.x[-1]) else
                    return(.x)
            }
            )
    # Take the first long word as the name for salutation (default max size
    # argument has a value equal to 3). Convert the name to title case and
    # output a data.table with original and final name
    first_long_word <- clean_lc_words %>% map(~.x[nchar(.x) > msize][1]) %>% str_to_title() %>% paste0(",")
    data.table(orig = x, salut = first_long_word)
}


# change into the sub directory containing csv files with the matched people
# database downloaded in community level csv files. On executing this function,
# a summary csv will be created community name wise.
files_summary <- function(n = "ALL"){
    x1 <- 
        list.files(pattern = ".csv$") %>% {if(n!= "ALL") .[seq_len(n)] else .} %>% 
        set_names(.,.) %>% 
        map(fread,colClasses = "character") %>% 
        rbindlist(fill=T,idcol = "filepath")
    x1
}
