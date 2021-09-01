# contact management of BNP
library(tidyverse)
library(magrittr)
library(wrapr)
library(stringr)
library(janitor)
library(lubridate)
library(stringdist)
library(textclean)
library(rvest)
library(httr)
library(openxlsx)
library(readxl)
library(data.table)
library(googlesheets4)
library(htmlTable)

read_sajid_excel_files <- function()
        list.files(path = "BNP_users-website") %>% 
        map(~read_excel(path = paste0("BNP_users-website/",.x),skip = 1, col_names = T) %>% cbind(file=.x)) 


# renames the DT column by searching a pattern (old) and takes the first match and renames it with the new name (new)
stand_name <- function(dt,old,new){
        dt %<>% repair_names()
        if(names(dt) %>% str_subset("^.{1,20}$") %>% str_detect(regex(old,ignore_case = T)) %>% any){
                oldname1 <- names(dt) %>% str_subset(regex(old,ignore_case = T)) %>% .[1]
                setnames(dt,old=oldname1,new = new)
                return(dt)
        } else
                return(dt)
}

merge_name <- function(dt){
        fname <- names(dt) %>% str_subset(regex("first.*name",ignore_case = T)) %>% .[1]
        lname <- names(dt) %>% str_subset(regex("last.*name",ignore_case = T)) %>% .[1]
        if(!is.na(fname) & !is.na(lname)){
                dt2 <- dt %>% 
                        mutate(FullName=paste(coalesce(get(fname),""),coalesce(get(lname),""))) 
                
                setDT(dt2)
                setcolorder(dt2,"FullName")
                return(dt2)
        }
        else
                return(dt)
        
}

proc_sajid_data <- function(ldt){
        ldt %>% 
        map(~stand_name(.x,"Name","FullName")) %>% 
        map(~stand_name(.x,"Ward","Ward")) %>% 
        map(~map_at(.x,"Ward",as.character) %>% as.data.table) %>% 
        map(~stand_name(.x,"number|phone","Phone_number")) %>% 
        map(~stand_name(.x,"date|added|created","TimeStamp")) %>% 
        map(~map_at(.x,"TimeStamp",as.POSIXct) %>% as.data.table) %>% 
        bind_rows()
}

# a global time stamp converter - picks excel numbers as well date time strings of various sizes
as.time <- function(x){
        x1 <- as.character(x) %>% str_trim()
        if(all(is.na(x))) return(as.POSIXct(x1))
        x2 <- case_when(
                str_detect(x1,"^4[1-5]\\d{3}") ~ suppressWarnings(excel_numeric_to_date(as.numeric(x1),include_time = T) %>% as.POSIXct()) ,
                str_detect(x1,"2019|2020|2021") ~ suppressWarnings(parse_date_time(x1,orders = c("dmy","mdy","ymd","dmyHMS","mdyHMS","ymdHMS"),tz = "Asia/Kolkata") %>% as.POSIXct()),
                T ~ NA_POSIXct_
        )
        x2 %>% as.POSIXct()
}

proc_srik_data <- function(ldt){
        ldt %>% 
        map(~stand_name(.x,"Full.*Name","FullName")) %>% 
        map(~stand_name(.x,"first.*name","FirstName")) %>% 
        map(~stand_name(.x,"last.*name","LastName")) %>% 
        map(~stand_name(.x,"age","Age")) %>% 
        map(~stand_name(.x,"gender","Gender")) %>% 
        map(~stand_name(.x,"address","Address")) %>% 
        # map(~stand_name(.x,"Ward","Ward")) %>% 
        # map(~map_at(.x,"Ward",as.character) %>% as.data.table) %>% 
        map(~stand_name(.x,"mobile|number","Mobile_number")) %>% 
        map(~stand_name(.x,"created|timest|called","TimeStamp")) %>% 
        map(~map_at(.x,"TimeStamp",as.time) %>% as.data.table) %>% 
        map(~stand_name(.x,"mail","email")) %>% 
        map(~stand_name(.x,"apart|layout|commun|area","Community")) %>% 
        map(~stand_name(.x,"community.*type","Community_type")) %>% 
        map(~stand_name(.x,"houses","Houses")) %>% 
        map(~stand_name(.x,"remark|comment","Remarks")) %>% 
        map(~stand_name(.x,"source","Source")) %>% 
        bind_rows() %>% select(TimeStamp,FullName,FirstName,LastName,Age,Gender,Mobile_number,email,Address,Ward,Community,Community_type,Houses,Remarks,file,Source)
}


# list only those who have registered more than once, keeping phone number as grouping variable
summ_mult_reg <- function(dt){
        dt %>% filter(!grepl("test",FullName,ig=T)) %>% 
                group_by(Phone_number) %>% 
                summarise(count=n(),
                          name=paste(unique(FullName),collapse = ","),
                          email=paste(unique(Email),collapse = ","),
                          db=paste(unique(file),collapse = "; ")) %>% 
                filter(count>1) %>% 
                arrange(desc(count)) %>%  
                select(Phone_number,count,name,email,db)
}



#single function to pull Srikanth files into one DT with contacts
pull_srik_files <- function(){
        # pull srikanth's files
        read_srik_xls <- function(){
                list.files(path = "BNP_contact_files_srikanth",pattern = "*.xls") %>% 
                        map(~read_excel(path = paste0("BNP_contact_files_srikanth/",.x),col_types = "text") %>% 
                                    cbind(file=.x) %>% merge_name) 
        }
        
        read_srik_csv <- function(){
                list.files(path = "BNP_contact_files_srikanth",pattern = "*.csv") %>% 
                        map(~fread(file = paste0("BNP_contact_files_srikanth/",.x),colClasses = "character") %>% 
                                    cbind(file=.x) %>% merge_name) 
        }
        
        x1 <- read_srik_xls()
        x2 <- read_srik_csv()
        c(x1,x2) -> x3
        proc_srik_data(x3) -> x3a
        x4 <- 
                x3a %>% 
                left_join(ward_master %>% 
                                  select(ward_no,ward_name) %>% 
                                  mutate(wstr=as.character(ward_no)),by=c(Ward="wstr")) %>% 
                select(-c("ward_no"))
        x4
}

# pass the final contacts through this to see a summary
summ_contacts <- function(srik_cont=srik_cont)
srik_cont[,.(TotCount=.N,
             Nemails=sum(ifelse(!is.na(email),1,0)),  
             FNames=sum(ifelse(!is.na(FirstName),1,0)),  
             LNames=sum(ifelse(!is.na(LastName),1,0)),  
             NwithWards=sum(case_when(is.na(Ward) ~ 0,Ward=="0" ~ 0, T ~ 1)),
             NRem=sum(case_when(is.na(Remarks) ~ 0, T ~ 1)),
             Starting=first(range(as.Date(TimeStamp),na.rm = T)),
             Ending=last(range(as.Date(TimeStamp),na.rm = T)),
             NAddr=sum(ifelse(!is.na(Address),1,0)),
             NPincodes=sum(ifelse(!is.na(Pincode),1,0)),
             Nfiles=uniqueN(na.omit(file))),Source]

clean_contacts <- function(srik_cont){
        srik_cont[,Gender:=case_when(grepl("^M",Gender,ig=T) ~ "M", grepl("^F",Gender,ig=T) ~ "F", grepl("^T|^O",Gender,ig=T) ~ "T") %>% as.factor()]
        srik_cont[,Age := Age %>% as.numeric]
        srik_cont[,Age := ifelse(Age>100 | Age<=0,NA,Age)]
        srik_cont[,Pincode:=str_extract(Address,"56\\d{4}")]
}
