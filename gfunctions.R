# global functions
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
        str1 <- addstr %>% str_replace_all("[#,&]"," ") %>% 
                str_trim %>% str_replace_all("\\s+","+")
        if(bangalore==T){
                str2 <- 
                        str1 %>% 
                        {ifelse(grepl("Bangalore|Bengaluru", .,ig=T), paste(.,"Karnataka",sep="+"),paste(.,"Bangalore+Karnataka",sep="+"))}
                
                str2 %<>% strsplit("\\s+") %>% map(paste,collapse="+") %>% unlist
        }
        else str2 <- str1
        return(str2)
        
}


getmap <- function(cen=center_ward46,zoom=15){
        get_googlemap(center = cen,
                      zoom=zoom,
                      size=c(640,640),
                      format="png8"
        )
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
# removed one addr_rm_vow_and_doub - as it is covered now in rm_vow
compress_addr <- function(dt,colname="ADDRESS"){
        dt[,addr_compr:=str_action(get(colname),"punct")] 
        dt[,addr_rm_doub:= str_action(addr_compr,"dbl")] # remove doubles before removing vowels
        dt[,addr_rm_vow:= str_action(addr_rm_doub,"vowel")] # remove vowels after doubles are removed
        dt[,google_addr:=prep_addr(get(colname))] # remember this will add Bangalore+Karnataka but not add pin code. For pin please add it in next step.
}