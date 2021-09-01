# kaggle database on covid
library(tidyverse)
library(janitor)
library(readxl)
library(magrittr)
library(stringr)
library(data.table)
library(googlesheets4)
library(googledrive)
library(lubridate)
library(wrapr)
library(ggrepel)

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