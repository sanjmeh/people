# analysie survey results
library(data.table)
library(tidyverse)
library(magrittr)
library(readxl)
library(googlesheets4)
library(googledrive)
library(ggplot2)

x1 <- fread("Lekka Beku - Form 1 (Responses).csv")

setnames(x1,c("ts","email","fullname","mob","alocward","nameleka","didspeak","wasNodOff","wcm","sept18","sixty","willing","remarks"))

head(x1)
x1[,ts:=mdy_hms(ts,tz = "Asia/Kolkata")]
x1[,didspeak:=factor(didspeak)]
x1[,wasNodOff:=factor(wasNodOff)]
x1[,wcm:=factor(wcm)]
x1[,sept18:=factor(sept18)]
x1[,sixty:=factor(sixty)]
x1[,willing:=factor(willing)]
x1[,mob2:=str_remove_all(mob,"[ \\-,]") %>% str_sub(-10)]
setkey(x1,mob2,alocward)
x2 <- unique(x1,by=key(x1))

