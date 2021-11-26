source("places.R")
# Load the RDS file with circle centres allotted to you and inspect its
# contents. Change the name of the file as assigned to you before executing.
dtcen1 <- readRDS("dtcen.SM")
head(dtcen1) # print the top rows
dtcen1[,.N] # print the total count of rows

# Scraping starts here. We will divide the scraping into 10 sessions to keep your
# computer from getting locked for longer periods. You can adjust the row counts
# to longer periods basis the time you can keep the computer locked. the
# following commands are tuned at 1000 API calls per command and should take
# around an hour each. If you are leaving the computer for the night then I
# suggest fire around 5000 in one shot.

dtcen1[1:1000] %>% build_urls_nearby(rad = 100) %>% verbose_GET() -> r1
dtcen1[1001:2000] %>% build_urls_nearby(rad = 100) %>% verbose_GET() -> r2
dtcen1[2001:3000] %>% build_urls_nearby(rad = 100) %>% verbose_GET() -> r3
dtcen1[3001:4000] %>% build_urls_nearby(rad = 100) %>% verbose_GET() -> r4
dtcen1[4001:5000] %>% build_urls_nearby(rad = 100) %>% verbose_GET() -> r5
dtcen1[5001:6000] %>% build_urls_nearby(rad = 100) %>% verbose_GET() -> r6
dtcen1[6001:7000] %>% build_urls_nearby(rad = 100) %>% verbose_GET() -> r7
dtcen1[7001:8000] %>% build_urls_nearby(rad = 100) %>% verbose_GET() -> r8
dtcen1[8001:9000] %>% build_urls_nearby(rad = 100) %>% verbose_GET() -> r9
dtcen1[9001:nrow(dtcen1)] %>% build_urls_nearby(rad = 100) %>% verbose_GET() -> r10 # the ending row number is equal to total row count
save(r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,file = "resp.SM")
######## API EXECUTION COMPLETED ############

# the following commands will process one response object at a time and store
# each in a new data.table. Make sure you do not over write a data table with a
# new response by changing the suffix of variables e.g. dt1, dt2, etc in line
# with the response code

r1 %>% map(extr_all_places) %>% compact %>% rbindlist(fill = T) -> dt1
r2 %>% map(extr_all_places) %>% compact %>% rbindlist(fill = T) -> dt2
r3 %>% map(extr_all_places) %>% compact %>% rbindlist(fill = T) -> dt3
r4 %>% map(extr_all_places) %>% compact %>% rbindlist(fill = T) -> dt4
r5 %>% map(extr_all_places) %>% compact %>% rbindlist(fill = T) -> dt5
r6 %>% map(extr_all_places) %>% compact %>% rbindlist(fill = T) -> dt6
r7 %>% map(extr_all_places) %>% compact %>% rbindlist(fill = T) -> dt7
r8 %>% map(extr_all_places) %>% compact %>% rbindlist(fill = T) -> dt8
r9 %>% map(extr_all_places) %>% compact %>% rbindlist(fill = T) -> dt9
r10 %>% map(extr_all_places) %>% compact %>% rbindlist(fill = T) -> dt10
rbind(dt1,dt2,dt3,dt4,dt5,dt6,dt7,dt8,dt9,dt10) -> dt.SM
saveRDS(dt.SM,'dt.sm.rds')
##### DONE #########


