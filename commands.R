library(data.table)
source("places.R")

### PLEASE EXECUTE COMMANDS IN INTERACTIVE MODE ONLY

### Read Google Cloud terms for free account here. Open a free account
####  https://cloud.google.com/free/docs/gcp-free-tier/#free-trial
#####


#### START #######
# Load the RDS file with circle centres allotted to you and inspect its
# contents. Change the name of the file as assigned to you before executing.

dtcen <- readRDS("dtcen.SM")
head(dtcen) # print the top rows
dtcen[,.N] # print the total count of rows


# Test if your API key is working by firing the first 3 calls.
dtcen[1:3] %>% build_urls_nearby(rad = 100) %>% verbose_GET() -> sample_response

# Just print one of the response:
sample_response[[2]]

# Or you can see a the response status with this:
sample_response[[1]] %>% http_status

# If you get the message: "Success" you are good to go.

###################################################################################
# Scraping starts here. You may divide the scraping into 5 to 10 sessions to keep your
# computer from getting locked for longer periods. You can adjust the row counts
# to longer periods basis the time you can keep the computer locked. the
# following commands are tuned at 1000 API calls per command and should take
# around an hour each. If you are leaving the computer for the night then I
# suggest fire around 4000 in one shot. 

dtcen[1:1000] %>% build_urls_nearby(rad = 100) %>% verbose_GET() -> r1
saveRDS(r1,"r1.RDS")   
# Using the saveRDS function you may save a response file in disk. If you think
# there is a risk of crash it is safer to save each response variable as soon as
# it is ready.
dtcen[1001:2000] %>% build_urls_nearby(rad = 100) %>% verbose_GET() -> r2
dtcen[2001:3000] %>% build_urls_nearby(rad = 100) %>% verbose_GET() -> r3
dtcen[3001:4000] %>% build_urls_nearby(rad = 100) %>% verbose_GET() -> r4
dtcen[4001:5000] %>% build_urls_nearby(rad = 100) %>% verbose_GET() -> r5
#Note : Please ensure you have credits remaining in your Google cloud platform
#before proceeding beyond 5000 API calls.
dtcen[5001:6000] %>% build_urls_nearby(rad = 100) %>% verbose_GET() -> r6
dtcen[6001:7000] %>% build_urls_nearby(rad = 100) %>% verbose_GET() -> r7
dtcen[7001:8000] %>% build_urls_nearby(rad = 100) %>% verbose_GET() -> r8
dtcen[8001:9000] %>% build_urls_nearby(rad = 100) %>% verbose_GET() -> r9
# the ending row number is equal to total row count, hence the below will flush
# to the last row.
dtcen[9001:nrow(dtcen)] %>% build_urls_nearby(rad = 100) %>% verbose_GET() -> r10 
######## MAIN API EXECUTION COMPLETED ############

# Let's save the responses in one response file. Please add your own initials in the filename.
saveRDS(list(r1,r2,r3,r4,r5,r6,r7,r8,r9,r10),file = "resp.SM.RDS")


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

# Now let's merge all tables into one
rbind(dt1,dt2,dt3,dt4,dt5,dt6,dt7,dt8,dt9,dt10) -> dt_all

# Now we will do PAGE 2 API calls. Google sends a "nextpage" token when the
# results in the first call are more than 20. So we will first identify which
# circle centers have returned the next page token and start next round of
# scrape using the following steps.


# Before starting we will see the total page 2 counts. Since a valid token is
# more than 35 characters, we will filter all rows that seems to have a valid
# nextpage token
dt_all[nchar(nextpage) > 35,.N]

#If the above count is smaller than 1000 you may just finish  off in one block
#command, Note : the next command is going to fire APIs again. Please ensure you
#have credits remaining in your Google cloud platform.
dt_all[nchar(nextpage) > 35,nextpage] %>% build_nextpage_urls() %>% verbose_GET() -> page2


# Now there could be some results with another page waiting, so we will test how many we have a next page.
page2 %>% map(extr_all_places) %>% compact() %>% rbindlist() -> dt_page2
dt_page2[,.N] # this will give the number of results in page 2
dt_page2[nchar(nextpage)>35,.N] # this will give the count of page 3 tokens available.

# If the count of page3 tokens is less than 1000 you can fire the following in one go.
dt_page2[nchar(nextpage)>35,nextpage]  %>% build_nextpage_urls() %>% verbose_GET() -> page3
page3 %>% map(extr_all_places) %>% compact() %>% rbindlist() -> dt_page3
dt_page3[,.N] # this will give the number of results in page 3 API calls.

# Now we will combine all results of page1,2 & 3 in one data.table and save only unique places
dt_allpages <- rbind(dt_all,dt_page2,dt_page3,fill = T) %>% unique(by = "placeid")
dt_allpages[,.N] # This will show the total results from your scrape.

# Now we will attach wards to each search
dt_allpages[,ward:=find_wards(dt_allpages)]
dt_allpages[,.N,ward][order(ward)]  # This will list ward wise count of results.
dt_allpages$name # This will show the names of apartments that you fetched.

# Save the full data.table into an RDS file
saveRDS(dt_allpages,"dt.SM.RDS") 
# Email the file to analytics@nammabnp.org 

##### THANK YOU #########


