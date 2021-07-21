

library(jobbR)
library(tidyverse)
library(rvest)
library(stringr)
library(slackr)
library(xlsx)
library(googlesheets4)
library(googledrive)



##GLOBALS 
{ 
  #slackr_setup(config_file = ".slackr")
  setwd("~/Desktop/indeed_daily_scraping")
  MY_ID <- as_id(drive_get("https://drive.google.com/drive/folders/1MgqQP9sZ0WOn0a2dQgCbf5h_cowPpxiM?usp=sharing"))
  
  #globals for functions, could change for other communities
  pubID <- 5447663788014200 #need to change if used externally
  LOCATION <- "05156"
  RADIUS <- 50
  running.title <- "springfield_udacity_jobs.csv"
  df.title <- "springfield_udacity_jobs.rds"
  
  #wrapper functions could merge into one 
  jobSearch2 <- function(query2){
    temp <- NULL
    tryCatch(
      expr = {
        temp <- jobSearch(publisher = pubID,
                          query = query2,
                          location = LOCATION,
                          radius = RADIUS,
                          limit = 25) %>% 
          select(results.jobtitle,
                 results.city,
                 results.state,
                 results.formattedLocation,
                 results.formattedLocationFull,
                 results.source,
                 results.date,
                 results.snippet,
                 results.jobkey)
      },
      error = function(cond){
        temp <- NA
      })
    
    return(temp)
  }
  jobSearchVector <- function(jobList){
    
    jobs.df <- NULL
    for(i in 1:length(jobList)){
      
      query.string <- jobList[i]
      df <- NULL
      df <- jobSearch2(query2 = query.string)
      jobs.df <- bind_rows(df, jobs.df)
    }
    return(jobs.df)
  }

  udacityDataTrack <- c("product+data+analyst",
                        "business+operations+analyst",
                        "business+analyst",
                        "junior+data+analyst",
                        "junior+business+analyst")
  
  udacityFrontendTrack <- c("digital+front-end+developer",
                            "web+application+developer",
                            "front+end+developer",
                            "front+end+engineer",
                            '"CSS+developer"',
                            '"HTML+developer"')
  
  udacityMarketingTrack <- c("SEO+manager",
                             "SEO+analyst", 
                             "SEO+partnership+specialist", 
                             "SEO+strategist",
                             "Social+media+analyst",
                             '"Social+media+manager"',
                             '"Social+media+community+manager"',
                             '"Social+media+coordinator"',
                             "Social+media+marketer",
                             '"Social+media+strategist"', 
                             "Google+adwords+and+analytics",
                             "SEM+analyst",
                             "SEM+manager",
                             "SEM+marketing+coordinator")
  
}
##get data from previous running list
prev.df <- read_csv(running.title)
#prev.df <- NULL

#jobs from that day from each track

#jobSearch2(udacityDataTrack[4]) %>% View()

##Get job data for all tracks
marketing.df <- jobSearchVector(udacityMarketingTrack)  %>% 
  mutate(Category = "Marketing")
data.df <- jobSearchVector(udacityDataTrack) %>% 
  mutate(Category = "Business Analytics")
front.df <- jobSearchVector(udacityFrontendTrack) %>% 
  mutate(Category = "Front-End Developer")

#combine into one df
daily.df <- rbind(marketing.df, data.df, front.df)

daily.title <- paste0("jobResults", Sys.Date(), ".csv")

###daily job posting
write_csv(daily.df,
          daily.title)


#this does not worl, I get a 400 error
write_sheet(daily.df,
            ss = MY_ID)


#This should keep track of all the unique jobs
running.df <- rbind(prev.df, daily.df) %>%
  distinct(results.jobkey, .keep_all = T)

##save cvs and rds locally for running list
write_csv(running.df,
          running.title)

write_rds(running.df,
          df.title)

quit(save = "no")




##testing
{
# employers.list <- running.df %>% 
#   group_by(results.source) %>% 
#   select(results.source,
#          results.formattedLocation) %>% 
#   add_count(name = "total")%>% 
#   unique() 

# running.df <- daily.df %>%
#   select(-query,
#          -Category,
#          -results.date,
#          -results.snippet) %>% 
#   unique()


# employers.list <- running.df %>%
#   group_by(results.formattedLocation,
#            results.source) %>%
#   select(results.source,
#          results.formattedLocation) %>%
#   add_count(name = "total")%>%
#   unique()

# employers.list <- employers.list[order(employers.list$total, decreasing= TRUE),]

# write.csv(employers.list,
#           "employerList.csv")

# data.df %>% select(-query) %>% unique() %>% View()


# SLACK STUFF
# 
# path <- paste0("~/Desktop/indeed_scraping/",csv_title)
# 
# slackr_upload(filename = path,
#               channel = "#fow-vista-r_integrations")
# 
# slackr_msg(txt = "foo",
#            channel = "")



#EARLY INDEED TESTS
#jobSearch(publisher = pubID, query = "farm") %>% View()

# temp <- jobSearch(publisher = pubID,
#                   query = "from",
#                   location = "05150",
#                   radius = 60,
#                   limit = 25)

# temp %>% select(results.jobtitle,
#                 results.company,
#                 results.formattedLocation,
#                 results.source,
#                 results.date,
#                 results.snippet,
#                 results.url,
#                 results.jobkey)

# temp <- jobSearch(publisher = pubID,
#                   query = "from",
#                   location = "VT",
#                   radius = 60,
#                   limit = 25)


# jobSearch(publisher = pubID, 
#           query = "farm",
#           location = "05150",
#           limit = 25) %>% 
#   
#   View()

# DataTrack <- c("data+analyst",
#                        "data+science",
#                        "data+scientist",
#                        "data+engineer",
#                        "business+analyst")

}




