library(academictwitteR)
library(dplyr)
library(lubridate)
library(ggplot2)
library(statsgrokse)
library(wikipediatrend)
library(ggplot2)
library(zoo)



##### WIKIPEDIA #######

is = wp_trend(                        
  "Carpobrotus edulis",                              
  from = "2005-01-01", 
  to = "2021-10-01")

#colour theme 

branded_colors <- list(
  "blue"   = "#00798c",
  "red"    = "#d1495b",
  "yellow" = "#edae49",
  "green"  = "#66a182",
  "navy"   = "#2e4057", 
  "grey"   = "#8d96a3"
)

is$yearmon<- as.yearmon(is$date)

is %>% group_by(yearmon) %>% 
  dplyr::summarise(across(where(is.numeric), list(mean = mean, sum = sum))) %>%
  ggplot(.,aes(x = yearmon, y = views_sum)) + 
  geom_point(colour="#edae49")+ theme_linedraw()+ geom_smooth(colour="#00798c", method = "loess")+
  labs(
    x =" ", 
    y = "Total views")+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank()) 

ggplot(is) + 
  geom_line(aes(x = date, y = views),colour="#00798c") + theme_linedraw()+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank()) +
  labs(title="Wikipedia page views for 'invasive species'",
       x =" ", 
       y = "views")

##### TWITTER #######

# specify query and date range to pull 
query = "Carpobrotus edulis"
start_tweets = "2006-10-01T00:00:00Z"
end_tweets = "2021-10-01T00:00:00Z"
bearer= "AAAAAAAAAAAAAAAAAAAAAIMWTgEAAAAADtbtu9ODnL0x7fMS%2B5RVpSmN2mA%3Dfn7LEknPNl1R8eNyy5dKq1Hep7JnQsa8mjnqQnqiaGPReCJHAm"

# get counts of tweets with query per day
tweet_counts <- count_all_tweets(
  query = query,
  start_tweets = start_tweets,
  end_tweets = end_tweets,
  n = 365*15,
  bearer_token = bearer)

## 5 mins for 2458 with pub wifi 

tweet_pull <- get_all_tweets(
  query = query,
  start_tweets = "2006-10-01T00:00:00Z",
  end_tweets = "2021-10-01T00:00:00Z",
  bearer_token = bearer,
  n = 100)


####### REDDIT ######

library(tidyverse)
library("RedditExtractoR")

t<- find_subreddits("invasive species")

t2<- find_thread_urls("carpobrotus edulis", period="all")
t3<- find_thread_urls("bambusa vulgaris", period="all")
t4<- find_thread_urls("aloe vera", period="all")


