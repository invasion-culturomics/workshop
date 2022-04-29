#### set-up ####

# load packages
library(gtrendsR)
library(tidyverse)
library(countrycode)

# import data
dat <- read_csv("data/GLONAF/Taxon_x_List_GloNAF_vanKleunenetal2018Ecology.csv")
regions <- read_csv("data/GLONAF/Region_GloNAF_vanKleunenetal2018Ecology.csv")
countries2 <- as_tibble(countries) # from gtrendsR package


#### edit data ####

# convert GLONAF codes from ISO-3 to ISO-2
regions2 <- regions %>%
  mutate(country_code = countrycode(country_ISO, origin = 'iso3c', destination = 'iso2c'),
         country_code = case_when(country_ISO == "ANT" & 
                                    country == "Netherlands Antilles" ~ "AN", # addresses warning
                                  TRUE ~ country_code))

# check for matching
regions2 %>%
  anti_join(countries2 %>%
              select(country_code)) %>%
  select(country_ISO, country, country_code)
# should return 0

# select alien taxa
# add country info
dat2 <- dat %>% filter(status == "alien") %>%
  left_join(regions2 %>%
              select(region_id, country_code) %>%
              unique())

# select random taxa
# set.seed(10)
# dat_sub <- dat2[sample(1:nrow(dat2), 5),]
# all had NA time series

# select taxa in US
dat_sub <- dat2 %>%
  filter(country_code == "US")

# random selection
set.seed(101)
dat_sub2 <- dat_sub[sample(1:nrow(dat_sub), 5),]


#### single species ####

# google trends
dat_temp <- gtrends(dat_sub2$standardized_name[5], time = "all", geo = "US")

# format interest over time
dat_temp2 <- dat_temp$interest_over_time %>%
  as_tibble() %>%
  mutate(days = difftime(date, min(date), units = "days"))

# fit model
mod <- lm(hits ~ days, data = dat_temp2)
summary(mod)

ggplot(dat_temp2, aes(x = days, y = hits)) +
  geom_line()

ggplot(dat_temp2, aes(x = days, y = hits)) +
  geom_point() +
  geom_smooth(method = "lm")


#### five species ####

# gtrends wrapper function
gtrends_slope <- function(taxon, location){
  
  dat_temp <- gtrends(taxon, time = "all", geo = location)
  
  if(is.null(dat_temp$interest_over_time)){
    
    return(NA_real_)
    
  } else {
    
    dat_temp2 <- dat_temp$interest_over_time %>%
      as_tibble() %>%
      mutate(days = difftime(date, min(date), units = "days"))
    
    mod <- lm(hits ~ days, data = dat_temp2)
    
    slope <- as.numeric(coef(mod)[2])
    
    return(slope)
    
  }
  
}

# apply function across dataset
dat_sub3 <- dat_sub2 %>%
  mutate(gt_slope = map2(standardized_name, country_code, gtrends_slope)) %>%
  unnest(gt_slope)

dat_sub3 %>%
  select(standardized_name, country, gt_slope)
