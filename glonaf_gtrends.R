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

# identify species to test functions
species <- tibble(standardized_name = c("Carpobrotus edulis", "Acacia dealbata", "Bromus arvensis", "Eichhornia crassipes"))

# add country info
# filter for test species
dat2 <- dat %>%
  left_join(regions2 %>%
              select(region_id, country_code) %>%
              unique()) %>%
  inner_join(species) %>%
  select(standardized_name, country_code) %>% # remove regions (smaller scale than country)
  unique()

# select 5 rows to try gtrends
dat_sub <- dat2[1:5,]

# select single species and location to try function
dat_sub2 <- dat2 %>%
  filter(standardized_name == "Eichhornia crassipes" &
           country_code == "US")


#### single species ####

# google trends
dat_temp <- gtrends(dat_sub2$standardized_name[1], time = "all", geo = dat_sub2$country_code[1])

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
dat_gt <- dat_sub %>%
  mutate(gt_slope = map2(standardized_name, country_code, gtrends_slope)) %>%
  unnest(gt_slope)

dat_gt
