## MATHEUS' MA THESIS ##

# ---
# Chapter 3 - POPULATION DATA

# ---
# SCRIPT: loading the necessary R Packages and scripts that deal with preliminary data

  library(pacman)
  p_load(readr, ipumsr, read.dbc, PNADcIBGE, tidyverse, survey, skimr, DDM)
  
  setwd("/Users/mlobo/Documents/GitHub/MA-Thesis/0 - New Approach")
  source("0.0_SCRIPT - POPULATION DATA.R")
  
# ---
# SCRIPT: computing the growth rates for SAO PAULO by age and gender
  
  Pop_10 <- filter(popEstimates_data_THREE_PLOT, YEAR == 2010) %>% 
    group_by(., YEAR, AGE_GROUP, GENDER) %>% 
    summarise(TOTAL = sum(TOTAL))
  
  Pop_19 <- filter(popEstimates_data_THREE_PLOT, YEAR == 2019) %>% 
    group_by(., YEAR, AGE_GROUP, GENDER) %>% 
    summarise(TOTAL = sum(TOTAL))

  GrowthRateSP_10_19 <- inner_join(Pop_10, Pop_19, join_by("AGE_GROUP", "GENDER")) %>% 
    rename("START_YEAR" = "YEAR.x",
           "START_POP" = "TOTAL.x",
           "END_YEAR" = "YEAR.y",
           "END_POP" = "TOTAL.y") %>% 
    mutate(GROWTH_RATE = (log(END_POP / START_POP))/(END_YEAR - START_YEAR)) %>% 
    select(GENDER, START_YEAR, END_YEAR, AGE_GROUP, START_POP, END_POP, GROWTH_RATE)
  
  rm(Pop_10, Pop_19, 
     find_replace_MORTALITY,
     popEstimates_data_ALL_MORTALITY,
     popEstimates_data_THREE_MORTALITY, popEstimates_data_THREE_PLOT,
     popEstimates_data_TWO_MORTALITY, popEstimates_data_TWO_PLOT)  
