## MATHEUS' MA THESIS ##

# ---
# Chapter 3 - POPULATION DATA
# ---
# SCRIPT: loading the necessary R Packages

library(pacman)
p_load(readr, ipumsr, read.dbc, PNADcIBGE, tidyverse, survey, skimr, DDM)

# ---
# SCRIPT: downloading (or locally loading) micro data

# DATA SOURCE: 1991, 2000, and 2010 Brazilian CENSUS - obtained from IPUMS
  
  setwd("/Users/mlobo/Documents/GitHub/MA-Thesis/IBGE/IPUMS")
  # 
  # DDI <- read_ipums_ddi("ipumsi_00005.xml")
  # popEstimatesIPUMS <- read_ipums_micro(DDI); rm(DDI)
  # 
  # popEstimatesIPUMS <- popEstimatesIPUMS %>%
  #   #filter(GEO1_BR == 76035) %>%
  #   count(GEO1_BR, YEAR, AGE, SEX, wt = PERWT)
  # 
  # write_excel_csv(popEstimatesIPUMS, "popEstimatesIPUMS_growthRate.csv", na = "NA", delim = ",")
 
# ---
# SCRIPT: reading some previously downloaded data from a local folder and making minor adjustments
  
  popEstimatesIPUMS <- read_csv("popEstimatesIPUMS_growthRate.csv")
  
  popEstimatesIPUMS <- popEstimatesIPUMS %>%
    rename("YEAR" = "YEAR",
           "AGE" = "AGE",
           "GENDER" = "SEX",
           "TOTAL" = "n")
  
  popEstimatesIPUMS$GENDER <- case_match(popEstimatesIPUMS$GENDER,
                                 1 ~ "Male",
                                 2 ~ "Female")
  
  popEstimatesIPUMS <- tibble(popEstimatesIPUMS)

# ---
# SCRIPT: creating age groups [0,1); [1,4]; [5,9] ... for calculating the growth rates

  popEstimatesIPUMS <- popEstimatesIPUMS %>%
    arrange(., YEAR, AGE) %>%
    mutate(INTERVAL = findInterval(AGE, seq(0, 85, by = 5)) - 1)
  
  find_replace_MORTALITY <- tibble(INTERVAL = unique(popEstimatesIPUMS$INTERVAL),
                         LOWER = seq(0, 85, by = 5),
                         UPPER = seq(4, 90, by = 5)) %>%
    unite(AGE_GROUP, LOWER:UPPER, sep = "-", remove = TRUE) %>%
    mutate(AGE_GROUP = ifelse(AGE_GROUP == "85-89", "85+", AGE_GROUP))
  
  popEstimatesIPUMS <- inner_join(popEstimatesIPUMS, find_replace_MORTALITY, by = "INTERVAL") %>% 
    mutate(AGE_GROUP = as_factor(AGE_GROUP)) %>% 
    group_by(GEO1_BR, YEAR, AGE_GROUP, GENDER) %>% 
    summarise(., TOTAL = sum(TOTAL))
  
  popEstimatesIPUMS_BR <- popEstimatesIPUMS %>% 
    group_by(YEAR, AGE_GROUP, GENDER) %>% 
    summarise(TOTAL = sum(TOTAL))
  
  popEstimatesIPUMS_SP <- popEstimatesIPUMS %>%
    filter(GEO1_BR == 76035) %>% 
    group_by(YEAR, AGE_GROUP, GENDER) %>% 
    summarise(TOTAL = sum(TOTAL))

  rm(find_replace_MORTALITY)
  
# ---
# SCRIPT: computing the growth rates at the NATIONAL LEVEL by age and gender
  
  Pop_91 <- filter(popEstimatesIPUMS_BR, YEAR == 1991)
  
  Pop_00 <- filter(popEstimatesIPUMS_BR, YEAR == 2000)
  
  Pop_10 <- filter(popEstimatesIPUMS_BR, YEAR == 2010)

  # GrowthRateBR_91_00 <- inner_join(Pop_91, Pop_00, join_by("AGE_GROUP", "GENDER")) %>% 
  #   rename("START_YEAR" = "YEAR.x",
  #          "START_POP" = "TOTAL.x",
  #          "END_YEAR" = "YEAR.y",
  #          "END_POP" = "TOTAL.y") %>% 
  #   mutate(GROWTH_RATE = (log(END_POP / START_POP))/(END_YEAR - START_YEAR)) %>% 
  #   select(GENDER, START_YEAR, END_YEAR, AGE_GROUP, START_POP, END_POP, GROWTH_RATE)

  GrowthRateBR_00_10 <- inner_join(Pop_00, Pop_10, join_by("AGE_GROUP", "GENDER")) %>% 
    rename("START_YEAR" = "YEAR.x",
           "START_POP" = "TOTAL.x",
           "END_YEAR" = "YEAR.y",
           "END_POP" = "TOTAL.y") %>% 
    mutate(GROWTH_RATE = (log(END_POP / START_POP))/(END_YEAR - START_YEAR)) %>%
    select(GENDER, START_YEAR, END_YEAR, AGE_GROUP, START_POP, END_POP, GROWTH_RATE)

  rm(Pop_91, Pop_00, Pop_10)  

  # write_excel_csv(GrowthRateBR_91_00, "GrowthRateBR_91_00.csv", na = "NA", delim = ",")
  # 
  # write_excel_csv(GrowthRateBR_00_10, "GrowthRateBR_00_10.csv", na = "NA", delim = ",")
  
# ---
# SCRIPT: computing the growth rates for SAO PAULO by age and gender
  
  Pop_91 <- filter(popEstimatesIPUMS_SP, YEAR == 1991)
  
  Pop_00 <- filter(popEstimatesIPUMS_SP, YEAR == 2000)
  
  Pop_10 <- filter(popEstimatesIPUMS_SP, YEAR == 2010)
  
  # GrowthRateSP_91_00 <- inner_join(Pop_91, Pop_00, join_by("AGE_GROUP", "GENDER")) %>% 
  #   rename("START_YEAR" = "YEAR.x",
  #          "START_POP" = "TOTAL.x",
  #          "END_YEAR" = "YEAR.y",
  #          "END_POP" = "TOTAL.y") %>% 
  #   mutate(GROWTH_RATE = (log(END_POP / START_POP))/(END_YEAR - START_YEAR)) %>%
  #   select(GENDER, START_YEAR, END_YEAR, AGE_GROUP, START_POP, END_POP, GROWTH_RATE)
  
  GrowthRateSP_00_10 <- inner_join(Pop_00, Pop_10, join_by("AGE_GROUP", "GENDER")) %>% 
    rename("START_YEAR" = "YEAR.x",
           "START_POP" = "TOTAL.x",
           "END_YEAR" = "YEAR.y",
           "END_POP" = "TOTAL.y") %>% 
    mutate(GROWTH_RATE = (log(END_POP / START_POP))/(END_YEAR - START_YEAR)) %>% 
    select(GENDER, START_YEAR, END_YEAR, AGE_GROUP, START_POP, END_POP, GROWTH_RATE)
  
  rm(Pop_91, Pop_00, Pop_10)  
  
  # write_excel_csv(GrowthRateSP_91_00, "GrowthRateSP_91_00.csv", na = "NA", delim = ",")
  # 
  # write_excel_csv(GrowthRateSP_00_10, "GrowthRateSP_00_10.csv", na = "NA", delim = ",")  
  
  rm(popEstimatesIPUMS, popEstimatesIPUMS_BR, popEstimatesIPUMS_SP)
  