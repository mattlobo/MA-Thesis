## MATHEUS' MA THESIS ##

# ---
# Chapter 3 - DEATH DISTRIBUTION METHODS

# ---
# SCRIPT: loading the necessary R Packages and scripts that deal with preliminary data

  library(pacman)
  p_load(readr, ipumsr, read.dbc, PNADcIBGE, tidyverse, survey, skimr, DDM)
  
  setwd("/Users/mlobo/Documents/GitHub/MA-Thesis/0 - New Approach")
  source("0.0_SCRIPT - POPULATION DATA.R")
  
  setwd("/Users/mlobo/Documents/GitHub/MA-Thesis/0 - New Approach")
  source("0.2_SCRIPT - MORTALITY DATA.R")
  
# ---
# SCRIPT: removing unnecessary data
  
  rm(distRACECOLORandAGEGROUP_processdata,
     popEstimates_data_THREE_MORTALITY, deathEstimates_data_THREE, popEstimates_data_THREE_PLOT,
     popEstimates_data_TWO_MORTALITY, popEstimates_data_TWO_PLOT, deathEstimates_data_TWO)
  
# ---
# SCRIPT: building the input for the "ddm" R function with three RACE_COLOR groups

# STRUCTURE: $pop1 (integer), $pop2 (integer), $deaths (numeric), 
# STRUCTURE (cont.): $date1 (integer/date), $date2 (integer/date), $age(integer), $sex (character), and $cod (integer/character)

  pop1 <- filter(popEstimates_data_ALL_MORTALITY, YEAR == 2010)
  pop1$GENDER <- case_match(pop1$GENDER,
                            "Male" ~ "m",
                            "Female" ~ "f")
  
  pop2 <- filter(popEstimates_data_ALL_MORTALITY, YEAR == 2019)
  pop2$GENDER <- case_match(pop2$GENDER,
                            "Male" ~ "m",
                            "Female" ~ "f")
  
  DDM_data_ALL <- inner_join(pop1, pop2, by = join_by(AGE_GROUP, GENDER)); rm(pop1, pop2)
  
  deaths <- deathEstimates_data_ALL %>% 
    group_by(AGE_GROUP, GENDER) %>% 
    summarise(MEAN = mean(TOTAL))
  deaths$GENDER <- case_match(deaths$GENDER,
                              "Male" ~ "m",
                              "Female" ~ "f")
  
  DDM_data_ALL <- inner_join(DDM_data_ALL, deaths, by = join_by(AGE_GROUP, GENDER)); rm(deaths)

# ---
# SCRIPT: creating age groups [0,1); [1,4]; [5,9] ... for building the life table with three RACE_COLOR groups (White, Mixed, Black)

  find_replace <- tibble(LOWER = c(0, 1, seq(5, 85, by = 5)),
                         UPPER = c(1, 4, seq(9, 89, by = 5))) %>%
    unite(AGE_GROUP, LOWER:UPPER, sep = "-", remove = FALSE) %>%
    mutate(AGE_GROUP = ifelse(AGE_GROUP == "85-89", "85+", AGE_GROUP))
  
  DDM_data_ALL <- inner_join(DDM_data_ALL, find_replace, by = join_by(AGE_GROUP)); rm(find_replace)

# ---
# SCRIPT: making adjustments to the input of the ddm R function with three RACE_COLOR groups (White, Mixed, Black)

  DDM_data_ALL <- DDM_data_ALL %>%
    ungroup() %>% 
    rename("date1" = "YEAR.x",
           "sex" = "GENDER",
           "pop1" = "TOTAL.x",
           "date2" = "YEAR.y",
           "pop2" = "TOTAL.y",
           "deaths" = "MEAN",
           "age" = "LOWER") %>% 
    select("pop1", "pop2", "deaths", "date1", "date2", "age", "sex") %>% 
    mutate(RACE_COLOR = paste(sex),
           date1 = as.Date("01/07/2010", format = "%d/%m/%Y"),
           date2 = as.Date("01/07/2019", format = "%d/%m/%Y")) %>% 
    rename("cod" = "RACE_COLOR")

# ---
# SCRIPT: running the ddm R function with three RACE_COLOR groups (White, Mixed, Black) and storing the results

  results_ALL <- ddm(DDM_data_ALL)

# ---
# SCRIPT: diagnostics plots for females

  ggbChooseAges(DDM_data_ALL[DDM_data_ALL$cod == "f", ])
  segplot(DDM_data_ALL[DDM_data_ALL$cod == "f", ])

# ---
# SCRIPT: diagnostics plots for males

  ggbChooseAges(DDM_data_ALL[DDM_data_ALL$cod == "m", ])
  segplot(DDM_data_ALL[DDM_data_ALL$cod == "m", ])
  