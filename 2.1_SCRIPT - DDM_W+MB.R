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
# SCRIPT: building the input for the "ddm" R function with three RACE_COLOR groups

# STRUCTURE: $pop1 (integer), $pop2 (integer), $deaths (numeric), 
# STRUCTURE (cont.): $date1 (integer/date), $date2 (integer/date), $age(integer), $sex (character), and $cod (integer/character)

  pop1 <- filter(popEstimates_data_TWO_MORTALITY, YEAR == 2010)
  pop1$GENDER <- case_match(pop1$GENDER,
                            "Male" ~ "m",
                            "Female" ~ "f")
  
  pop2 <- filter(popEstimates_data_TWO_MORTALITY, YEAR == 2019)
  pop2$GENDER <- case_match(pop2$GENDER,
                            "Male" ~ "m",
                            "Female" ~ "f")
  
  DDM_data_TWO <- inner_join(pop1, pop2, by = join_by(AGE_GROUP, GENDER, RACE_COLOR)); rm(pop1, pop2)
  
  deaths <- deathEstimates_data_TWO %>% 
    group_by(AGE_GROUP, GENDER, RACE_COLOR) %>% 
    summarise(MEAN = mean(TOTAL))
  deaths$GENDER <- case_match(deaths$GENDER,
                              "Male" ~ "m",
                              "Female" ~ "f")
  
  DDM_data_TWO <- inner_join(DDM_data_TWO, deaths, by = join_by(AGE_GROUP, GENDER, RACE_COLOR)); rm(deaths)

# ---
# SCRIPT: creating age groups [0,1); [1,4]; [5,9] ... for building the life table with three RACE_COLOR groups (White, Mixed, Black)

  find_replace <- tibble(LOWER = c(0, 1, seq(5, 85, by = 5)),
                         UPPER = c(1, 4, seq(9, 89, by = 5))) %>%
    unite(AGE_GROUP, LOWER:UPPER, sep = "-", remove = FALSE) %>%
    mutate(AGE_GROUP = ifelse(AGE_GROUP == "85-89", "85+", AGE_GROUP))
  
  DDM_data_TWO <- inner_join(DDM_data_TWO, find_replace, by = join_by(AGE_GROUP)); rm(find_replace)

# ---
# SCRIPT: making adjustments to the input of the ddm R function with three RACE_COLOR groups (White, Mixed, Black)

  DDM_data_TWO <- DDM_data_TWO %>%
    ungroup() %>% 
    rename("date1" = "YEAR.x",
           "sex" = "GENDER",
           "pop1" = "TOTAL.x",
           "date2" = "YEAR.y",
           "pop2" = "TOTAL.y",
           "deaths" = "MEAN",
           "age" = "LOWER") %>% 
    select("pop1", "pop2", "deaths", "date1", "date2", "age", "sex", "RACE_COLOR") %>% 
    mutate(RACE_COLOR = paste(sex, "-", RACE_COLOR),
           date1 = as.Date("01/01/2010", format = "%d/%m/%Y"),
           date2 = as.Date("01/01/2019", format = "%d/%m/%Y")) %>% 
    rename("cod" = "RACE_COLOR")

# ---
# SCRIPT: running the ddm R function with three RACE_COLOR groups (White, Mixed, Black) and storing the results

  results_TWO <- ddm(DDM_data_TWO)

# ---
# SCRIPT: diagnostics plots for the RACE_COLOR = White

  ggbChooseAges(DDM_data_TWO[DDM_data_TWO$cod == "f - White", ])
  segplot(DDM_data_TWO[DDM_data_TWO$cod == "f - White", ])
  
  ggbChooseAges(DDM_data_TWO[DDM_data_TWO$cod == "m - White", ])
  segplot(DDM_data_TWO[DDM_data_TWO$cod == "m - White", ])

# ---
# SCRIPT: diagnostics plots for the RACE_COLOR = Mixed_Black

  ggbChooseAges(DDM_data_TWO[DDM_data_TWO$cod == "f - Mixed_Black", ])
  segplot(DDM_data_TWO[DDM_data_TWO$cod == "f - Mixed_Black", ])
  
  ggbChooseAges(DDM_data_TWO[DDM_data_TWO$cod == "m - Mixed_Black", ])
  segplot(DDM_data_TWO[DDM_data_TWO$cod == "m - Mixed_Black", ])
  