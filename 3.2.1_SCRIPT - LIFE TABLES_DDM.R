## MATHEUS' MA THESIS ##

# ---
# Chapter 3 - THE LIFE TABLE

# ---
# SCRIPT: loading the necessary R Packages and scripts that deal with preliminary data

  library(pacman)
  p_load(readr, ipumsr, read.dbc, PNADcIBGE, tidyverse, survey, skimr, DDM)

# ---
# SCRIPT: removing unnecessary data

  rm(popEstimates_data_THREE_MORTALITY, popEstimates_data_TWO_MORTALITY, 
     popEstimates_data_THREE_PLOT, popEstimates_data_TWO_PLOT,
     deathEstimates_data_THREE, deathEstimates_data_TWO,
     distRACECOLORandAGEGROUP_processdata)
  
# ---
# SCRIPT: preparing the input to run the life table function
  
  lifetable_data_ALL <- inner_join(filter(popEstimates_data_ALL_MORTALITY, YEAR == 2010),
                                   filter(popEstimates_data_ALL_MORTALITY, YEAR == 2019),
                                   join_by(AGE_GROUP, GENDER)) %>% 
    rename("YEAR_1" = "YEAR.x",
           "POP_1" = "TOTAL.x",
           "YEAR_2" = "YEAR.y",
           "POP_2" = "TOTAL.y") %>% 
    separate(., AGE_GROUP, c("LOWER", "UPPER"), remove = FALSE, convert = TRUE) %>% 
    mutate(N = ifelse(is.na(UPPER) == TRUE, 4, UPPER - LOWER),
           N = ifelse(LOWER > 0, N + 1, N),
           cod = GENDER)
  
  temp_data_MORTALITY_ALL <- deathEstimates_data_ALL %>% 
    group_by(AGE_GROUP, GENDER) %>% 
    summarise(DEATHS = sum(TOTAL))
  
  lifetable_data_ALL <- inner_join(lifetable_data_ALL,
                                   temp_data_MORTALITY_ALL,
                                   join_by(AGE_GROUP, GENDER)) %>% 
    select(AGE_GROUP, LOWER, N, GENDER, POP_1, POP_2, DEATHS, cod); rm(temp_data_MORTALITY_ALL)

# ---
# SCRIPT: generating plot of RAW mortality rates before any adjustments
      
  ggplot(data = lifetable_data_ALL, 
         mapping = aes(x = AGE_GROUP, y = log(DEATHS / (10 * sqrt(POP_1 * POP_2))), group = GENDER, color = GENDER)) +
    
    geom_line(linewidth = 0.8) +
    
    scale_colour_manual(values = c("#8D6A9F", "#00CC99"),
                        aesthetics = c("colour", "fill"),
                        labels = c("Female","Male")) +
    
    theme(axis.title = element_text(),
          legend.title = element_blank(),
          legend.position = "bottom") +
    
    guides(x = guide_axis(n.dodge = 1, check.overlap = TRUE, angle = 45)) +
    
    labs(x = "Age Groups", y = "log(nMx)", 
         title = "Death Rates by Age and Gender: 2010-2019",
         subtitle = "São Paulo, Brazil",
         caption = "\n Source: Own calculations from IBGE and DATASUS")

# ---
# SCRIPT: adjusting data for incompleteness
  
  adjFactors_ages <- seq(15, 60, by = 5)
  
  adjFactors_data_ALL <- results_ALL %>% 
    select(cod, ggbseg, delta)
    
  adjFactors_data_ALL$cod <- case_match(adjFactors_data_ALL$cod,
                                    "m" ~ "Male",
                                    "f" ~ "Female")
  
  # Data seems to be complete, so this part of the code is unecessary
  
  lifetable_data_ALL <- lifetable_data_ALL %>%
    inner_join(., adjFactors_data_ALL, join_by(cod)) %>%
    mutate(ggbseg = ifelse(ggbseg > 1, 1, ggbseg),
         delta = ifelse(ggbseg > 1, 1, delta),
         DEATHS = ifelse(LOWER %in% adjFactors_ages, DEATHS / ggbseg, DEATHS),
         POP_1 = ifelse(delta < 1 & LOWER %in% adjFactors_ages, POP_1 / delta, POP_1),
         POP_2 = ifelse(delta >= 1 & LOWER %in% adjFactors_ages, POP_2 / delta, POP_2))
           
# ---
# SCRIPT: writing the function to generate the life tables
  
  LT = function(data, gender = c("Female", "Male")){
    
    gender = match.arg(gender)
    
    temp = data %>% 
      filter(GENDER == gender)
    
    LT = tibble(temp$AGE_GROUP, temp$N, temp$GENDER, temp$POP_1, temp$POP_2, temp$DEATHS) %>% 
      rename(., 
             AGE_GROUP = "temp$AGE_GROUP",
             N = "temp$N",
             GENDER = "temp$GENDER",
             POP_1 = "temp$POP_1",
             POP_2 = "temp$POP_2",
             DEATHS = "temp$DEATHS") %>% 
      mutate(PYL = 10 * sqrt(POP_1 * POP_2),
             nMx = DEATHS / PYL,
             nax = ifelse(GENDER == "Female", 
                          ifelse(AGE_GROUP == "0-1", 
                                 ifelse(nMx >= 0.107, 0.350, 0.053 + 2.8 * nMx),
                                 N / 2), 
                          ifelse(AGE_GROUP == "0-1", 
                                 ifelse(nMx >= 0.107, 0.330, 0.045 + 2.684 * nMx),
                                 N / 2)),
             nax = ifelse(GENDER == "Female",
                          ifelse(AGE_GROUP == "1-4",
                                 ifelse(nMx >= 0.107, 1.361, 1.522 + 1.518 * nMx),
                                 nax),
                          ifelse(AGE_GROUP == "1-4", 
                                 ifelse(nMx >= 0.107, 1.352, 1.651 + 2.816 * nMx),
                                 nax)),
             nax = ifelse(AGE_GROUP == "85+", 
                          1 / nMx,
                          nax)) %>% 
      mutate(nqx = (nMx * N) / (1 + (N - nax) * nMx),
             nqx = ifelse(AGE_GROUP == "85+", 1, nqx),
             npx = 1 - nqx) %>% 
      group_by(GENDER) %>% 
      mutate(lx = 1 * cumprod(c(1, npx[-n()])),
             ndx = lx * nqx,
             Lx = (lx - ndx) * N + ndx * nax,
             Tx = rev(cumsum(rev(Lx))),
             ex = Tx / lx) %>% 
      ungroup(.)
    
    return(LT)
  }

# ---
# Storing the results
  
  LT_Male <- LT(lifetable_data_ALL, "Male")
  
  LT_Female <- LT(lifetable_data_ALL, "Female")
  
# ---
# SCRIPT: generating plot of the death probability used in the life table
  
  ggplot() +
    
    geom_line(data = LT_Male, 
              mapping = aes(x = AGE_GROUP, y = log(nqx), group = GENDER, color = GENDER),
              linewidth = 0.8) +
    
    geom_line(data = LT_Female, 
              mapping = aes(x = AGE_GROUP, y = log(nqx), group = GENDER, color = GENDER),
              linewidth = 0.8) +
    
    scale_colour_manual(values = c("#8D6A9F", "#00CC99"),
                        aesthetics = c("colour", "fill"),
                        labels = c("Female","Male")) +
    
    theme(axis.title = element_text(),
          legend.title = element_blank(),
          legend.position = "bottom") +
    
    guides(x = guide_axis(n.dodge = 1, check.overlap = TRUE, angle = 45)) +
    
    labs(x = "Age Groups", y = "log(nqx)", 
         title = "Probability od Death by Age and Gender: 2010-2019",
         subtitle = "São Paulo, Brazil",
         caption = "\n Source: Own calculations from IBGE and DATASUS")
  
  rm(adjFactors_data_ALL, adjFactors_ages, DDM_data_ALL, results_ALL,
     popEstimates_data_ALL_MORTALITY, deathEstimates_data_ALL)
  