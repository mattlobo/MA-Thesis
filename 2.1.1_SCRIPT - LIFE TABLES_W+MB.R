## MATHEUS' MA THESIS ##

# ---
# Chapter 3 - THE LIFE TABLE

# ---
# SCRIPT: loading the necessary R Packages and scripts that deal with preliminary data

  library(pacman)
  p_load(readr, ipumsr, read.dbc, PNADcIBGE, tidyverse, survey, skimr, DDM)

# ---
# SCRIPT: removing unnecessary data

  rm(popEstimates_data_THREE_MORTALITY, popEstimates_data_ALL_MORTALITY, 
     popEstimates_data_THREE_PLOT,
     deathEstimates_data_THREE, deathEstimates_data_ALL,
     distRACECOLORandAGEGROUP_processdata)

# ---
# SCRIPT: preparing the input to run the life table function

  lifetable_data_TWO <- inner_join(filter(popEstimates_data_TWO_MORTALITY, YEAR == 2010),
                                   filter(popEstimates_data_TWO_MORTALITY, YEAR == 2019),
                                   join_by(AGE_GROUP, GENDER, RACE_COLOR)) %>% 
    rename("YEAR_1" = "YEAR.x",
           "POP_1" = "TOTAL.x",
           "YEAR_2" = "YEAR.y",
           "POP_2" = "TOTAL.y") %>% 
    separate(., AGE_GROUP, c("LOWER", "UPPER"), remove = FALSE, convert = TRUE) %>% 
    mutate(N = ifelse(is.na(UPPER) == TRUE, 4, UPPER - LOWER),
           N = ifelse(LOWER > 0, N + 1, N),
           cod = paste(ifelse(GENDER == "Female", "f", "m"), "-", RACE_COLOR))
  
  temp_data_MORTALITY_TWO <- deathEstimates_data_TWO %>% 
    group_by(AGE_GROUP, GENDER, RACE_COLOR) %>% 
    summarise(DEATHS = sum(TOTAL))
  
  lifetable_data_TWO <- inner_join(lifetable_data_TWO,
                                   temp_data_MORTALITY_TWO,
                                   join_by(AGE_GROUP, GENDER, RACE_COLOR)) %>% 
    select(AGE_GROUP, LOWER, N, GENDER, RACE_COLOR, POP_1, POP_2, DEATHS, cod); rm(temp_data_MORTALITY_TWO)

# ---
# SCRIPT: generating plot of RAW mortality rates before any adjustments

  ggplot(data = lifetable_data_TWO, 
         mapping = aes(x = AGE_GROUP, y = log(DEATHS / (10 * sqrt(POP_1 * POP_2))), group = RACE_COLOR, color = RACE_COLOR)) +
    
    geom_line(linewidth = 0.8) +
    
    scale_color_manual(values = c("White" = "#ef476f",
                                  "Mixed_Black" = "#118ab2",
                                  "Asian" = "#06d6a0",
                                  "Indigenous" = "#7F96FF",
                                  "Missing" = "#575761")) +
    facet_wrap(~ GENDER)  +
    
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
  
  adjFactors_data_TWO <- results_TWO %>% 
    select(cod, ggbseg, delta)
  
  lifetable_data_TWO <- lifetable_data_TWO %>%
    inner_join(., adjFactors_data_TWO, join_by(cod)) %>%
    mutate(ggbseg = ifelse(ggbseg > 1, 1, ggbseg),
           delta = ifelse(ggbseg < 1, delta, 1),
           DEATHS = ifelse(LOWER %in% adjFactors_ages, DEATHS / ggbseg, DEATHS),
           POP_1 = ifelse(delta < 1 & LOWER %in% adjFactors_ages, POP_1 / delta, POP_1),
           POP_2 = ifelse(delta >= 1 & LOWER %in% adjFactors_ages, POP_2 / delta, POP_2))

# ---
# SCRIPT: writing the function to generate the life tables

  LT = function(data, gender = c("Female", "Male"), race_color = c("White", "Mixed_Black", "Mixed", "Black")){
    
    gender = match.arg(gender)
    race_color = match.arg(race_color)
    
    temp = data %>% 
      filter(GENDER == gender,
             RACE_COLOR == race_color)
    
    LT = tibble(temp$AGE_GROUP, temp$N, temp$GENDER, temp$RACE_COLOR, temp$POP_1, temp$POP_2, temp$DEATHS) %>% 
      rename(., 
             AGE_GROUP = "temp$AGE_GROUP",
             N = "temp$N",
             GENDER = "temp$GENDER",
             RACE_COLOR = "temp$RACE_COLOR",
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
             nax = ifelse(AGE_GROUP == "90+", 
                          1 / nMx,
                          nax)) %>% 
      mutate(nqx = (nMx * N) / (1 + (N - nax) * nMx),
             nqx = ifelse(AGE_GROUP == "90+", 1, nqx),
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
# SCRIPT: Storing the results for RACE_COLOR White

  LT_Male_White <- LT(lifetable_data_TWO, "Male", "White")
  
  LT_Female_White <- LT(lifetable_data_TWO, "Female", "White")

# ---
# Storing the results for RACE_COLOR Mixed_Black

  LT_Male_Mixed_Black <- LT(lifetable_data_TWO, "Male", "Mixed_Black")
  
  LT_Female_Mixed_Black <- LT(lifetable_data_TWO, "Female", "Mixed_Black")
