## MATHEUS' MA THESIS ##

# ---
# Chapter 3 - MORTALITY DATA

# ---
# SCRIPT: loading the necessary R Packages

  library(pacman)
  p_load(readr, ipumsr, read.dbc, PNADcIBGE, tidyverse, survey, skimr, DDM)

# ---
# SCRIPT: downloading (or locally loading) micro data

# DATA SOURCE: DATASUS

  setwd("/Users/mlobo/Documents/GitHub/MA-Thesis/SIM-DOSP")
  
  # file_list <- list.files(path = "/Users/mlobo/Documents/GitHub/MA-Thesis/SIM-DOSP")
  # 
  # DATASUS_data <- tibble()
  # 
  # for (i in 1:length(file_list)){
  #   
  #   temp_data <- read.dbc(file = file_list[i]) %>% 
  #     process_sim(., municipality_data = TRUE) %>% 
  #     mutate("AGE" = trunc((DTNASC %--% DTOBITO) / years(1)),
  #            "INDDoB" = ifelse(is.na(DTNASC) == TRUE, NA, 1)) %>% 
  #     separate(DTOBITO, c("ANOOBITO", "MESOBITO", "DIAOBITO")) %>% 
  #     select(c(ANOOBITO, AGE, INDDoB, SEXO, RACACOR))
  #   
  #   DATASUS_data <- rbind(DATASUS_data, temp_data)
  #   
  # }
  
  # rm(temp_data)
  
  # write_excel_csv(DATASUS_data, "deathEstimatesDATASUS.csv", na = "NA", delim = ",")
  
# ---
# SCRIPT: reading some previously downloaded data from a local folder and making minor adjustments

  deathEstimates_data <- read_csv("deathEstimatesDATASUS.csv")
  
  deathEstimates_data <- deathEstimates_data %>% 
    rename("YEAR" = "ANOOBITO",
           "GENDER" = "SEXO",
           "RACE_COLOR" = "RACACOR")
  
  deathEstimates_data$GENDER <- case_match(deathEstimates_data$GENDER,
                                 "Masculino" ~ "Male",
                                 "Feminino" ~ "Female")

# ---
  # SCRIPT: creating age groups [0,4]; [5,9]; [10,14] ... for building the life table with three RACE_COLOR groups (White, Mixed, Black)
  
  deathEstimates_data_THREE<- deathEstimates_data %>%
    arrange(., YEAR, AGE) %>% 
    mutate(INTERVAL = findInterval(AGE, seq(0, 85, by = 5)) - 1)
  
  find_replace <- tibble(INTERVAL = unique(na.omit(deathEstimates_data_THREE$INTERVAL)),
                         LOWER = seq(0, 85, by = 5),
                         UPPER = seq(4, 89, by = 5)) %>%
    unite(AGE_GROUP, LOWER:UPPER, sep = "-", remove = TRUE) %>%
    mutate(AGE_GROUP = ifelse(AGE_GROUP == "85-89", "85+", AGE_GROUP),
           AGE_GROUP = as_factor(AGE_GROUP))
  
  deathEstimates_data_THREE <- inner_join(deathEstimates_data_THREE, find_replace, by = "INTERVAL"); rm(find_replace)

  rm(deathEstimates_data)
  
# ---
# SCRIPT: creating an object to assess the degree to which race information is missing

  distRACECOLORandAGEGROUP_processdata <- deathEstimates_data_THREE %>% 
    group_by(., YEAR, AGE_GROUP, GENDER) %>%
    select(., "YEAR", "AGE_GROUP", "GENDER", "RACE_COLOR") %>% 
    filter(., YEAR >= 2010) %>% 
    skim(data = .)

# ---
# SCRIPT: minor adjustments to the data required for the three RACE_COLOR life tables
  
  deathEstimates_data_THREE$RACE_COLOR <- case_match(deathEstimates_data_THREE$RACE_COLOR,
                                               "Branca" ~ "White",
                                               "Preta" ~ "Black",
                                               "Amarela" ~ "Asian",
                                               "Parda" ~ "Mixed",
                                               "Indígena" ~ "Indigenous",
                                               NA ~ "Missing")
  
  deathEstimates_data_THREE <- deathEstimates_data_THREE %>% 
    mutate(RACE_COLOR = as_factor(RACE_COLOR),
           RACE_COLOR = ordered(RACE_COLOR, c("White", "Mixed", "Black", "Asian", "Indigenous", "Missing"))) 
  
  deathEstimates_data_THREE <- deathEstimates_data_THREE %>% 
    mutate(AGE_GROUP = as_factor(AGE_GROUP)) %>% 
    filter(RACE_COLOR %in% c("White", "Mixed", "Black"),
           YEAR >= 2010) %>% 
    group_by(YEAR, AGE_GROUP, GENDER, RACE_COLOR) %>% 
    summarise(., TOTAL = n()) %>% 
    na.exclude(.)

# ---
# SCRIPT: creating age groups [0,1); [1,4]; [5,9] ... for building the life table with two RACE_COLOR groups (White, Mixed_Black)

  deathEstimates_data_TWO <- deathEstimates_data_THREE
  
  deathEstimates_data_TWO$RACE_COLOR <- case_match(deathEstimates_data_TWO$RACE_COLOR,
                                                     "White" ~ "White",
                                                     "Black" ~ "Mixed_Black",
                                                     "Asian" ~ "Asian",
                                                     "Mixed" ~ "Mixed_Black",
                                                     "Indigenous" ~ "Indigenous",
                                                     "Missing" ~ "Missing")
  
  deathEstimates_data_TWO <- deathEstimates_data_TWO %>% 
    mutate(RACE_COLOR = as_factor(RACE_COLOR),
           RACE_COLOR = ordered(RACE_COLOR, c("White", "Mixed_Black", "Asian", "Indigenous", "Missing"))) 
  
  deathEstimates_data_TWO <- deathEstimates_data_TWO %>% 
    group_by(YEAR, AGE_GROUP, GENDER, RACE_COLOR) %>% 
    filter(RACE_COLOR %in% c("White", "Mixed_Black"),
           YEAR >= 2010) %>% 
    summarise(., TOTAL = sum(TOTAL))

  # ---
  # SCRIPT: creating age groups [0,1); [1,4]; [5,9] ... for building the life table with all RACE_COLOR groups combined
  
  deathEstimates_data_ALL <- deathEstimates_data_TWO %>% 
    group_by(YEAR, AGE_GROUP, GENDER) %>% 
    filter(YEAR >= 2010) %>% 
    summarise(., TOTAL = sum(TOTAL))

# ---
# SCRIPT: reating plot to highlight how little of the race data is missing in 2010
  
  ggplot(data = distRACECOLORandAGEGROUP_processdata,
         aes(x = AGE_GROUP, fill = GENDER, color = GENDER,
             y = ifelse(test = GENDER == "Male", yes = -(1 - complete_rate), no = (1 - complete_rate)))) +
    
    geom_bar(stat = "identity", width = 0.7) +
    
    scale_y_continuous(labels = function(x) abs(x * 100), 
                       limits = 0.15 * c(-1, 1)) +
    
    coord_flip() +
    
    facet_wrap(~ YEAR) +
    
    scale_colour_manual(values = c("#8D6A9F", "#00CC99"),
                        aesthetics = c("colour", "fill"),
                        labels = c("Female","Male")) +
    
    theme(axis.title = element_text(),
          legend.title = element_blank(),
          legend.position = "bottom") +
    
    guides(y = guide_axis(n.dodge = 1, check.overlap = TRUE)) +
    
    labs(x = "Age Groups", y = "Percentage of Death Records with missing Race Information", 
         title = "Evolution of missing values regarding Race on Death Records by Age Groups",
         subtitle = "São Paulo, Brazil",
         caption = "\n Source: Own calculations from DATASUS")
  