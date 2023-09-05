## MATHEUS' MA THESIS ##

# ---
# Chapter 3 - MORTALITY DATA

# ---
# SCRIPT: loading the necessary R Packages

  library(pacman)
  p_load(readr, ipumsr, read.dbc, PNADcIBGE, tidyverse, survey, skimr, DDM, ggbreak)

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
  
  deathEstimates_data$RACE_COLOR <- case_match(deathEstimates_data$RACE_COLOR,
                                               "Branca" ~ "White",
                                               "Preta" ~ "Black",
                                               "Amarela" ~ "Asian",
                                               "Parda" ~ "Mixed",
                                               "Indígena" ~ "Indigenous",
                                               NA ~ NA)

# ---
# SCRIPT: Running some analysis to present in the Thesis
  
  total_byYEAR <- deathEstimates_data %>% 
    filter(., YEAR >= 2010) %>% 
    group_by(YEAR) %>% 
    summarise(TOTAL = n()) %>% 
    as.data.frame()
  
  total_byYEARandGENDER <- deathEstimates_data %>% 
    filter(., YEAR >= 2010) %>% 
    group_by(YEAR, GENDER) %>% 
    summarise(TOTAL = n()) %>% 
    as.data.frame()
    
  total_byYEARandRACE_ALL <- deathEstimates_data %>% 
    filter(., YEAR >= 2010) %>% 
    group_by(YEAR, GENDER, RACE_COLOR) %>% 
    summarise(TOTAL = n())%>% 
    as.data.frame()
  
  total_byYEARandRACE_ALL$RACE_COLOR <- case_match(total_byYEARandRACE_ALL$RACE_COLOR,
                                                   "White" ~ "White",
                                                   "Black" ~ "Black",
                                                   "Asian" ~ "Asian",
                                                   "Mixed" ~ "Mixed",
                                                   "Indigenous" ~ "Indigenous",
                                                   NA ~ "Missing")
  
  total_byYEARandRACE_ALL <- total_byYEARandRACE_ALL %>% 
    mutate(RACE_COLOR = as_factor(RACE_COLOR),
           RACE_COLOR = ordered(RACE_COLOR, c("White", "Mixed", "Black", "Asian", "Indigenous", "Missing")))
  
  total_byYEARandRACE_three <- deathEstimates_data %>% 
    filter(., YEAR >= 2010, RACE_COLOR %in% c("White", "Mixed", "Black")) %>% 
    group_by(YEAR, GENDER, RACE_COLOR) %>% 
    summarise(TOTAL = n())%>% 
    as.data.frame()
  
  total_byYEARandRACE_three <- total_byYEARandRACE_three %>% 
    mutate(RACE_COLOR = as_factor(RACE_COLOR),
           RACE_COLOR = ordered(RACE_COLOR, c("White", "Mixed", "Black", "Asian", "Indigenous", "Missing")))
  
  totalMissing_byCOLUMN <- deathEstimates_data %>% 
    filter(., YEAR >= 2010) %>% 
    summarise(across(everything(), ~ sum(is.na(.))))
  
  totalMissing_byYEAR <- deathEstimates_data %>% 
    filter(., YEAR >= 2010) %>% 
    group_by(YEAR) %>% 
    summarise(across(everything(), ~ sum(is.na(.))))
  
  distAGEandYEAR_processdata <- deathEstimates_data %>%
    filter(., YEAR >= 2010) %>% 
    ungroup(.) %>% 
    group_by(., YEAR) %>%
    select(., "YEAR", "AGE") %>% 
    skim(data = .)
  
  distAGEandGENDER_processdata <- deathEstimates_data %>%
    filter(., YEAR >= 2010) %>% 
    ungroup(.) %>% 
    group_by(., GENDER) %>%
    select(., "AGE", "GENDER") %>% 
    skim(data = .)
  
  distAGEandRACE_processdata <- deathEstimates_data %>%
    filter(., YEAR >= 2010) %>% 
    ungroup(.) %>% 
    group_by(., RACE_COLOR) %>%
    select(., "AGE", "RACE_COLOR") %>% 
    skim(data = .)
  
  distGENDERandYEAR_processdata <- deathEstimates_data %>%
    filter(., YEAR >= 2010) %>% 
    ungroup(.) %>% 
    group_by(., YEAR) %>%
    select(., "YEAR", "GENDER") %>% 
    skim(data = .)
  
  distGENDERandRACE_processdata <- deathEstimates_data %>%
    filter(., YEAR >= 2010) %>% 
    ungroup(.) %>% 
    group_by(., RACE_COLOR) %>%
    select(., "GENDER", "RACE_COLOR") %>% 
    skim(data = .)
  
  distRACEandYEAR_processdata <- deathEstimates_data %>%
    filter(., YEAR >= 2010) %>% 
    ungroup(.) %>% 
    group_by(., YEAR) %>%
    select(., "YEAR", "RACE_COLOR") %>% 
    skim(data = .)
  
  distRACEandGENDER_processdata <- deathEstimates_data %>%
    filter(., YEAR >= 2010) %>% 
    ungroup(.) %>% 
    group_by(., GENDER) %>%
    select(., "GENDER", "RACE_COLOR") %>% 
    skim(data = .)

# ---
# SCRIPT:  
  
  ggplot(data = filter(total_byYEAR),
         aes(x = YEAR, y = TOTAL)) +
    
    geom_line(linewidth = 0.8, color = "#806991") +
    
    geom_point(size = 1.5, color = "#806991") +
    
    scale_x_continuous(breaks = seq(2010, 2019, by = 1)) +
    
    scale_y_continuous(labels = function(x) abs(x * 0.000001)) +
    
    theme(axis.title = element_text(),
          legend.title = element_blank(),
          legend.position = "none") +
    
    guides(x = guide_axis(n.dodge = 1, check.overlap = TRUE)) +
    
    labs(x = "Year", y = "Deaths (in millions)", 
         # title = "Total Number of Deaths: 2010 - 2019",
         # subtitle = "São Paulo, Brazil",
         # caption = "\n Source: Own calculations from CENSUS/PNAD")
    )
  
  ggplot(data = filter(total_byYEARandGENDER, GENDER %in% c("Male", "Female")),
         aes(x = YEAR, y = TOTAL, color = GENDER)) +
    
    geom_line(linewidth = 0.8) +
    
    geom_point(size = 1.5) +
    
    scale_colour_manual(values = c("#8D6A9F", "#00CC99"),
                        aesthetics = c("colour", "fill"),
                        labels = c("Female","Male")) +
    
    scale_x_continuous(breaks = seq(2010, 2019, by = 1)) +
    
    scale_y_continuous(labels = function(x) abs(x * 0.000001)) +
    
    theme(axis.title = element_text(),
          legend.title = element_blank(),
          legend.position = "bottom") +
    
    guides(x = guide_axis(n.dodge = 1, check.overlap = TRUE)) +
    
    labs(x = "Year", y = "Deaths (in millions)", 
         # title = "Total Number of Deaths: 2010 - 2019",
         # subtitle = "São Paulo, Brazil",
         # caption = "\n Source: Own calculations from CENSUS/PNAD")
    )
  
  ggplot(data = filter(total_byYEARandRACE_ALL, GENDER %in% c("Female", "Male")),
         aes(x = YEAR, y = TOTAL, fill = RACE_COLOR, color = RACE_COLOR)) +
    
    scale_fill_manual(values = c("White" = "#ef476f",
                                 "Black" = "#ffd166",
                                 "Mixed" = "#118ab2",
                                 "Asian" = "#06d6a0",
                                 "Indigenous" = "#7F96FF",
                                 "Missing" = "#575761")) +
    
    scale_color_manual(values = c("White" = "#ef476f",
                                  "Black" = "#ffd166",
                                  "Mixed" = "#118ab2",
                                  "Asian" = "#06d6a0",
                                  "Indigenous" = "#7F96FF",
                                  "Missing" = "#575761")) +
    
    geom_line(linewidth = 0.8) +
    
    geom_point(size = 1.5) +
    
    scale_x_continuous(breaks = seq(2010, 2019, by = 1)) +
    
    scale_y_continuous(labels = function(x) abs(x * 0.001)) +
    
    # scale_y_break(c(40000, 80000)) +
    
    facet_wrap(~ GENDER) +
    
    theme(axis.title = element_text(),
          legend.title = element_blank(),
          legend.position = "bottom") +
    
    guides(x = guide_axis(n.dodge = 1, check.overlap = TRUE, angle = 45)) +
    
    labs(x = "Year", y = "Deaths (in thousands)", 
         # title = "Total Number of Deaths: 2010 - 2019",
         # subtitle = "São Paulo, Brazil",
         # caption = "\n Source: Own calculations from CENSUS/PNAD")
    )
  
  ggplot(data = filter(total_byYEARandRACE_three, GENDER %in% c("Female", "Male")),
         aes(x = YEAR, y = TOTAL, fill = RACE_COLOR, color = RACE_COLOR)) +
    
    scale_fill_manual(values = c("White" = "#ef476f",
                                 "Black" = "#ffd166",
                                 "Mixed" = "#118ab2")) +
    
    scale_color_manual(values = c("White" = "#ef476f",
                                  "Black" = "#ffd166",
                                  "Mixed" = "#118ab2")) +
    
    geom_line(linewidth = 0.8) +
    
    geom_point(size = 1.5) +
    
    scale_x_continuous(breaks = seq(2010, 2019, by = 1)) +
    
    scale_y_continuous(labels = function(x) abs(x * 0.001)) +
    
    scale_y_break(c(40000, 80000)) +
    
    facet_wrap(~ GENDER) +
    
    theme(axis.title = element_text(),
          legend.title = element_blank(),
          legend.position = "bottom") +
    
    guides(x = guide_axis(n.dodge = 1, check.overlap = TRUE, angle = 45)) +
    
    labs(x = "Year", y = "Deaths (in thousands)", 
         # title = "Total Number of Deaths: 2010 - 2019",
         # subtitle = "São Paulo, Brazil",
         # caption = "\n Source: Own calculations from CENSUS/PNAD")
    )
  
# ---
# SCRIPT: creating age groups [0,4]; [5,9]; [10,14] ... for building the life table with three RACE_COLOR groups (White, Mixed, Black)
  
  deathEstimates_data_THREE <- deathEstimates_data %>%
    arrange(., YEAR, AGE) %>% 
    mutate(INTERVAL = findInterval(AGE, seq(0, 85, by = 5)) - 1)
  
  find_replace <- tibble(INTERVAL = unique(na.omit(deathEstimates_data_THREE$INTERVAL)),
                         LOWER = seq(0, 85, by = 5),
                         UPPER = seq(4, 89, by = 5)) %>%
    unite(AGE_GROUP, LOWER:UPPER, sep = "-", remove = TRUE) %>%
    mutate(AGE_GROUP = ifelse(AGE_GROUP == "85-89", "85+", AGE_GROUP),
           AGE_GROUP = as_factor(AGE_GROUP))
  
  deathEstimates_data_THREE <- inner_join(deathEstimates_data_THREE, find_replace, by = "INTERVAL"); rm(find_replace)
  
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
                                                     "White" ~ "White",
                                                     "Black" ~ "Black",
                                                     "Asian" ~ "Asian",
                                                     "Mixed" ~ "Mixed",
                                                     "Indigenous" ~ "Indigenous",
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
# SCRIPT: minor adjustments to the data required for the two RACE_COLOR life tables

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
  # SCRIPT: minor adjustments to the data required for the life tables with all RACE_COLOR groups together
  
  deathEstimates_data_ALL <- deathEstimates_data %>% 
    arrange(., YEAR, AGE) %>%
    mutate(INTERVAL = findInterval(AGE, seq(0, 85, by = 5)) - 1)
  
  find_replace <- tibble(INTERVAL = unique(na.omit(deathEstimates_data_ALL$INTERVAL)),
                         LOWER = seq(0, 85, by = 5),
                         UPPER = seq(4, 89, by = 5)) %>%
    unite(AGE_GROUP, LOWER:UPPER, sep = "-", remove = TRUE) %>%
    mutate(AGE_GROUP = ifelse(AGE_GROUP == "85-89", "85+", AGE_GROUP),
           AGE_GROUP = as_factor(AGE_GROUP))
  
  deathEstimates_data_ALL <- inner_join(deathEstimates_data_ALL, find_replace, by = "INTERVAL")
  
  deathEstimates_data_ALL <- deathEstimates_data_ALL %>% 
    group_by(YEAR, AGE_GROUP, GENDER) %>% 
    filter(YEAR >= 2010) %>% 
    summarise(., TOTAL = n()) %>% 
    na.exclude(.)
  
  rm(deathEstimates_data)

# ---
# SCRIPT: reating plot to highlight how little of the race data is missing in 2010
  
  ggplot(data = distRACECOLORandAGEGROUP_processdata,
         aes(x = AGE_GROUP, fill = GENDER, color = GENDER,
             y = ifelse(test = GENDER == "Male", yes = -(1 - complete_rate), no = (1 - complete_rate)))) +
    
    geom_bar(stat = "identity", width = 0.7) +
    
    scale_y_continuous(labels = function(x) abs(x * 100), 
                       limits = 0.15 * c(-1, 1)) +
    
    coord_flip() +
    
    facet_wrap(~ YEAR, nrow = 2) +
    
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

# ---
# SCRIPT: creating stacked plot to show how the three largest categories are distributed in 2010 and 2019
  
  ggplot(data = filter(deathEstimates_data_THREE, RACE_COLOR %in% c("White", "Mixed", "Black") &
                         YEAR %in% c(2010, 2019)) %>% 
           group_by(., YEAR, GENDER, RACE_COLOR) %>% 
           summarise(TOTAL = sum(TOTAL)) %>%
           group_by(., YEAR, GENDER) %>% 
           mutate(SHARE = TOTAL / sum(TOTAL)),
         aes(x = YEAR, y = SHARE, fill = RACE_COLOR, color = RACE_COLOR)) +
    
    scale_fill_manual(values = c("White" = "#ef476f",
                                 "Black" = "#ffd166",
                                 "Mixed" = "#118ab2",
                                 "Asian" = "#06d6a0",
                                 "Indigenous" = "#7F96FF",
                                 "Missing" = "#575761")) +
    
    scale_color_manual(values = c("White" = "#ef476f",
                                  "Black" = "#ffd166",
                                  "Mixed" = "#118ab2",
                                  "Asian" = "#06d6a0",
                                  "Indigenous" = "#7F96FF",
                                  "Missing" = "#575761")) +
    
    geom_bar(position="fill", stat="identity") + 
    
    geom_text(aes(label = paste0(round(SHARE * 100, digits = 1), "%")),
              color = "white",
              vjust = 1.5, size = 3.3, fontface = "bold") +
    
    scale_x_continuous(breaks = seq(2010, 2019, by = 9)) +
    
    scale_y_continuous(labels = function(x) abs(x * 100)) +
    
    facet_wrap(~ GENDER) +
    
    theme(axis.title = element_text(),
          legend.title = element_blank(),
          legend.position = "bottom") +
    
    guides(y = guide_axis(n.dodge = 1, check.overlap = TRUE)) +
    
    labs(x = "Year", y = "Share of Registered Deaths by Race/Color (in %)", 
         title = "Change of the Total Registered Deaths by Race/Color: 2010 - 2019",
         subtitle = "São Paulo, Brazil",
         caption = "\n Source: Own calculations from DATASUS")
  