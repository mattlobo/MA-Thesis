## MATHEUS' MA THESIS ##

# ---
# Chapter 3 - POPULATION DATA

# ---
# SCRIPT: loading the necessary R Packages

  library(pacman)
  p_load(readr, ipumsr, read.dbc, PNADcIBGE, tidyverse, survey, skimr, DDM, Cairo)

# ---
# SCRIPT: downloading (or locally loading) micro data

# DATA SOURCE: 2010 Brazilian CENSUS - obtained from IPUMS
  
  setwd("/Users/mlobo/Documents/GitHub/MA-Thesis/IBGE/IPUMS")

  # DDI <- read_ipums_ddi("ipumsi_00003.xml")
  # popEstimatesIPUMS <- read_ipums_micro(DDI); rm(DDI)
  # 
  # popEstimatesIPUMS <- popEstimatesIPUMS %>%
  #   filter(GEO1_BR2010 == 35) %>%
  #   count(YEAR, BR2010A_AGE, BR2010A_SEX, BR2010A_RACE, wt = PERWT)
  # 
  # write_excel_csv(popEstimatesIPUMS, "popEstimatesIPUMS.csv", na = "NA", delim = ",")
 
# ---
# SCRIPT: reading some previously downloaded data from a local folder and making minor adjustments
  
  popEstimatesIPUMS <- read_csv("popEstimatesIPUMS.csv")
  
  popEstimatesIPUMS <- popEstimatesIPUMS %>%
    rename("YEAR" = "YEAR",
           "AGE" = "BR2010A_AGE",
           "GENDER" = "BR2010A_SEX",
           "RACE_COLOR" = "BR2010A_RACE",
           "TOTAL" = "n")
  
  popEstimatesIPUMS$GENDER <- case_match(popEstimatesIPUMS$GENDER,
                                 1 ~ "Male",
                                 2 ~ "Female")
  
  popEstimatesIPUMS$RACE_COLOR <- case_match(popEstimatesIPUMS$RACE_COLOR,
                                     1 ~ "White",
                                     2 ~ "Black",
                                     3 ~ "Asian",
                                     4 ~ "Mixed",
                                     5 ~ "Indigenous",
                                     9 ~ "Missing")

  skim(popEstimatesIPUMS)
  
# ---
# SCRIPT: downloading (or locally loading) micro data

# DATA SOURCE: 2019 PNADc

  setwd("/Users/mlobo/Documents/GitHub/MA-Thesis/IBGE/PNADc")

  # popEstimatesPNADc <- tibble()
  # 
  # years_to_dowload <- 2019
  # 
  # for (i in 1:length(years_to_dowload)){
  # 
  #   temp_data <- get_pnadc(year = years_to_dowload[i], quarter = 3, design = FALSE) %>%
  #     filter(UF == "São Paulo") %>%
  #     count(Ano, V2009, V2007, V2010, wt = V1028)
  # 
  #   popEstimatesPNADc <- rbind(popEstimatesPNADc, temp_data); gc()
  # 
  # }
  # 
  # write_excel_csv(popEstimatesPNADc, "popEstimatesPNADc.csv", na = "NA", delim = ","); rm(years_to_dowload, temp_data)

# ---
# SCRIPT: reading some previously downloaded data from a local folder and making minor adjustments
  
  popEstimatesPNADc <- read_csv("~/Documents/GitHub/MA-Thesis/IBGE/PNADc/popEstimatesPNADc.csv")
  
  popEstimatesPNADc <- popEstimatesPNADc %>%
    rename("YEAR" = "Ano",
           "AGE" = "V2009",
           "GENDER" = "V2007",
           "RACE_COLOR" = "V2010",
           "TOTAL" = "n")
  
  popEstimatesPNADc$GENDER <- case_match(popEstimatesPNADc$GENDER,
                                 "Homem" ~ "Male",
                                 "Mulher" ~ "Female")
  
  popEstimatesPNADc$RACE_COLOR <- case_match(popEstimatesPNADc$RACE_COLOR,
                                     "Branca" ~ "White",
                                     "Preta" ~ "Black",
                                     "Amarela" ~ "Asian",
                                     "Parda" ~ "Mixed",
                                     "Indígena" ~ "Indigenous",
                                     "Ignorado" ~ "Missing")
  
  skim(popEstimatesPNADc)
  
# ---
# SCRIPT: merging the necessary data before proceeding

  popEstimates <- tibble(rbind(popEstimatesIPUMS, popEstimatesPNADc)); rm(popEstimatesIPUMS, popEstimatesPNADc)

# ---
# SCRIPT: creating age groups [0,1); [1,4]; [5,9] ... for building the life table with three RACE_COLOR groups (White, Mixed, Black)

  popEstimates_data_THREE_MORTALITY <- popEstimates %>%
    arrange(., YEAR, AGE) %>%
    mutate(INTERVAL = findInterval(AGE, c(0, 1, seq(5, 85, by = 5))) - 1)
  
  find_replace_MORTALITY <- tibble(INTERVAL = unique(popEstimates_data_THREE_MORTALITY$INTERVAL),
                         LOWER = c(0, 1, seq(5, 85, by = 5)),
                         UPPER = c(1, 4, seq(9, 90, by = 5))) %>%
    unite(AGE_GROUP, LOWER:UPPER, sep = "-", remove = TRUE) %>%
    mutate(AGE_GROUP = ifelse(AGE_GROUP == "85-89", "85+", AGE_GROUP))
  
  popEstimates_data_THREE_MORTALITY <- inner_join(popEstimates_data_THREE_MORTALITY, find_replace_MORTALITY, by = "INTERVAL") %>% 
    mutate(AGE_GROUP = as_factor(AGE_GROUP)) %>% 
    filter(RACE_COLOR %in% c("White", "Mixed", "Black")) %>% 
    group_by(YEAR, AGE_GROUP, GENDER, RACE_COLOR) %>% 
    summarise(., TOTAL = sum(TOTAL))
  
  popEstimates_data_THREE_MORTALITY <- popEstimates_data_THREE_MORTALITY %>% 
    mutate(RACE_COLOR = addNA(as_factor(RACE_COLOR)),
           RACE_COLOR = ordered(RACE_COLOR, c("White", "Mixed", "Black", "Asian", "Indigenous", "Missing"))) 

# ---
# SCRIPT: creating age groups [0,1); [1,4]; [5,9] ... for building the life table with two RACE_COLOR groups (White, Mixed_Black)

  popEstimates_data_TWO_MORTALITY <- popEstimates_data_THREE_MORTALITY
  
  popEstimates_data_TWO_MORTALITY$RACE_COLOR <- case_match(popEstimates_data_TWO_MORTALITY$RACE_COLOR,
                                             "White" ~ "White",
                                             "Black" ~ "Mixed_Black",
                                             "Asian" ~ "Asian",
                                             "Mixed" ~ "Mixed_Black",
                                             "Indigenous" ~ "Indigenous",
                                             "Missing" ~ "Missing")
  
  popEstimates_data_TWO_MORTALITY <- popEstimates_data_TWO_MORTALITY %>% 
    mutate(RACE_COLOR = addNA(as_factor(RACE_COLOR)),
           RACE_COLOR = ordered(RACE_COLOR, c("White", "Mixed_Black", "Asian", "Indigenous", "Missing"))) %>% 
    filter(RACE_COLOR %in% c("White", "Mixed_Black")) %>% 
    group_by(YEAR, AGE_GROUP, GENDER, RACE_COLOR) %>% 
    summarise(., TOTAL = sum(TOTAL)); rm(find_replace_MORTALITY)

# ---
# SCRIPT: creating age groups [0,1); [1,4]; [5,9] ... for building the life table with all RACE_COLOR groups combined

  popEstimates_data_ALL_MORTALITY <- popEstimates %>%
    arrange(., YEAR, AGE) %>%
    mutate(INTERVAL = findInterval(AGE, c(0, 1, seq(5, 85, by = 5))) - 1)

  find_replace_MORTALITY <- tibble(INTERVAL = unique(popEstimates_data_ALL_MORTALITY$INTERVAL),
                                   LOWER = c(0, 1, seq(5, 85, by = 5)),
                                   UPPER = c(1, 4, seq(9, 90, by = 5))) %>%
    unite(AGE_GROUP, LOWER:UPPER, sep = "-", remove = TRUE) %>%
    mutate(AGE_GROUP = ifelse(AGE_GROUP == "85-89", "85+", AGE_GROUP))

  popEstimates_data_ALL_MORTALITY <- inner_join(popEstimates_data_ALL_MORTALITY, find_replace_MORTALITY, by = "INTERVAL") %>%
    mutate(AGE_GROUP = as_factor(AGE_GROUP)) %>%
    group_by(YEAR, AGE_GROUP, GENDER) %>%
    summarise(., TOTAL = sum(TOTAL))
  
# ---
# SCRIPT: creating age groups [0,4]; [5,9]; [10,14] ... for making plots with three RACE_COLOR groups (White, Mixed, Black)

  popEstimates_data_THREE_PLOT <- popEstimates %>%
    arrange(., YEAR, AGE) %>%
    mutate(INTERVAL = findInterval(AGE, seq(0, 85, by = 5)) - 1) %>% 
    filter(YEAR %in% seq(2010, 2020, 1))
  
  find_replace_PLOT <- tibble(INTERVAL = unique(popEstimates_data_THREE_PLOT$INTERVAL),
                                   LOWER = seq(0, 85, by = 5),
                                   UPPER = seq(4, 89, by = 5)) %>%
    unite(AGE_GROUP, LOWER:UPPER, sep = "-", remove = TRUE) %>%
    mutate(AGE_GROUP = ifelse(AGE_GROUP == "85-89", "85+", AGE_GROUP))
  
  popEstimates_data_THREE_PLOT <- inner_join(popEstimates_data_THREE_PLOT, find_replace_PLOT, by = "INTERVAL") %>% 
    mutate(AGE_GROUP = as_factor(AGE_GROUP)) %>% 
    group_by(YEAR, AGE_GROUP, GENDER, RACE_COLOR) %>% 
    summarise(., TOTAL = sum(TOTAL))
  
  popEstimates_data_THREE_PLOT <- popEstimates_data_THREE_PLOT %>% 
    mutate(RACE_COLOR = addNA(as_factor(RACE_COLOR)),
           RACE_COLOR = ordered(RACE_COLOR, c("White", "Mixed", "Black", "Asian", "Indigenous", "Missing"))) 

  rm(popEstimates)
  
  summary_popEstimates_data_THREE_PLOT <- popEstimates_data_THREE_PLOT %>% 
    ungroup(.) %>% 
    group_by(., YEAR, GENDER, RACE_COLOR) %>% 
    summarise(TOTAL = sum(TOTAL)) %>% 
    mutate(SHARE = round((TOTAL / sum(TOTAL) * 100), 2))
  
# ---
# SCRIPT: creating age groups [0,4]; [5,9]; [10,14] ... for making plots with two RACE_COLOR groups (White, Mixed_Black)
  
  popEstimates_data_TWO_PLOT <- popEstimates_data_THREE_PLOT
  
  popEstimates_data_TWO_PLOT$RACE_COLOR <- case_match(popEstimates_data_TWO_PLOT$RACE_COLOR,
                                                           "White" ~ "White",
                                                           "Black" ~ "Mixed_Black",
                                                           "Asian" ~ "Asian",
                                                           "Mixed" ~ "Mixed_Black",
                                                           "Indigenous" ~ "Indigenous",
                                                           "Missing" ~ "Missing")
  
  popEstimates_data_TWO_PLOT <- popEstimates_data_TWO_PLOT %>% 
    mutate(RACE_COLOR = addNA(as_factor(RACE_COLOR)),
           RACE_COLOR = ordered(RACE_COLOR, c("White", "Mixed_Black", "Asian", "Indigenous", "Missing"))) %>% 
    filter(RACE_COLOR %in% c("White", "Mixed_Black")) %>% 
    group_by(YEAR, AGE_GROUP, GENDER, RACE_COLOR) %>% 
    summarise(., TOTAL = sum(TOTAL)); rm(find_replace_PLOT)

# ---
# SCRIPT: creating plot to show age pyramids by gender in 2010 and 2019
  
  ggplot(data = filter(popEstimates_data_THREE_PLOT),
         aes(x = AGE_GROUP, fill = GENDER, color = GENDER,
             y = ifelse(test = GENDER == "Male", yes = -TOTAL, no = TOTAL))) +
    
    geom_bar(stat = "identity", width = 0.7) +
    
    scale_y_continuous(labels = function(x) abs(x * 0.000001)) +
    
    coord_flip() +
    
    facet_wrap(~ YEAR) +
    
    scale_colour_manual(values = c("#8D6A9F", "#00CC99"),
                        aesthetics = c("colour", "fill"),
                        labels = c("Female","Male")) +
    
    theme(axis.title = element_text(),
          legend.title = element_blank(),
          legend.position = "bottom") +
    
    guides(y = guide_axis(n.dodge = 1, check.overlap = TRUE)) +
    
    labs(x = "Age Groups", y = "Population (in millions)", 
         title = "Age Pyramids by Age and Sex",
         subtitle = "Total Population - São Paulo, Brazil",
         caption = "\n Source: Own calculations from CENSUS/PNAD")

# ---
# SCRIPT: creating plot to show age pyramid by gender in 2019
  
  # ggplot(data = filter(popEstimates_data_THREE_PLOT, YEAR == 2019),
  #        aes(x = AGE_GROUP, fill = GENDER, color = GENDER,
  #            y = ifelse(test = GENDER == "Male", yes = -TOTAL, no = TOTAL))) +
  #   
  #   geom_bar(stat = "identity", width = 0.7) +
  #   
  #   scale_y_continuous(labels = function(x) abs(x * 0.000001)) +
  #   
  #   coord_flip() +
  #   
  #   scale_colour_manual(values = c("#8D6A9F", "#00CC99"),
  #                       aesthetics = c("colour", "fill"),
  #                       labels = c("Female","Male")) +
  #   
  #   theme(axis.title = element_text(),
  #         legend.title = element_blank(),
  #         legend.position = "bottom") +
  #   
  #   guides(y = guide_axis(n.dodge = 1, check.overlap = TRUE)) +
  #   
  #   labs(x = "Age Groups", y = "Population (in millions)", 
  #        title = "Age Pyramids by Age and Sex: 2019",
  #        subtitle = "Total Population - São Paulo, Brazil",
  #        caption = "\n Source: Own calculations from CENSUS/PNAD")

  # ---
  # SCRIPT: creating plot to highlight how little of the race data is missing in 2010
  
  ggplot(data = filter(popEstimates_data_THREE_PLOT, YEAR == 2010) %>% 
           ungroup(.) %>% 
           group_by(., YEAR, GENDER, RACE_COLOR) %>%
           summarise(TOTAL = sum(TOTAL)), 
         aes(x = YEAR, fill = RACE_COLOR,
             y = TOTAL)) + 
    
    geom_col(position = position_dodge()) +
    
    scale_fill_manual(values = c("White" = "#ef476f",
                                 "Black" = "#ffd166",
                                 "Mixed" = "#118ab2",
                                 "Asian" = "#06d6a0",
                                 "Indigenous" = "#7F96FF",
                                 "Missing" = "#575761")) +
    
    scale_y_continuous(labels = function(x) abs(x * 0.000001),
                       limits = c(0, 15000000),
                       breaks = seq(0, 15000000, by = 5000000)) +
    
    facet_wrap(~ GENDER, strip.position = "top") +
    
    # scale_x_continuous(breaks = seq(2010, 2019, by = 9)) +
    
    theme(strip.placement = "outside", 
          axis.title = element_text(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          legend.title = element_blank(),
          legend.position = "bottom") +
    
    guides(x = guide_axis(n.dodge = 1, check.overlap = TRUE)) +
    
    labs(x = "", y = "Population (in millions)", 
         # title = "Population Distribution by Year and Race",
         # subtitle = "São Paulo, Brazil",
         # caption = "\n Source: Own calculations from CENSUS/PNAD"
    )
  
# ---
# SCRIPT: analyzing the missing information on race
  
  popEstimates_data_THREE_PLOT <- popEstimates_data_THREE_PLOT %>% 
    group_by(., YEAR, AGE_GROUP, GENDER) %>% 
    mutate(COMP_RATE = TOTAL / sum(TOTAL))

  ggplot(data = filter(popEstimates_data_THREE_PLOT, RACE_COLOR == "Missing"),
         aes(x = AGE_GROUP, fill = GENDER, color = GENDER,
             y = ifelse(test = GENDER == "Male", yes = -COMP_RATE, no = COMP_RATE))) +
    
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
    
    labs(x = "Age Groups", y = "Percentage of missing Race Information in the Population", 
         title = "Evolution of missing values regarding Race on the Population by Age Groups",
         subtitle = "São Paulo, Brazil",
         caption = "\n Source: Own calculations from CENSUS/PNAD")
  
# ---
# SCRIPT: creating plot to highlight how little of the race data is missing in 2010
  
  # ggplot(data = filter(popEstimates_data_THREE_PLOT, YEAR == 2010),
  #        aes(x = GENDER, fill = RACE_COLOR,
  #            y = COMP_RATE)) +
  # 
  #   geom_col(position = position_dodge()) +
  # 
  #   scale_fill_manual(values = c("White" = "#ef476f",
  #                                "Black" = "#ffd166",
  #                                "Mixed" = "#118ab2",
  #                                "Asian" = "#06d6a0",
  #                                "Indigenous" = "#7F96FF",
  #                                "Missing" = "#575761")) +
  # 
  #   scale_y_continuous(labels = function(x) abs(x * 0.000001)) +
  # 
  #   facet_wrap(~ AGE_GROUP, strip.position = "bottom") +
  # 
  #   theme(strip.placement = "outside",
  #         axis.title = element_text(),
  #         legend.title = element_blank(),
  #         legend.position = "bottom") +
  # 
  #   guides(x = guide_axis(n.dodge = 1, check.overlap = TRUE)) +
  # 
  #   labs(x = "Age Groups by Gender", y = "Population (in millions)",
  #        title = "Population Distribution by Race: 2010",
  #        subtitle = "São Paulo, Brazil",
  #        caption = "\n Source: Own calculations from CENSUS/PNAD")
  
# ---
# SCRIPT: creating plot to highlight how little of the race data is missing in 2019
  
  # ggplot(data = filter(popEstimates_data_THREE_PLOT, YEAR == 2019), 
  #        aes(x = GENDER, fill = RACE_COLOR,
  #            y = TOTAL)) + 
  #   
  #   geom_col(position = position_dodge()) +
  #   
  #   scale_fill_manual(values = c("White" = "#ef476f",
  #                                "Black" = "#ffd166",
  #                                "Mixed" = "#118ab2",
  #                                "Asian" = "#06d6a0",
  #                                "Indigenous" = "#7F96FF",
  #                                "Missing" = "#575761")) +
  #   
  #   scale_y_continuous(labels = function(x) abs(x * 0.000001)) +
  #   
  #   facet_wrap(~ AGE_GROUP, strip.position = "bottom") +
  #   
  #   theme(strip.placement = "outside", 
  #         axis.title = element_text(),
  #         legend.title = element_blank(),
  #         legend.position = "bottom") +
  #   
  #   guides(x = guide_axis(n.dodge = 1, check.overlap = TRUE)) +
  #   
  #   labs(x = "Age Groups by Gender", y = "Population (in millions)", 
  #        title = "Population Distribution by Race: 2019",
  #        subtitle = "São Paulo, Brazil",
  #        caption = "\n Source: Own calculations from CENSUS/PNAD")
  
  
# ---
# SCRIPT: creating plot to show age pyramids by race/color and gender considering all six categories
  
  ggplot(data = filter(popEstimates_data_THREE_PLOT),
         aes(x = AGE_GROUP, fill = RACE_COLOR, color = RACE_COLOR,
             y = ifelse(test = GENDER == "Male", yes = -TOTAL, no = TOTAL))) +
    
    geom_bar(stat = "identity", width = 0.7) +
    
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
    
    scale_y_continuous(labels = function(x) abs(x * 0.000001)) +
    
    coord_flip() +
    
    geom_hline(yintercept = 0) +
    
    facet_wrap(~ YEAR, strip.position = "top") +
    
    theme(axis.title = element_text(),
          legend.title = element_blank(),
          legend.position = "bottom") +
    
    guides(y = guide_axis(n.dodge = 1, check.overlap = TRUE)) +
    
    labs(x = "Age Groups", y = "Population (in millions)", 
         # title = "Age Pyramids by Age, Sex, and Race/Color: 2019",
         # subtitle = "Total Population - São Paulo, Brazil",
         # caption = "\n Source: Own calculations from CENSUS/PNAD"
         )

# ---
# SCRIPT: creating plot to show age pyramids by race/color and gender considering the three largest categories
  
  ggplot(data = filter(popEstimates_data_THREE_PLOT, YEAR == 2010, 
                       RACE_COLOR %in% c("White", "Mixed", "Black")),
         aes(x = AGE_GROUP, fill = RACE_COLOR, color = RACE_COLOR,
             y = ifelse(test = GENDER == "Male", yes = -TOTAL, no = TOTAL))) +
    
    geom_bar(stat = "identity", width = 0.7) +
    
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
    
    scale_y_continuous(labels = function(x) abs(x * 0.000001)) +
    
    coord_flip() +
    
    geom_hline(yintercept = 0) +
    
    # facet_wrap(~ YEAR, strip.position = "top") +
    
    theme(axis.title = element_text(),
          legend.title = element_blank(),
          legend.position = "bottom") +
    
    guides(y = guide_axis(n.dodge = 1, check.overlap = TRUE)) +
    
    labs(x = "Age Groups", y = "Population (in millions)", 
         #title = "Age Pyramids by Age, Sex, and Race/Color: 2019",
         #subtitle = "White, Mixed, and Black Population - São Paulo, Brazil",
         #caption = "\n Source: Own calculations from CENSUS/PNAD"
         )

# ---
# SCRIPT: creating plot to show age pyramids by race/color and gender in 2019 considering the two largest categories in 2019

  ggplot(data = filter(popEstimates_data_TWO_PLOT, RACE_COLOR %in% c("White", "Mixed_Black")),
         aes(x = AGE_GROUP, fill = RACE_COLOR, color = RACE_COLOR,
             y = ifelse(test = GENDER == "Male", yes = -TOTAL, no = TOTAL))) +
    
    geom_bar(stat = "identity", width = 0.7) +
    
    scale_fill_manual(values = c("White" = "#ef476f",
                                 "Mixed_Black" = "#118ab2",
                                 "Asian" = "#06d6a0",
                                 "Indigenous" = "#7F96FF",
                                 "Missing" = "#575761"),
                      labels = c("White", "Mixed + Black")) +
    
    scale_color_manual(values = c("White" = "#ef476f",
                                  "Mixed_Black" = "#118ab2",
                                  "Asian" = "#06d6a0",
                                  "Indigenous" = "#7F96FF",
                                  "Missing" = "#575761"),
                       labels = c("White", "Mixed + Black")) +
    
    scale_y_continuous(labels = function(x) abs(x * 0.000001)) +
    
    coord_flip() +
    
    geom_hline(yintercept = 0) +
    
    facet_wrap(~ YEAR, strip.position = "top") +
    
    theme(axis.title = element_text(),
          legend.title = element_blank(),
          legend.position = "bottom") +
    
    guides(y = guide_axis(n.dodge = 1, check.overlap = TRUE)) +
    
    labs(x = "Age Groups", y = "Population (in millions)", 
         #title = "Age Pyramids by Age, Sex, and Race/Color: 2019",
         #subtitle = "White and Mixed Population - São Paulo, Brazil",
         #caption = "\n Source: Own calculations from CENSUS/PNAD"
         )

# ---
# SCRIPT: creating stacked plot to show how the three largest categories are distributed in 2010 and 2019
  
  ggplot(data = filter(popEstimates_data_THREE_PLOT, RACE_COLOR %in% c("White", "Mixed", "Black")) %>% 
           group_by(., YEAR, GENDER, RACE_COLOR) %>% 
           summarise(TOTAL = sum(TOTAL)),
         aes(x = YEAR, y = TOTAL, color = GENDER)) +
    
    scale_colour_manual(values = c("#8D6A9F", "#00CC99"),
                        aesthetics = c("colour", "fill"),
                        labels = c("Female","Male")) +
    
    geom_line(linewidth = 0.8) +
    
    geom_point(size = 1.5) +
    
    facet_wrap(~ RACE_COLOR) +
    
    scale_x_continuous(breaks = seq(2010, 2019, by = 9)) +
    
    scale_y_continuous(labels = function(x) abs(x * 0.000001)) +
    
    theme(axis.title = element_text(),
          legend.title = element_blank(),
          legend.position = "bottom") +
    
    guides(y = guide_axis(n.dodge = 1, check.overlap = TRUE)) +
    
    labs(x = "Year", y = "Population (in millions)", 
         title = "Change of the Total Population by Race/Color: 2010 - 2019",
         subtitle = "São Paulo, Brazil",
         caption = "\n Source: Own calculations from CENSUS/PNAD")
  
# ---
# SCRIPT: creating stacked plot to show how the three largest categories are distributed in 2010 and 2019
  
  ggplot(data = filter(popEstimates_data_THREE_PLOT, RACE_COLOR %in% c("White", "Mixed", "Black")) %>% 
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
    
    geom_text(aes(label = paste0(round(SHARE * 100, digits = 2), "%")),
              color = "white",
              vjust = 1.5, size = 3.3, fontface = "bold") +
    
    scale_x_continuous(breaks = seq(2010, 2019, by = 9)) +
    
    scale_y_continuous(labels = function(x) abs(x * 100)) +
    
    facet_wrap(~ GENDER) +
    
    theme(axis.title = element_text(),
          legend.title = element_blank(),
          legend.position = "bottom") +
    
    guides(y = guide_axis(n.dodge = 1, check.overlap = TRUE)) +
    
    labs(x = "Year", y = "Share of Population by Race/Color (in %)", 
         # title = "Change of the Total Population by Race/Color: 2010 - 2019",
         # subtitle = "São Paulo, Brazil",
         # caption = "\n Source: Own calculations from CENSUS/PNAD"
         )

# ---
# SCRIPT: creating stacked plot to show how the three largest categories are distributed in 2010 and 2019
  
  ggplot(data = filter(popEstimates_data_TWO_PLOT, RACE_COLOR %in% c("White", "Mixed_Black")) %>% 
           group_by(., YEAR, GENDER, RACE_COLOR) %>% 
           summarise(TOTAL = sum(TOTAL)) %>%
           group_by(., YEAR, GENDER) %>% 
           mutate(SHARE = TOTAL / sum(TOTAL)),
         aes(x = YEAR, y = SHARE, fill = RACE_COLOR, color = RACE_COLOR)) +
    
    scale_fill_manual(values = c("White" = "#ef476f",
                                 "Black" = "#ffd166",
                                 "Mixed_Black" = "#118ab2",
                                 "Asian" = "#06d6a0",
                                 "Indigenous" = "#7F96FF",
                                 "Missing" = "#575761"),
                      labels = c("White","Mixed + Black")) +
    
    scale_color_manual(values = c("White" = "#ef476f",
                                  "Black" = "#ffd166",
                                  "Mixed_Black" = "#118ab2",
                                  "Asian" = "#06d6a0",
                                  "Indigenous" = "#7F96FF",
                                  "Missing" = "#575761"),
                       labels = c("White","Mixed + Black")) +
    
    geom_bar(position="fill", stat="identity") + 
    
    geom_text(aes(label = paste0(round(SHARE * 100, digits = 1), "%")),
              color = "white",
              vjust = 2, size = 3.3, fontface = "bold") +
    
    scale_x_continuous(breaks = seq(2010, 2019, by = 9)) +
    
    scale_y_continuous(labels = function(x) abs(x * 100)) +
    
    facet_wrap(~ GENDER) +
    
    theme(axis.title = element_text(),
          legend.title = element_blank(),
          legend.position = "bottom") +
    
    guides(y = guide_axis(n.dodge = 1, check.overlap = TRUE)) +
    
    labs(x = "Year", y = "Share of Population by Race/Color (in %)", 
         # title = "Change of the Total Population by Race/Color: 2010 - 2019",
         # subtitle = "São Paulo, Brazil",
         # caption = "\n Source: Own calculations from CENSUS/PNAD"
    ) 
  