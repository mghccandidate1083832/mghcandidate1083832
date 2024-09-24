####-----------------------------------------------------------####
#'
#'title: "Effective Cascade Complete Surveys Compilation and Analysis"
#'author: "Candidate_no_1083832"
#'date: "2024-04-06"
#'
######---------------------------------------------------------####
#'
####-----------------------------------------------------------####
#   This R code is adopted by the candidate to take 
#   DHS/MIS survey recode data variables related to fever,
#   excel outputs of the effective
#   coverage metrics by country, admin1 (sub national level), and survey by year
#   compiles them together and creates decriptive plots for these outputs
#      
#   The code requires the following files
#   
#   - Output files: denoted as dhs_effcov_hbhi_allSrvy_v4.csv in our data folder - An output Excel file containing the effective coverage estimate
#                                              corresponding to each survey year
#   
####-----------------------------------------------------------####
####-----------------------------------------------------------####

####=====================  1. Setup  =========================####

rm( list =  ls() )

library(readxl)
library(tidyverse)
library(dplyr)
library(scales)
library(ggradar)
library(gridExtra)

surveys_dir    <- "" # set the path to the working directory with the "Candidate_1083832_GitHub_Upload" folder has been downloaded
#'
#'
#' We load the data outputs for each survey year from our data folder
kenya_survey_2003 <- read.csv(file.path(surveys_dir,"data/dhs_effcov_hbhi_allSrvy_v4_2003.csv"), sep=",")
kenya_survey_2009 <- read.csv(file.path(surveys_dir,"data/dhs_effcov_hbhi_allSrvy_v4_2009.csv"), sep=",")
kenya_survey_2014 <- read.csv(file.path(surveys_dir,"data/dhs_effcov_hbhi_allSrvy_v4_2014.csv"), sep=",")
kenya_survey_2015 <- read.csv(file.path(surveys_dir,"data/dhs_effcov_hbhi_allSrvy_v4_2015.csv"), sep=",")
kenya_survey_2020 <- read.csv(file.path(surveys_dir,"data/dhs_effcov_hbhi_allSrvy_v4_2020.csv"), sep=",")
kenya_survey_2022 <- read.csv(file.path(surveys_dir,"data/dhs_effcov_hbhi_allSrvy_v4_2022.csv"), sep=",")

kenya_admin <- read.csv(file.path(surveys_dir,"data/admin_data.csv"), sep=",") #' We need this excel to be able to allocate the admin1 regions correctly for the years before 2022 when the
#'                                                                            surveys used provinces rather than counties as the current admin1 boundaries
#'
#'
###############
# SUBNATIONAL COMPILATION#
###############

# We are interested in counties and provinces within the admin files

kenya_admin = kenya_admin %>% 
  select(province, county) %>% 
  distinct()

kenya_admin$province <- tolower(kenya_admin$province)

colnames(kenya_admin)[1] <- "admin1"


# We exclude national values from DHS survey data

kenya_subnational_2003= kenya_survey_2003 %>% 
  filter(admin1 !="kenya") 

kenya_subnational_2009= kenya_survey_2009 %>% 
  filter(admin1 !="kenya") 

kenya_subnational_2014= kenya_survey_2014 %>% 
  filter(admin1 !="kenya") 

kenya_subnational_2015= kenya_survey_2015 %>% 
  filter(admin1 !="kenya") 

kenya_subnational_2020= kenya_survey_2020 %>% 
  filter(admin1 !="kenya") 

kenya_subnational_2022= kenya_survey_2022 %>% 
  filter(admin1 !="kenya")


# We edit some of the names for counties and provinces so that they match across datasets

kenya_subnational_2009$admin1[kenya_subnational_2009$admin1=="northeastern"] = "north eastern"

kenya_subnational_2022$admin1 <- str_to_title(kenya_subnational_2022$admin1)

colnames(kenya_subnational_2022)[7] <- "county"

kenya_subnational_2022$county[kenya_subnational_2022$county== "Murang'a"]="Muranga"


# We merge our admin file with the survey file to transform the data by county between 2003 and 2020

# Between 2003 and 2020, we are making the assumption that all counties within a province have some values for the DHS indicators

KE_dhs_2003 = merge(kenya_subnational_2003, kenya_admin, by="admin1")
KE_dhs_2009 = merge(kenya_subnational_2009, kenya_admin, by="admin1")
KE_dhs_2014 = merge(kenya_subnational_2014, kenya_admin, by="admin1")
KE_dhs_2015 = merge(kenya_subnational_2015, kenya_admin, by="admin1")
KE_dhs_2020 = merge(kenya_subnational_2020, kenya_admin, by="admin1")

KE_dhs_2003 = KE_dhs_2003 %>% 
  select(-admin1)

KE_dhs_2009 = KE_dhs_2009 %>% 
  select(-admin1)

KE_dhs_2014 = KE_dhs_2014 %>% 
  select(-admin1)

KE_dhs_2015 = KE_dhs_2015 %>% 
  select(-admin1)

KE_dhs_2020 = KE_dhs_2020 %>% 
  select(-admin1)


####=====================  ploting regional indicators ======================####

# We put together all DHS survey data for Kenya between 2003 and 2022

# List of all data frames to be combined

kenya_all_subnational_surveys = rbind(
  KE_dhs_2003,
  KE_dhs_2009,
  KE_dhs_2014,
  KE_dhs_2015,
  KE_dhs_2020,
  kenya_subnational_2022
)


kenya_all_subnational_surveys_wide <- kenya_all_subnational_surveys %>%
  group_by(year)%>%
  select(county, year, access, formal, compl_f, eff_cov)

write.csv(kenya_all_subnational_surveys_wide, "Kenya Counties Effective Cascade Model Outcome 2003 - 2022.csv") #All survey output compilation excel


lake_endemic_surveys <-  kenya_all_subnational_surveys%>%
  select(county, year, access, formal, compl_f, eff_cov) %>%
  filter(county == "Siaya" |  county =="Busia" | county == "Homa Bay"|county == "Kisumu"|
           county == "Migori"| county == "Bungoma" | county== "Kakamega" |county == "Vihiga")


#' Lake endemic region descriptive statistics
descriptive_stats_lake_endemic <- lake_endemic_surveys %>%
  group_by(county) %>%
  select_if(is.numeric) %>%
  summary(list(
    mean = ~mean(., na.rm = TRUE),
    median = ~median(., na.rm = TRUE),
    sd = ~sd(., na.rm = TRUE),
    min = ~min(., na.rm = TRUE),
    max = ~max(., na.rm = TRUE),
    n = ~sum(!is.na(.))
  ))

print(descriptive_stats_lake_endemic)



lake_endemic_surveys <-  kenya_all_subnational_surveys%>%
  select(county, year, access, formal, compl_f, eff_cov) %>%
  filter(county == "Siaya" |  county =="Busia" | county == "Homa Bay"|county == "Kisumu"|
           county == "Migori"| county == "Bungoma" | county== "Kakamega" |county == "Vihiga") %>%
  filter(year == 2022)

summary(lake_endemic_surveys)



write_csv(lake_endemic_surveys, "Lake_Endemic_Surveys.csv")

# Define the preferred definitions for the indicators
indicator_definitions <- c(
  "access" = "Access to Care Seeking",
  "compl_f" = "Provider First Line Policy Compliance",
  "eff_cov" = "Effective Coverage Estimate",
  "formal" = "Proportion Care Seeking in Formal System"
)

# Lake_endemic Zone Effective Coverage Estimate 2022 Plot

plot1 <- ggplot(lake_endemic_surveys) +
  geom_bar(aes(x = county, y = eff_cov), stat = "identity") +  
  geom_text(aes(x = county, y = eff_cov, label = scales::percent(eff_cov, accuracy = 1)), 
            vjust = -0.5, size = 3.5) +  
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Kenya Lake Endemic Zone Effective Coverage Estimate 2022",
    x = "County",  
    y = "Effective Coverage Estimate",
    caption = "Source : DHS"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) 

print(plot1)

kenya_all_subnational_surveys = rbind(
  KE_dhs_2003,
  KE_dhs_2009,
  KE_dhs_2014,
  KE_dhs_2015,
  KE_dhs_2020,
  kenya_subnational_2022
)

kenya_all_subnational_surveys = kenya_all_subnational_surveys %>% 
  select(county, year, access, formal, compl_f, eff_cov) %>% 
  pivot_longer(cols= access:eff_cov,
               names_to = "indicator",
               values_to = "value" )

kenya_all_subnational_surveys$value <- as.numeric(sub(",", ".", kenya_all_subnational_surveys$value, fixed = TRUE))

lake_endemic_surveys <-  kenya_all_subnational_surveys%>%
  filter(county == "Siaya" |  county =="Busia" | county == "Homa Bay"|county == "Kisumu"|
           county == "Migori"| county == "Bungoma" | county== "Kakamega" |county == "Vihiga") %>%
  filter(year == 2022)


# Lake_endemic Zone Care Cascade Indicators 2022 Plot

plot2 <- ggplot(lake_endemic_surveys, aes(x = indicator, y = value, fill = indicator)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Lake Endemic Zone Care Cascade Indicators - 2022",
    y = "Proportion",
    x = "Cascade Indicators",
    caption = "Source: DHS"
  ) +
  scale_fill_manual(
    values = c(
      "access" = "skyblue", 
      "compl_f" = "#3E2F5B", 
      "eff_cov" = "tomato", 
      "formal" = "purple"
    ),  
    labels = indicator_definitions
  ) +
  facet_wrap(~ county) +
  theme_minimal()+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

print(plot2)


# Creating a grouped bar plot
plot2a <- ggplot(lake_endemic_surveys, aes(x = county, y = value, fill = indicator)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Lake Endemic Region Care Cascade Indicators - 2022",
    y = "Proportion",
    x = "County",
    caption = "Source: DHS"
  ) +
  scale_fill_manual(
    values = c(
      "access" = "skyblue", 
      "formal" = "#3E2F5B", 
      "compl_f" = "tomato", 
      "eff_cov" = "purple"
    )
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(plot2a)

# Creating a heatmap
heatmap_data <- lake_endemic_surveys %>%
  pivot_wider(names_from = indicator, values_from = value)

plot2b <- ggplot(lake_endemic_surveys, aes(x = indicator, y = county, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red", labels = scales::percent) +
  labs(
    title = "Lake Endemic Region Care Cascade Indicators - 2022",
    y = "County",
    x = "Cascade Indicators",
    caption = "Source: DHS"
  ) +
  theme_minimal()

print(plot2b)



getwd()
############
# NATIONAL #
############


#Select national level cascade output
kenya_national_2003= kenya_survey_2003 %>% 
  filter(admin1 =="kenya") 

kenya_national_2009= kenya_survey_2009 %>% 
  filter(admin1 =="kenya") 

kenya_national_2014= kenya_survey_2014 %>% 
  filter(admin1 =="kenya") 

kenya_national_2015= kenya_survey_2015 %>% 
  filter(admin1 =="kenya") 

kenya_national_2020= kenya_survey_2020 %>% 
  filter(admin1 =="kenya") 

kenya_national_2022= kenya_survey_2022 %>% 
  filter(admin1 =="kenya")

# merge all years
kenya_all_national_surveys = rbind(kenya_national_2003,
                                   kenya_national_2009,
                                   kenya_national_2014,
                                   kenya_national_2015,
                                   kenya_national_2020,
                                   kenya_national_2022)


kenya_all_national_surveys = kenya_all_national_surveys %>% 
  select(year, access, formal, compl_f, eff_cov)

print(kenya_all_national_surveys)
summary(kenya_all_national_surveys)

#' Sub national descriptive statistics
descriptive_stats_national <- kenya_all_national_surveys %>%
  select_if(is.numeric) %>%
  summary(list(
    mean = ~mean(., na.rm = TRUE),
    median = ~median(., na.rm = TRUE),
    sd = ~sd(., na.rm = TRUE),
    min = ~min(., na.rm = TRUE),
    max = ~max(., na.rm = TRUE),
    n = ~sum(!is.na(.))
  ))

print(kenya_all_national_surveys)

#' National descriptive statistics
descriptive_stats_national_2022 <- kenya_all_national_surveys %>%
  filter(year == 2022) %>%
  select_if(is.numeric) %>%
  summary(list(
    mean = ~mean(., na.rm = TRUE),
    median = ~median(., na.rm = TRUE),
    sd = ~sd(., na.rm = TRUE),
    min = ~min(., na.rm = TRUE),
    max = ~max(., na.rm = TRUE),
    n = ~sum(!is.na(.))
  ))

print(descriptive_stats_national_2022)

#Malaria Case Management Cascade Effective Coverage Estimate in Kenya 2003 - 2022 Plot


plot3 <- ggplot(kenya_all_national_surveys, aes(x = year, y = eff_cov)) +
  geom_point(size = 1.5) +
  geom_line(linewidth = 1) +
  geom_text(aes(x = year, y = eff_cov, label = scales::percent(eff_cov, accuracy = 1)), 
            size = 3, nudge_y = 0.03, vjust = 1) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Malaria Case Management Effective Coverage in Kenya, 2003 - 2022",
    x = "Year",
    y = "Proportion",
    caption = "Source: DHS",
    color = "Effective Coverage Estimate"  
  ) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

print(plot3)

kenya_all_national_surveys_long = rbind(kenya_national_2003,
                                        kenya_national_2009,
                                        kenya_national_2014,
                                        kenya_national_2015,
                                        kenya_national_2020,
                                        kenya_national_2022) %>%
  select(country, year, access, formal, compl_f, eff_cov)%>%
  pivot_longer(cols= access:eff_cov,
               names_to = "Indicators",
               values_to = "value" )




#Malaria Case Management Cascade Indicators in Kenya 2003 - 2022 Plot

indicator_definitions <- c(
  "access" = "Access to Care Seeking",
  "compl_f" = "Provider First Line Policy Compliance",
  "eff_cov" = "Effective Coverage Estimate",
  "formal" = "Proportion Care Seeking in Formal System"
)
plot4 <- ggplot(kenya_all_national_surveys_long, aes(x = year, y = value, color = Indicators)) +
  geom_point(size = 5) +
  geom_line(linewidth = 1.5) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Malaria Case Management Cascade Indicators in Kenya 2003 - 2022",
    x = "Year",
    y = "",
    caption = "Source: DHS",
    color = "Malaria Cascade Indicators"  
  ) +
  scale_color_manual(
    values = c(
      "access" = "skyblue", 
      "compl_f" = "#3E2F5B", 
      "eff_cov" = "tomato", 
      "formal" = "purple"
    ),  
    labels = indicator_definitions
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = c(2003, 2008, 2015, 2020, 2022))

print(plot4)

