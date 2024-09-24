####-----------------------------------------------------------####
#'
#'title: "Dynamic Modelling of Malaria Transmission for Case Management Impact"
#'author: "Candidate_no_1083832"
#'date: "2024-04-06"
######---------------------------------------------------------####
#'
####-----------------------------------------------------------####
#   This R code is adopted by the candidate to take a country's children recode 
#   DHS/MIS survey recode data variables related to fever,
#   health-seeking, and drug treatment compliance to derive malaria effective
#   coverage metrics by country, admin1 (sub national level), and survey by year.
#   The code also requires adherence, cure and resistance rates for the antimalarial
#   drugs informed by literature review.
#   
#   The code was created and developed by  Katya Galactionova (e.galactionova@unibas.ch)
#    and was converted from Stata to R by Rémi Turquier (remi.turquier@ensae.fr).
# 
#   
#   The code requires the following files
#   - RX recodes: dhsrecode_rx_hbhi.csv - An Excel file that allows for the code
#                                         to pull and recode drug and treatment data
#                                         from the recoded DHS survey data for each survey round
#
#   - HSB recodes: dhsrecode_hsb_hbhi.csv - An Excel file that allows for the code
#                                           to pull and recode drug and treatment data
#                                           from the raw DHS survey data for each survey round
#
#   - DHS data - Children recode survey data denoted by a KR prefix files downloaded from the DHS website
#                for the analysis preriod. In our case the surveys for the years 2003 -2022
#   
#   - Output: dhs_effcov_hbhi_allSrvy_v4.csv - An output Excel file containing the effective coverage estimate
#                                              corresponding to each survey year
#   
#  
#
#   Cascade Model Code was Last updated: August 21, 2020
####-----------------------------------------------------------####
####-----------------------------------------------------------####
#
####------------ Acronyms and abbreviations used ------------
#' `act`       artemisinin-based combination therapy, first-line anti-malarial
#' `antimal`   antimalarial
#' `chw`       community health worker
#' `compl`     compliance (proportion of first line antimalarials given)
#' `compl_f`   compliance in the formal health sector
#' `compl_i`   compliance in the informal health sector
#' `df`        data frame
#' `dhs`       Demographic and Health Surveys
#' `dir`       directory
#' `fline`     first line
#' `hc`        health center
#' `hf`        health facility
#' `hsb`       health seeking behavior
#' `rx`        drugs

####=====================  1. Setup  =========================####
rm( list =  ls() )
library(tidyverse)
library(dplyr)
library(labelled)
library(haven)  # To read STATA format DHS .dta files


setwd("") # set the working directory to the path where the "Candidate_1083832_GitHub_Upload" folder has been downloaded

#'Set paths to the two mapping files, to the DHS directory and to 
#'the output directory.
#'
#'For the 2003 survey data we will work with diffrent assumptions compared to other years,
#'because after 2004, kenya introduced ACTs as the first line treatment policy and as such
#'we use more recent data from recent reviews
#'
dhs_dir <- "paste working directory path here" # Paste the path to the working directory here. Sets path to directory containing the Candidate_1083832_GitHub_Upload folder
hsb_path   <- "dhs_dir/data/kenya_2003_dhsrecode_hsb_hbhi.csv" #paste hsb file from the data folder for each year here in our example we are using 2003 file# #sets path to the excel health seeking behaviour mapping for each survey year
rx_path    <- "dhs_dir/data/kenya_2003_dhsrecode_rx_hbhi.csv" #paste rx file from the data folder for each year here in our example we are using 2003 file# #sets path to the excel health seeking behaviour mapping for each survey year
dhs_file   <- "dhs_dir/data/KEKR42FL2003.DTA" #paste .dta file from the data folder for each year here in our example we are using 2003 file# #sets path to the DHS recode data file for each survey year
output_dir <- "dhs_dir/Candidate_1083832_GitHub_Upload/eff_cov_output" #Sets path to output folder in the Candidate_1083832_GitHub_Upload folder
#'
#'
#' Set the country name as it appears in the mapping files -  corresponds to country of analysis
countries <- c("kenya")

# Set the period over which metrics should be calculated -  
#' `year_range` can be a list of years, or a single year. For our example we are using the 2022 year as we are using the data from same year
year_range <- 2003

#'
#'
#' We set up functions to perform data manipulation in our files as set above
#' 
####=====================  2. Functions  ======================####
dhs_files          <- function(country_name, year_range){
  # Returns a list of DHS file names associated with a given country
  country_name <- tolower(country_name)
  hsb_mapping %>%
    dplyr::filter(country == country_name, startyear %in% year_range) %>%
    dplyr::select(filename) %>%
    unique() %>%
    pull()
}

recode_dhs <- function(df, country, dhs_file){
  # Returns a recoded DHS dataframe

  df %>%
    rename("year"   = "v007",
           "month"  = "v006",
           "admin1" = "v024",
           "alive"  = "b5") %>%
    mutate(
      "country" = country,
      survey    = dhs_file,
      year      = as.numeric(year),
      year      = case_when(year < 90               ~ 2000 + year, 
                            year >= 90 & year <= 99 ~ 1900 + year,
                            TRUE                    ~        year),
      year      = max(year),
      urban     = v102 == 1,
      weight    = v005 / 1000000,
      agemm     = v008 - b3,             #  age in months at interview
      fever     = (alive == 1 & h22 == 1)
    ) %>%
    recode_dhs_access(dhs_file) %>%
    recode_dhs_drugs(dhs_file) %>%
    recode_dhs_test()
}

recode_dhs_access  <- function(df, dhs_file){
  #' Called from `recode_dhs`
  #' Returns a further recoded DHS dataframe:
  #' Creates variables related to care access based on the `hsb_mapping` file

  #' The place at which medical treatment or advice was sought for 
  #' the last episode of fever and/or cough. This question has 
  #' multiple coding categories and each category is recorded
  #' separately in these variables. Most of the categories are 
  #' standard (H32A, B, C, D, E, J, K, L, M, N, S, T, X). However, 
  #' room has been left for country-specific categories (H32F, G, 
  #' H, I, O, P, Q, R, U, V, W). Any category not used in a 
  #' particular country is left blank. 
  
  # hf_level_names <- paste("h32", letters[1:24], sep="")
  
  hf_level_names <- 
    hsb_mapping %>%
    dplyr::filter(filename == dhs_file) %>%
    dplyr::select(varname) %>%
    unique() %>%
    pull()
  
  
  # recode to facility levels -9/7
  for (hf_level_name in hf_level_names){
    new_value <- hsb_mapping %>%
      dplyr::filter(varname == hf_level_name, filename == dhs_file) %>%
      dplyr::select(newvalue) %>%
      unique() %>%
      pull()

    df[[hf_level_name]] <- 
      case_when(
        df[[hf_level_name]] == 1 ~ new_value,
        df[[hf_level_name]] == 0 ~ 0,
        TRUE ~ -9
      )
  }
  
  df %>%
    mutate(
      #' `carelevel` is the highest level of care provider accessed by each 
      #' individual with fever.
      carelevel = case_when(
        fever ~ pmax(!!!rlang::syms(hf_level_names))
      ),
      access    = case_when(fever          ~ carelevel %in% 2:7),
      formal    = case_when(fever & access ~ carelevel %in% 3:7),
      self      = case_when(fever          ~ carelevel  ==   2 ),
      chw       = case_when(fever & formal ~ carelevel  ==   3 ),
      hcnobeds  = case_when(fever & formal ~ carelevel  ==   4 ),
      govcntr   = case_when(fever & formal ~ carelevel  ==   5 ),
      hcbeds    = case_when(fever & formal ~ carelevel  ==   6 ),
      opd       = case_when(fever & formal ~ carelevel  ==   7 )
    )
  
}

recode_dhs_drugs   <- function(df, dhs_file){
  #' Called from `recode_dhs`
  #' Returns a further recoded DHS dataframe:
  #' Creates variables related to drugs taken based on the `rx_mapping` file
  
  # Return dataframe if rx questions were not part of the module
  if (no_drug_data(dhs_file)){
    return(df)
  }
  
  # Get list of drug level variables
  drug_vars <- 
    rx_mapping %>%
    dplyr::filter(filename == dhs_file) %>%
    dplyr::select(varname) %>%
    unique() %>%
    pull()
  
  for (drug_var in drug_vars){
    new_value <- rx_mapping %>%
      dplyr::filter(varname == drug_var, filename == dhs_file) %>%
      dplyr::select(newvalue) %>%
      pull()
    
    df[[drug_var]] <- 
      case_when(
        df[[drug_var]] ==  1 ~ new_value,
        df[[drug_var]] ==  0 ~  0,
        df[[drug_var]] == -8 ~ -8,
        TRUE ~ -9
      ) 
  }
  
  df %>%
    mutate(
      #' `druglevel` is the highest level of drug quality accessed by each 
      #' individual with fever.
      druglevel = case_when(
        fever ~ pmax(!!!rlang::syms(drug_vars))
      ),
      other       = case_when(fever & access ~ druglevel  ==   2 ),
      othrantimal = case_when(fever & access ~ druglevel  ==   3 ),
      fline       = case_when(fever & access ~ druglevel  ==   4 ),
      anyantimal  = fline | othrantimal,
      compl_f     = case_when(anyantimal &  formal ~ fline),
      compl_i     = case_when(anyantimal & !formal ~ fline)
    )
}

recode_dhs_test    <- function(df){
  #' Called from `recode_dhs`
  #' Returns a further recoded DHS dataframe:
  #' Creates a `test`variable, which says whether blood was taken 
  #' from child's finger/heel for testing.
  
  if ("h47" %in% colnames(df)){
    df %>%
      mutate(test = if_else(
                      access & fever & h47 < 8,
                      h47 == 1,
                      NA
                    )
      )
  } else {
    df %>% mutate(test = NA)
  }
  
}

no_drug_data       <- function(dhs_file){
  #' Called from `recode_dhs_drugs`
  #' Returns `True` if no questions about drugs are available for a given survey.
   
  rx_mapping %>% 
    dplyr::filter(filename == dhs_file) %>%
    nrow(.) == 0
}

survey_metrics     <- function(df){
  #' Takes a recoded DHS dataframe, summarises it, and calculates effective coverage.
  #' 
  #' Returns a summarised dataframe with effective coverage at admin1 and country level.
  #' 
  # --- admin1 level (Sub national level according to the sampling stratifying data in the DHS data)---
  admin1_mean <- 
    df %>% 
    group_by(admin1) %>%
    summarise(
              fever       = weighted.mean(fever,       weight, na.rm = T),
              access      = weighted.mean(access,      weight, na.rm = T), 
              formal      = weighted.mean(formal,      weight, na.rm = T), 
              test        = weighted.mean(test,        weight, na.rm = T), 
              fline       = weighted.mean(fline,       weight, na.rm = T), 
              othrantimal = weighted.mean(othrantimal, weight, na.rm = T), 
              compl_f     = weighted.mean(compl_f,     weight, na.rm = T), 
              compl_i     = weighted.mean(compl_i,     weight, na.rm = T), 
              hcnobeds    = weighted.mean(hcnobeds,    weight, na.rm = T), 
              hcbeds      = weighted.mean(hcbeds,      weight, na.rm = T),
              govcntr     = weighted.mean(govcntr,     weight, na.rm = T), 
              chw         = weighted.mean(chw,         weight, na.rm = T), 
              opd         = weighted.mean(opd,         weight, na.rm = T), 
              urban       = weighted.mean(urban,       weight, na.rm = T),  
              year        = max(year),
              obs         = n()
       )
  
  #' Create `admin_level`, which is the name of the admin1 units in the country (eg. `"region"`)
  admin1_mean[["admin_level"]] <- 
    attr(admin1_mean[["admin1"]], "label") %>%
    tolower()
  
  #' Save numbers identifying admin1s to variable `admin1_num`.
  #' This might by useful to get around typos / spelling errors when working at admin1 level.
  admin1_mean <- admin1_mean %>%
    mutate(admin1_num = admin1)
  
  #' Replace admin1 units numbers by their names 
  #' (eg. replace `1` with `"mombasa"`).
  admin1_mean[["admin1"]] <- 
    admin1_mean[["admin1"]] %>% as_factor() %>% as.character()
  
  # --- country level ---
  country_mean <-  df %>%
    summarise(
              fever       = weighted.mean(fever,       weight, na.rm = T),
              access      = weighted.mean(access,      weight, na.rm = T), 
              formal      = weighted.mean(formal,      weight, na.rm = T), 
              test        = weighted.mean(test,        weight, na.rm = T), 
              fline       = weighted.mean(fline,       weight, na.rm = T), 
              othrantimal = weighted.mean(othrantimal, weight, na.rm = T), 
              compl_f     = weighted.mean(compl_f,     weight, na.rm = T), 
              compl_i     = weighted.mean(compl_i,     weight, na.rm = T), 
              hcnobeds    = weighted.mean(hcnobeds,    weight, na.rm = T), 
              hcbeds      = weighted.mean(hcbeds,      weight, na.rm = T),
              govcntr     = weighted.mean(govcntr,     weight, na.rm = T), 
              chw         = weighted.mean(chw,         weight, na.rm = T), 
              opd         = weighted.mean(opd,         weight, na.rm = T), 
              urban       = weighted.mean(urban,       weight, na.rm = T),  
              year        = max(year),
              obs         = n()
    )
  
  country_mean[["admin_level"]] <- "country"

  country_mean[["admin1"]] <-
    df %>% dplyr::select(country) %>% unique() %>% pull()
  
  # --- Binds admin1 and country level and calculates effective coverage ---
  metrics_df <- 
    bind_rows(admin1_mean, country_mean) %>%
    mutate(
      eff_cov     = effective_coverage(access, formal, compl_f, compl_i),
      eff_cov_raw = eff_cov,
      
      # Aggregates provider categories as defined by WHO-CHOICE
      #'Note: for treatments by chws costed only drugs, assumes CHWs are
      #' not paid and are only costed for commodities #'
      
      hcnobeds    = hcnobeds + govcntr
    )
  
  # Adds the name of the country and the name of the survey file to the output
  metrics_df[["country"]] <-
    df %>% dplyr::select(country) %>% unique() %>% pull()
  metrics_df[["survey"]]  <-
    df %>% dplyr::select(survey) %>% unique() %>% pull()
  
  # Return
  metrics_df
  
}

effective_coverage <- function(access, formal, compl_f, compl_i){
  # Calculates effective coverage metric
  #' Assumes that any malaria fever was treated with an antimalarial, 
  #' thus adherence calculated as a fraction of any antimalarial;
  # inputs for effective coverage calculation
  #' Assumptions on adherence and cure rates informed by literature 
  #' See Galactionova et al 2015
  adh_act_f          <- 0.8
  cure_act_f         <- 0.989
  adh_act_i          <- adh_act_f * 0.8
  cure_act_i         <- cure_act_f - 0.17
  cure_othrantimal_f <- 0.53
  cure_othrantimal_i <- cure_othrantimal_f - 0.33  
  # return effective coverage.
  #' If nobody accessed care providers, then the effective coverage is 0.
  if_else(
    access == 0,
    0,
    access * ( 
      formal     
      * (compl_f * adh_act_f * cure_act_f + (1 - compl_f) * cure_othrantimal_f)
    + (1 - formal)
      * (compl_i * adh_act_i * cure_act_i + (1 - compl_i) * cure_othrantimal_i)
    )
  )
}

impute_missings    <- function(df){
  #' Uses country averages to impute compliance where it is missing. 
  #' Then recalculates effective coverage with the imputed values where needed.
  #' Returns a df without missing effective coverage.
  
  country_averages <- 
    df %>% 
    dplyr::filter(admin_level == "country") %>%
    ungroup() %>%
    dplyr::select(survey,
                  country_fline = fline,
                  country_othrantimal = othrantimal)
  
  df %>% 
    left_join(country_averages, by = "survey") %>%
    mutate( 
      c_missing = fline == 0        | othrantimal == 0        |
                  fline %>% is.na() | othrantimal %>% is.na()
    ) %>%
    group_by(country) %>%
    mutate(
      # if information on compliance is missing impute based on country average
      fline       = if_else(c_missing, country_fline,       fline      ),
      othrantimal = if_else(c_missing, country_othrantimal, othrantimal),
      
      # for eff cov assume lower compliance in formal sector (80%)
      compl_f = if_else(
        c_missing | compl_f %>% is.na() | compl_i %>% is.na(),
        (fline/(fline+othrantimal))/(formal+(1-formal)*0.8),
        compl_f
      ),
      compl_i = if_else(
        c_missing | compl_f %>% is.na() | compl_i %>% is.na(),
        compl_f * 0.8,
        compl_i
      ),
      
      # impute effective coverage
      eff_cov = if_else(
        c_missing | eff_cov %>% is.na(),
        effective_coverage(access, formal, compl_f, compl_i),
        eff_cov
      ),
      
      # flag imputed values
      imputed = (eff_cov != eff_cov_raw | eff_cov_raw %>% is.na())
    )
}

  #---- Additional functions - plots ---- 
#' These functions can be called *after* running the routine cascade code
#' to create plots of the effective coverage routine.

latest_year    <- function(country_name){
  #' Called by the `plot_*` fucntions
  #' Returns the latest available year for a given country
  result_df %>% 
    dplyr::filter(country     == country_name,
                  year        == max(year),
                  admin_level == "country") %>%
    ungroup() %>%
    dplyr::select(year) %>%
    unique() %>%
    pull()
}


plot_waterfall <- function(waterfall_country, waterfall_year = NULL){
  #' Plots output plots after running the effective cascade code
  #' for each year as described above
  #' Can be called after running the routine
  #' 
  #' Creates waterfall chart of the effective coverage cascade for a given
  #' country (`waterfall_country`)
  #' 
  #' `waterfall_year` is an optional argument. If not specified, the function
  #' will plot for the latest available year. 
  
  
  if(!require("waterfalls")) stop("Please install `waterfalls` package.")
  
  #' Use latest available year if `waterfall_year` is not specified
  if(waterfall_year %>% is.null()){
    waterfall_year <- latest_year(waterfall_country)
  } 
  
  # Assumptions on adherence
  adh_act_f          <- 0.905
  adh_act_i          <- adh_act_f * 0.8
  adh_othrantimal_f  <- 1    #? Assumed complete adherence for non ACT antimalarial prescribed in formal sector
  adh_othrantimal_i  <- 1    #? Assumed complete adherence for non ACT antimalarial prescribed in the informal sector
  
  # Create a dataframe with the right format
  waterfall_df <-
    result_df %>% 
    dplyr::filter(country     == waterfall_country,
           year        == waterfall_year,
           admin_level == "country"
           ) %>%
    mutate(
      cases               = 100,
      lost_access         = - 100 * (1 - access),
      remain_adherence = access * 
        (
          formal       * (
            compl_f       * adh_act_f
            + (1 - compl_f) * adh_othrantimal_f
          )
          + (1 - formal) * (
            compl_i       * adh_act_i
            + (1 - compl_i) * adh_othrantimal_i
          )
        ),
      lost_adherence   = - 100 * (1 - remain_adherence) - lost_access,
      lost_cure        = - 100 * (1 - eff_cov) - lost_adherence - lost_access
    ) %>%
    ungroup() %>%
    unique() %>%
    dplyr::select(cases, lost_access, lost_adherence, lost_cure) %>%
    mutate_all(~round(., digits = 1)) %>%
    reshape2::melt()
  
  # create waterfall chart with total column
  waterfall(waterfall_df, 
            calc_total = TRUE, 
            total_rect_text_color="black",
            total_rect_color="seagreen3") +
    scale_y_continuous(label=function(x) paste0(x, "%")) +
    labs(title = paste("Effective coverage in", waterfall_country), 
         subtitle = paste("Year", waterfall_year),
         y="", 
         x="") +
    scale_x_discrete(labels = c("All malaria\nfevers", 
                                "No access\nto care", 
                                "Incomplete\nadherence\nto treatment",
                                "Drug\ninefficiencies",
                                "Effective\ncoverage"
    )
    ) +
    theme_minimal() 
}


#' Other types of plot that could be called for the effective coverage estimate cascade
#' 
#' Bar Plot
#' 
plot_bars      <- function(bars_country, bars_year = NULL){
  #' Plots type output plots after running the effective cascade code
  #' for each year as described above
  #' Can be called after running the routine
  #' 
  #' Creates a bars chart of the effective coverage cascade for a given
  #' country (`bars_country`)
  #' 
  #' `bars_year` is an optional argument. If not specified, the function
  #' will plot for the latest available year. 
  
  #' Use latest available year if `bars_year` is not specified
  if(bars_year %>% is.null()){
    bars_year <- latest_year(bars_country)
  } 
  
  # Assumptions on adherence
  adh_act_f          <- 0.905
  adh_act_i          <- adh_act_f * 0.8
  adh_othrantimal_f  <- 1    #? Assumed complete adherence for non ACT antimalarial prescribed in formal sector
  adh_othrantimal_i  <- 1    #? Assumed complete adherence for non ACT antimalarial prescribed in the informal sector
  
  # Create a dataframe with the right format
  bars_df <-
    result_df %>% 
    dplyr::filter(country     == bars_country,
                  year        == bars_year,
                  admin_level == "country"
    ) %>%
    mutate(
      cases            = 100,
      remain_access    = 100 * access,
      remain_adherence = 100 * access * 
        (
          formal       * (
            compl_f       * adh_act_f
            + (1 - compl_f) * adh_othrantimal_f
          )
          + (1 - formal) * (
            compl_i       * adh_act_i
            + (1 - compl_i) * adh_othrantimal_i
          )
        ),
      remain_cure        = 100 * eff_cov,
      total              = NA
    ) %>%
    ungroup() %>%
    dplyr::select(cases, 
                  remain_access, 
                  remain_adherence, 
                  remain_cure, 
                  total
                  ) %>%
    unique() %>%
    reshape2::melt(measure.vars = 
                     c("cases", 
                       "remain_access", 
                       "remain_adherence", 
                       "remain_cure", 
                       "total")
                    ) %>%
    mutate(
      background_value = lag(value),
      percent          = 100 * value / background_value
    )
  
  bars_background <- ggplot(bars_df) +
    geom_col(mapping = aes(variable, 
                           background_value, 
                           fill = (variable == "total"),
    ),
    color = "black"
    ) +
    scale_fill_manual(values = c("#F8766D", "seagreen3")) +
    guides(fill = FALSE) +
    geom_text(data = subset(bars_df, variable == "total"),
              mapping = aes(
                variable, background_value, 
                label = background_value %>% round(1)
              ),
              position = position_stack(vjust = 0.5),
    ) +
    scale_y_continuous(label=function(x) paste0(x, "%")) +
    labs(title = paste("Effective coverage in", bars_country), 
         subtitle = paste("Year", bars_year),
         y="", 
         x="") +
    scale_x_discrete(labels = c("All malaria\nfevers", 
                                "Access\nto care", 
                                "Adherence\nto treatment",
                                "Drugs\neffectiveness",
                                "Effective\ncoverage"
    )
    ) +
    theme_minimal() 
  
  bars_background +
    geom_col(mapping = aes(variable, value), 
             fill = "#00BFC4", 
             color= "black") +
    geom_text(data = subset(bars_df, grepl("remain", variable)),
              mapping = aes(
                variable, value, 
                label = percent %>% round(1) %>% paste0("%")
              ),
              position = position_stack(vjust = 0.5),
    )
}


#' Alluvial chart Plot
#' 
plot_alluvial <- function(alluvial_country, alluvial_year = NULL){
  #' Plots output plots after running the effective cascade code
  #' for each year as described above
  #' Can be called after running the routine
  #' 
  #' Creates alluvial chart for a given country (`alluvial_country`)
  #' Can be used in complement of `plot_bars` to see the proportion of the 
  #' formal and informal sectors, and the proportion of first line
  #' antimalarials in the two different sectors. 
  #' 
  #' `alluvial_year` is an optional argument. If not specified, the function
  #' will plot for the latest available year. 
  
  if(!require("ggalluvial")) stop("Please install `ggalluvial` package.")
  
  #' Use latest available year if `alluvial_year` is not specified
  if(alluvial_year %>% is.null()){
    alluvial_year <- latest_year(alluvial_country)
  } 
  
  
  # Create dataframe with the right format
  alluvial_df <- tibble(
    sector = c("formal", "informal"),
    drug_quality = c("first line", "second line")
  ) %>%
    expand(sector, drug_quality)
  
  values <- result_df %>% 
    filter(admin_level == "country",
           country == alluvial_country,
           year    == alluvial_year) %>%
    ungroup() %>%
    select(formal, compl_f, compl_i) %>%
    unique()
  
  prop_df <- tibble(
    prop = with(values,
                c(
                  formal * compl_f,
                  formal * (1 - compl_f),
                  
                  (1 - formal) * compl_i,
                  (1 - formal) * (1 - compl_i)
                )
    )
  )
  
  alluvial_df <- alluvial_df %>% bind_cols(prop_df)
  
  
  
  #' Reorder variables so that the rectangles (or "strata") of the plot are in the
  #' right order.
  alluvial_df <- alluvial_df %>%
    mutate(
      sector = recode_factor(sector,
                             "formal"    = "formal",
                             "informal" = "informal",
                             .ordered   = T
      ),
      drug_quality = recode_factor(drug_quality,
                                   "first line"  = "first line",
                                   "second line" = "second line",
                                   .ordered      = T
      )
    )
  
  
  # Plot
  ggplot(alluvial_df,
         aes(axis1 = sector,
             axis2 = drug_quality,
             y = prop)) +
    geom_stratum(reverse = T) +
    geom_alluvium(aes(fill = sector), reverse = T) +
    geom_text(stat = "stratum", 
              aes(label = after_stat(stratum)), reverse = T) +
    scale_x_discrete(limits = c("sector", "drug quality"),
                     expand = c(.1, .1)) +
    labs(title = paste("Type of care providers and antimalarials \nin", 
                       alluvial_country),
         subtitle = paste("Year", alluvial_year),
         y = "") +
    theme_minimal() +
    theme(legend.position = "none") 
}

#' Alluvial With Access Chart
#' 
plot_alluvial_with_access <- function(alluvial_country, 
                                      alluvial_year = NULL){
  #' Plots output plots after running the effective cascade code
  #' for each year as described above
  #' Can be called after running the routine
  #' 
  #' Same plot as `plot_alluvial`, but also represents `access`.
  
  if(!require("ggalluvial")) stop("Please install `ggalluvial` package.")
  
  #' Use latest available year if `alluvial_year` is not specified
  if(alluvial_year %>% is.null()){
    alluvial_year <- latest_year(alluvial_country)
  } 
  
  
  # Create dataframe with the right format
  alluvial_df <- tibble(
    sector = c("formal", "informal", "no access"),
    drug_quality = c("first line", "second line", "no antimalarial")
  ) %>%
    expand(sector, drug_quality)
  
  values <- result_df %>% 
    filter(admin_level == "country",
           country == alluvial_country,
           year    == alluvial_year) %>%
    ungroup() %>%
    select(access, formal, compl_f, compl_i) %>%
    unique()
  
  prop_df <- tibble(
    prop = with(values,
                c(
                  access * formal * compl_f,
                  0,
                  access * formal * (1 - compl_f),
                  
                  access * (1 - formal) * compl_i,
                  0,
                  access * (1 - formal) * (1 - compl_i),
                  
                  0,
                  (1 - access),
                  0
                )
    )
  )
  
  alluvial_df <- alluvial_df %>% bind_cols(prop_df)
  
  
  
  #' Reorder variables so that the rectangles (or "strata") of the plot are in the
  #' right order.
  alluvial_df <- alluvial_df %>%
    mutate(
      sector = recode_factor(sector,
                             "formal"    = "formal",
                             "informal" = "informal",
                             "no access" = "no access",
                             .ordered   = T
      ),
      drug_quality = recode_factor(drug_quality,
                                   "first line"  = "first line",
                                   "second line" = "second line",
                                   "no antimalarial"  = "no antimalarial",
                                   .ordered      = T
      )
    )
  
  
  # Plot
  ggplot(alluvial_df,
         aes(axis1 = sector,
             axis2 = drug_quality,
             y = prop)) +
    geom_stratum(reverse = T) +
    geom_alluvium(aes(fill = sector), reverse = T) +
    geom_text(stat = "stratum", 
              aes(label = after_stat(stratum)), reverse = T) +
    scale_x_discrete(limits = c("sector", "drug quality"),
                     expand = c(.1, .1)) +
    labs(title = paste("Access to care providers and antimalarials \nin", 
                       alluvial_country),
         subtitle = paste("Year", alluvial_year),
         y = "") +
    theme_minimal() +
    theme(legend.position = "none") 
}



####=====================  3. Main code  ======================####
# Create empty dataframe to collect estimates 
result_df <- tibble()

# Read mapping files
hsb_mapping <- read_csv2(hsb_path)
rx_mapping  <- read_csv2(rx_path)

# Calculate metrics for each survey of the country
for (country in countries){
  print(country)
  for (dhs_file in dhs_files(country, year_range)){
    print(dhs_file)
    
    
    dhs_df <-
      file.path(dhs_dir, dhs_file) %>%
      read_dta() %>%
      recode_dhs(country, dhs_file) %>%
      dplyr::filter(alive == 1)

    new_rows <- survey_metrics(dhs_df)

    result_df <- result_df %>% bind_rows(new_rows)
  }
}

if (result_df %>% is_empty()) {
  stop("No DHS surveys found for given countries and year_range.")
}

# impute missings and calculate scaling factor
result_df <- result_df %>% 
  impute_missings() %>%
  mutate(
    scaling = case_when(access == 0 ~ 1,
                        TRUE        ~ eff_cov / access
              ) 
  )

# export to csv
output_path <- file.path(output_dir, "dhs_effcov_hbhi_allSrvy_v4_2003.csv")
#' Note the year of the survey is attached to each output to allow for easier identification
#' of each years' output
#' 
#' 
result_df %>%
  dplyr::select(
    "country", "survey", "year", "obs", "admin_level", 
    "admin1", "admin1_num", "urban", "fever", "access", 
    "formal", "hcnobeds", "hcbeds", "opd", "chw", 
    "test", "fline", "othrantimal", "compl_f", "compl_i", 
    "eff_cov", "scaling", "imputed"
  ) %>%
  write.csv(output_path)

#' We plot a waterfall chart to visualize the case management effective cascade output
plot_waterfall(result_df$country, result_df$year)
