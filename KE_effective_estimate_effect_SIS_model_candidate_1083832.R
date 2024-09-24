####-----------------------------------------------------------####
#'
#'title: "Dynamic Modelling of Malaria Transmission for Case Management Impact"
#'author: "Candidate_no_1083832"
#'date: "2024-04-06"
######---------------------------------------------------------####
#'
####-----------------------------------------------------------####
#'   This R code is adopted by the candidate to take outputs of the effective
#'   coverage metrics and model what effect improving the effective coverage estimate 
#'   has on observed malaria incidence and prevalence   
#'   
####-----------------------------------------------------------####
####-----------------------------------------------------------####

library(devtools)  
install_github("SwissTPH/VivaxModelR") # Installs the VivaxModelR model package 

library(VivaxModelR)
library(ggplot2)

#' Defining calculated model incidence function in order to use it to calculate 
#' an appropriate rho value based on reported prevalence and incidence. This leads
#' deriving an appropriate proportion of the number of cases accurately reported by the system

get_incidence_from_PR=function(PR, r=r, rho, alpha){
  return(PR*r*rho/(1-alpha))
}
#'
#' We use observed prevalence incidence and estimates derived from the Kenya 2020
#' Malaria Indicator Survey and the electronic Kenya Health Information System platform
#' respectively
#'
#'National Values 
kenya_reported_prevalence_2020= 0.06
kenya_reported_incedence_2023 = 104.41
#'
#'
#' Lake Endemic Region Values
lake_endemic_reported_prevalence_2020 = 0.19
lake_endemic_reported_incedence_2023 = 430.13


#' Calculating incidence estimates using reported prevalence rates to estimate assumed number of cases
#' accurately captured by the system: We vary the reporting rate rho between 0 and 1 to accurately reflect the reported 
#' incidence rate using the reported prevalence rates - in our cases we estimate a rho of 0.441 for reported prevalence of 0.06 nationally
#' and a Rho of 0.551 for the lake endemic zone for a reported prevalence of 0.19 for the year 2023

my_incidence_national = get_incidence_from_PR(PR = kenya_reported_prevalence_2020, r=1/200, rho=0.441, alpha=0.538)
my_incidence_year_national = incidence_day2year(my_incidence)

my_incidence_lake_endemic = get_incidence_from_PR(PR = lake_endemic_reported_prevalence_2020, r=1/200, rho=0.511, alpha=0.588)
my_incidence_year_lake_endemic = incidence_day2year(my_incidence)


#' Modelling Case Management effect size on incidence and prevalence using the VivaxModelR Package

#### National Level #####
mydata=data.frame(id=c("Kenya"),incidence=104.41, prop_import = c(0))

mydata$h = incidence_year2day(mydata$incidence)

mydata$alpha= 0.538 # proportion of treated cases derived from our effective cascade model
mydata$beta= 0 # proportion of radical cure
mydata$rho= 0.441 # reporting rate (here, we assume that all treated cases are reported)
mydata$omega= 1  # Perfect vector control


mydata_withR0RC=calibrate_vivax_equilibrium(df=mydata, f=0, gamma=1, r=1/200, return.all = TRUE)

mydata_withR0RC

intervention_object0=list(intervention_name="baseline", "alpha.new"=NA, "beta.new"=NA, "omega.new"=NA, "rho.new"=NA )
intervention_objectA=list(intervention_name="effect treat 80%", "alpha.new"= 0.80, "beta.new"=NA, "omega.new"=NA, "rho.new"=NA )
intervention_objectC=list(intervention_name="effect treat 95%", "alpha.new"=0.95, "beta.new"=NA, "omega.new"=NA, "rho.new"=NA  )

my_intervention_list=list(intervention_object0, intervention_objectA, intervention_objectC)
simulation_model= simulate_vivax_interventions(df=mydata_withR0RC, my_intervention_list, f=0, gamma=1, r=1/200, delay = F)

# prevalence plot over 5 years
my_simu_prevalence_plot = ggplot(simulation_model)+
  geom_line(aes(x=time/365,y=I, color=intervention), lwd=1)+
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Case Management Effect on Prevalence, Kenya",
    x = "Years Since Baseline",  
    y = "Prevalence",
    caption = "Source : VivaxModelR", size = 0.5
  ) +
  theme_minimal()

my_simu_prevalence_plot


#Incidence Plot over 5 years
my_simu_incidence_plot = ggplot(simulation_model)+
  geom_line(aes(x=time/365,y=incidence, color=intervention), lwd=1)+
  ylim(0,NA) +
  labs(
    title = "Case Management Effect on Incidence, Kenya",
    x = "Years Since Baseline",  
    y = "Incidence Per 1000 Population",
    caption = "Source : VivaxModelR", size = 0.5
  ) +
  theme_minimal()

my_simu_incidence_plot
##########################

###############
#Lake Endemic Zone Level#
mydata_lake_endemic=data.frame(id=c("Lake Endemic Zone Kenya"),incidence_lake_endemic= 430.13, prop_import = c(0))

mydata_lake_endemic$h = incidence_year2day(mydata_lake_endemic$incidence_lake_endemic)

mydata_lake_endemic$alpha= 0.588 # proportion of treated cases derived from our effective cascade model
mydata_lake_endemic$beta= 0 # proportion of radical cure
mydata_lake_endemic$rho= 0.551 # reporting rate (here, we assume that all treated cases are reported)
mydata_lake_endemic$omega= 1  # Perfect vector control


mydata_lake_endemic_withR0RC=calibrate_vivax_equilibrium(df=mydata_lake_endemic, f=0, gamma=1, r=1/200, return.all = TRUE)

mydata_lake_endemic_withR0RC

intervention_object0=list(intervention_name="baseline", "alpha.new"=NA, "beta.new"=NA, "omega.new"=NA, "rho.new"=NA )
intervention_objectA=list(intervention_name="effect treat 80%", "alpha.new"= 0.80, "beta.new"=NA, "omega.new"=NA, "rho.new"=NA )
intervention_objectC=list(intervention_name="effect treat 95%", "alpha.new"=0.95, "beta.new"=NA, "omega.new"=NA, "rho.new"=NA  )

my_intervention_list_lake_endemic=list(intervention_object0, intervention_objectA, intervention_objectC)
simulation_model_lake_endemeic= simulate_vivax_interventions(df=mydata_lake_endemic_withR0RC, my_intervention_list_lake_endemic, f=0, gamma=1, r=1/200, delay = F)

# prevalence plot over 5 years
lake_endemic_prevalence_plot = ggplot(simulation_model_lake_endemeic)+
  geom_line(aes(x=time/365,y=I, color=intervention), lwd=1)+
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Case Management Effect on Prevalence, Lake Endemic Zone Kenya",
    x = "Years Since Baseline",  
    y = "Prevalence",
    caption = "Source : VivaxModelR", size = 0.5
  ) +
  theme_minimal()

lake_endemic_prevalence_plot


#Incidence Plot over 5 years
lake_endemic_incidence_plot = ggplot(simulation_model_lake_endemeic)+
  geom_line(aes(x=time/365,y=incidence, color=intervention), lwd=1)+
  ylim(0,NA)+
  labs(
    title = "Case Management Effect on Incidence, Lake Endemic Zone Kenya",
    x = "Years Since Baseline",  
    y = "Incidence Per 1000 Population",
    caption = "Source : VivaxModelR", size = 0.5
  ) +
  theme_minimal()
lake_endemic_incidence_plot

