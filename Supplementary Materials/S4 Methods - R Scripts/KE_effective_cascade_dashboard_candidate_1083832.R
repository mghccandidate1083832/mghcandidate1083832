####-----------------------------------------------------------####
#'
#'title: "Interactive visualization of model results dashboard"
#'author: "Candidate_no_1083832"
#'date: "2024-04-06"
######---------------------------------------------------------####
#'
####-----------------------------------------------------------####
#' A shiny app to provide a user an interactive and intuitive interface
#' to visualize malaria cases effective treated by the healthcare system using
#' the effective cascade model and the effect on observed incidence and prevalence
#' by improving the effective cascade estimate using the VivaxModelR package model  
#'   
####-----------------------------------------------------------####
####-----------------------------------------------------------####

# load libraries
library(shiny)
library(bslib)
library(deSolve)
library(tidyverse)
library(plotly)
library(reactable)
library(VivaxModelR)


# Define the ui ----
ui <- page_sidebar(
  title = "Kenya Case Management Cascade",
  # side bar ----
  sidebar = sidebar(
    title = "Effective Cascade Model Inputs", #' Slider inputs for the user to define effective cascade model data
    width = 300,
    
    strong("Malaria fevers Indicators"),
    
    sliderInput(
      inputId = "access_provider",
      label = "Proportion Malaria Fevers Accessing Provider",
      value = 0.69,
      min = 0,
      max = 1,
      step = 0.05
    ),
    sliderInput(
      inputId = "access_formal_provider",
      label = "Proportion Malaria Fevers Accessing Formal Provider",
      value = 0.82,
      min = 0,
      max = 1,
      step = 0.05
    ),
    sliderInput(
      inputId = "provider_compliance_act",
      label = "Proportion Malaria Fevers Treated with First Line Antimalarials",
      value = 0.83,
      min = 0,
      max = 1,
      step = 0.05
    ),
    sliderInput(
      inputId = "treatment_adherence_act",
      label = "Proportion Malaria Fevers Adhering to Drug Regimen",
      value = 0.91,
      min = 0,
      max = 1,
      step = 0.05
    ),
    
    strong("Malaria Medication Indicators"), #' Slider inputs for the user to define malaria treatment drug data
    
    sliderInput(
      inputId = "ACT_cure_rate",
      label = "ACT Efficacy Cure Rates",
      value = 0.97,
      min = 0,
      max = 1,
      step = 0.05
    ),
    sliderInput(
      inputId = "non_ACT_cure_rate",
      label = "Non-ACTs Efficacy Cure Rates",
      value = 0.53,
      min = 0,
      max = 1,
      step = 0.05
    ),
    
    strong("SIS model output"), #' Slider inputs for the user to define VivaxModelR model observed epidemiologic data
    
    sliderInput(inputId = "incidence", label = "Yearly reported incidence per 1000", value = 87.3, min = 0, max = 1000, step = 0.1),
    sliderInput(inputId = "alpha", label = "Proportion of Cases Effectively Treated by Healthcare System", value = 0.538, min = 0, max = 1, step = 0.1),
    sliderInput(inputId = "rho", label = "Proportion of Cases Reported and Captured by Healthcare System", value = 0.368, min = 0, max = 1, step = 0.1),
    
    actionButton(inputId = "run_model", label = "Run model"  ),
    actionButton(inputId = "run_SIR_model", label = "Run SIR Model")
    
  ),
  
  # main panel ----
  card(
    card_title("Model Results"),
    plotOutput("model_plot"),
    plotOutput("incidence_model_output")
    
  )
)


# define the server function ----
server <- function(input, output, session) {
  
  # reactive to store the model output ----
  model_output <- eventReactive(eventExpr = input$run_model, ignoreNULL = FALSE, {
    
    # Input parameters
    A = input$access_provider
    P = input$access_formal_provider
    D = input$provider_compliance_act
    H = input$treatment_adherence_act
    C = input$ACT_cure_rate
    N = input$non_ACT_cure_rate
    
    O= 1 # Non ACT antimalarial adherence in the formal sector
    
    D2= 0.14
    H2= H * 0.8
    C2 = C - 0.083
    O2 = 1 # Non ACT antimalarial adherence in the informal sector
    N2 = N - 0.33
    
    
    bars_df=data.frame("cases"=100) %>% 
      mutate(
        rem_access     = 100 *  A,
        rem_adherence = 100 *  A * (
          P
          * (D * H + (1 - D) * O)
          + (1 - P)
          * (D2 * H2 + (1 - D2) * O2)
        ),
        effective_care   = 100 * A * (
          P
          * (D * H * C + (1 - D) * O * N)
          + (1 - P)
          * (D2 * H2 * C2 + (1 - D2) *O2 * N2)
        )) %>%
      reshape2::melt(measure.vars =
                       c("cases",
                         "rem_access",
                         "rem_adherence",
                         "effective_care")
      ) %>%
      mutate(
        background_value = lag(value),
        percent          = 100 * value / background_value) %>%  
      mutate(value=ifelse(variable%in% c("effective_care"),NA,value))
    
    bars_background <- ggplot(bars_df) +
      geom_col(mapping = aes(variable ,
                             background_value,
                             fill = (variable %in% c("total","effective_care"))
      ),
      color = "black", position = "dodge"
      ) +
      scale_fill_manual(values = c("grey47", "gold")) +
      guides(fill = FALSE) +
      geom_text(data = subset(bars_df, variable %in% c("total","effective_care")),
                mapping = aes(
                  variable, background_value,
                  label = background_value %>% round(1)
                ),
                position = position_stack(vjust = 0.5),
      ) +
      scale_y_continuous(label=function(x) paste0(x, "%")) +
      labs(y="",
           x="") +
      scale_x_discrete(labels = c(("All
                                     Fevers"),
                                  
                                  ("Fevers
                                    Accessing
                                     provider"),
                                  
                                  ("Adhering to 
                                     treatment regimen"),
                                  
                                  ("Effectively 
                                     cured")))+
      
      theme_classic()     +
      geom_col(mapping = aes(variable, value),
               fill = "lightgrey",
               color= "black") +
      geom_text(data = subset(bars_df, grepl("rem_", variable)),
                mapping = aes(
                  variable, value,
                  label = percent %>% round(1) %>% paste0("%")
                ),
                position = position_stack(vjust = 0.5),
      )+
      update_geom_defaults("text", list(size = 10))+ 
      theme(legend.position = "none",
            axis.text.x=element_text(size=25))
    
  })
  
  SIR_model_output <- eventReactive(eventExpr = input$run_SIR_model, ignoreNULL = FALSE, {
    
    # #SIR model inputs parameters
    incidence = input$incidence
    alpha  = input$alpha
    rho = input$rho
    
    mydata <- data.frame(id = "Kenya", incidence = incidence, prop_import = 0)
    mydata$h <- VivaxModelR::incidence_year2day(mydata$incidence)
    mydata$alpha <- alpha
    mydata$beta <- 0
    mydata$rho <- rho
    mydata$omega <- 1
    
    mydata_withR0RC <- VivaxModelR::calibrate_vivax_equilibrium(df = mydata, f = 0, gamma = 1, r = 1/200, return.all = TRUE)
    
    intervention_object0 <- list(intervention_name = "baseline", alpha.new = NA, beta.new = NA, omega.new = NA, rho.new = NA)
    intervention_objectA <- list(intervention_name = "effect treat 80%", alpha.new = 0.80, beta.new = NA, omega.new = NA, rho.new = NA)
    intervention_objectC <- list(intervention_name = "effect treat 95%", alpha.new = 0.95, beta.new = NA, omega.new = NA, rho.new = NA)
    
    my_intervention_list <- list(intervention_object0, intervention_objectA, intervention_objectC)
    simulation_model <- VivaxModelR::simulate_vivax_interventions(df = mydata_withR0RC, my_intervention_list, f = 0, gamma = 1, r = 1/200, delay = FALSE)
    
    incidence_plot <- ggplot(simulation_model) +
      geom_line(aes(x = time / 365, y = incidence, color = intervention), lwd = 1) +
      facet_wrap(~ id, scales = "free_y") +
      xlab("Year since baseline") +
      ylim(0, NA)
    
    
  })
  
  
  # render the model output plot ----
  
  output$model_plot <- renderPlot({
    print( model_output())
    
  })
  # Render the SIR model output plot ----
  output$incidence_model_output <- renderPlot({
    print(SIR_model_output())
    
  })
}

shinyApp(ui = ui, server = server)
