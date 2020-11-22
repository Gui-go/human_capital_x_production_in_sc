# R-script 00-rascunho.R

# Setup -------------------------------------------------------------------
rm(list = ls())
gc()
options(stringsAsFactors = F)
theme_set(ggplot2::theme_minimal())
options(scipen = 666)

# Packages ----------------------------------------------------------------

if(!require(readr)){install.packages("readr")}
if(!require(plyr)){install.packages("plyr")}
if(!require(dplyr)){install.packages("dplyr")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(janitor)){install.packages("janitor")}
if(!require(sf)){install.packages("sf")}
if(!require(sp)){install.packages("sp")}
if(!require(st)){install.packages("st")}
if(!require(leaflet)){install.packages("leaflet")}
if(!require(mongolite)){install.packages("mongolite")}
if(!require(readxl)){install.packages("readxl")}
if(!require(janitor)){install.packages("janitor")}
if(!require(spdep)){install.packages("spdep")}
if(!require(vroom)){install.packages("vroom")}
if(!require(jtools)){install.packages("jtools")}

# Functions ---------------------------------------------------------------

source(file = "code/02-matrix_correlation_plot.R")




dict <- list('SC'=42, 'PR'=41, 'RS'=43)
dict[['SC']]

# Model -------------------------------------------------------------------

model <- function(state=c('SC')){
  # Function to build the outputs of the analysis
  
  data <- readr::read_csv(file = "data/data_exp_edusup_sc.csv")
  mc <- matrixCorrelationPlot(data[, c('exp_fob', 'log_exp', 'pop_sup_comp', 'log_pop_sup_comp')])
  data_popsup_reg <- lm(log_exp ~ log_pop_sup_comp, data = data)
  # hist(data_popsup_reg$residuals, nclass = 30)
  shapiro.test(data_popsup_reg$residuals)
  data_popsup_regsum <- summary(data_popsup_reg)
  pmodel <- jtools::summ(data_popsup_reg, digits = 5)
  # confint(data_popsup_reg, level = .99)
  
  return(
    list(
      correlationMatrixPlot = mc,
      logModel = pmodel
    )
  )
  
}

model()$correlationMatrixPlot
model()$logModel
