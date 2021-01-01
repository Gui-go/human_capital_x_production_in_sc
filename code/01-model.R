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

# Model -------------------------------------------------------------------

model <- function(state=c('SC'), sh2='02'){
  # Function to build the outputs of the analysis
  
  # data <- readr::read_csv(file = 'data/data_exp_edusup_sc.csv')
  data <- readr::read_csv(file = paste0('data/data_exp_edusup_sc_sh2-', sh2, '.csv'))[which(data$exp_fob>0),]
  # data <- readr::read_csv(file = paste0('data/data_exp_edusup_sc_sh2-94.csv'))
  mc <- matrixCorrelationPlot(data[, c('exp_fob', 'log_exp', 'pop_sup_comp', 'log_pop_sup_comp')])
  data_popsup_reg <- lm(log_exp ~ log_pop_sup_comp, data = data)
  # hist(data_popsup_reg$residuals, nclass = 30)
  # shapiro.test(data_popsup_reg$residuals)
  data_popsup_regsum <- summary(data_popsup_reg)
  pmodel <- jtools::summ(data_popsup_reg, digits = 5)
  # confint(data_popsup_reg, level = .99)
  
  return(
    list(
      correlationMatrixPlot = mc,
      logModel = pmodel,
      summary = data_popsup_regsum,
      lm = data_popsup_reg
    )
  )
  
}

model(sh='00')$correlationMatrixPlot
model(sh='10')$correlationMatrixPlot
model(sh='84')$correlationMatrixPlot

model(sh='10')$logModel # sh2 = 10 = Cereais
model(sh='02')$logModel # sh2 = 02 = Carnes
model(sh='64')$logModel # sh2 = 64 = Texteis
model(sh='84')$logModel # sh2 = 84 = Maquinários


#---------------------------------------------
model(sh='02')$logModel # sh2 = 02 = Carnes
model(sh='10')$logModel # sh2 = 10 = Cereais
model(sh='84')$logModel # sh2 = 84 = Maquinários
model(sh='94')$logModel # sh2 = 94 = Móveis
model(sh='69')$logModel # sh2 = 69 = Ceramica
model(sh='00')$logModel # sh2 = 00 = total
model(sh='69')$correlationMatrixPlot # sh2 = 69 = Ceramica

data <- readr::read_csv(file = paste0('data/data_exp_edusup_sc_sh2-69.csv'))

anova(model(sh='00')$logModel, model(sh='94')$logModel)
anova(model(sh='00')$summary, model(sh='69')$summary)
anova(model(sh='00')$lm, model(sh='00')$lm)
