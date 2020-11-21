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
  dict <- list('SC'=42, 'PR'=41, 'RS'=43)
  est <- paste(state, collapse = ', ')
  print(paste0('Analysis on ', est))
  # Pop & Edu
  # "https://sidra.ibge.gov.br/Tabela/1554
  # https://sidra.ibge.gov.br/geratabela?format=us.csv&name=tabela1554.csv&terr=NC&rank=-&query=t/1554/n6/all/v/allxp/p/all/c1568/0,99713/d/v140%200/l/v,p%2Bc1568,t
  # https://sidra.ibge.gov.br/geratabela?format=us.csv&name=tabela1554.csv&terr=NC&rank=-&query=t/1554/n6/all/v/1000140/p/all/c1568/0,99713/d/v1000140%202/l/v,p%2Bc1568,t"
  suppressMessages(
    df <- readr::read_csv("data/clean/tabela1554.csv") %>% 
      janitor::clean_names() %>%
      dplyr::mutate(
        uf = substr(cod, 1, 2),
        cd_mun = as.character(cod)
      ) %>%
      dplyr::rename('pop_sup_comp'='superior_completo') %>% 
      dplyr::filter(uf%in%c('42')) %>% 
      dplyr::group_by(cd_mun) %>% 
      dplyr::summarise(
        pop_sup_comp = pop_sup_comp,
        municipio = municipio
      ) %>% 
      dplyr::select(cd_mun, municipio, pop_sup_comp)
  )
  
  # Exportações
  # http://www.mdic.gov.br/index.php/comercio-exterior/estatisticas-de-comercio-exterior/base-de-dados-do-comercio-exterior-brasileiro-arquivos-para-download
  suppressMessages(
    exp_comex <- vroom::vroom(file = "data/clean/EXP_COMPLETA_MUN.csv") %>% 
      janitor::clean_names() %>% 
      dplyr::filter(co_ano>=2010) %>%
      dplyr::filter(sg_uf_mun%in%c('SC')) %>%
      dplyr::mutate(exp_fob=if_else(is.na(vl_fob), 0, vl_fob)) %>% 
      # dplyr::mutate("sh2" = substr(sh4, 1, 2)) %>%
      dplyr::group_by(co_mun) %>%
      dplyr::summarise(exp_fob = sum(exp_fob)) %>% 
      dplyr::mutate(cd_mun=as.character(co_mun)) %>% 
      dplyr::select(cd_mun, exp_fob)
  )
  
  # Joins -------------------------------------------------------------------
  data <- left_join(df, exp_comex, by=c("cd_mun")) %>% 
    na.omit() %>% 
    dplyr::mutate(
      log_exp = log(exp_fob),
      log_pop_sup_comp = log(pop_sup_comp)
    )
  
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
