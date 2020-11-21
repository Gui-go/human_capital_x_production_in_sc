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


# Data --------------------------------------------------------------------
# "https://sidra.ibge.gov.br/Tabela/1554
# https://sidra.ibge.gov.br/geratabela?format=us.csv&name=tabela1554.csv&terr=NC&rank=-&query=t/1554/n6/all/v/allxp/p/all/c1568/0,99713/d/v140%200/l/v,p%2Bc1568,t
# https://sidra.ibge.gov.br/geratabela?format=us.csv&name=tabela1554.csv&terr=NC&rank=-&query=t/1554/n6/all/v/1000140/p/all/c1568/0,99713/d/v1000140%202/l/v,p%2Bc1568,t"

# Pop & Edu
df <- readr::read_csv("data/clean/tabela1554.csv") %>% 
  janitor::clean_names() %>%
  dplyr::mutate(
    uf = substr(cod, 1, 2),
    cd_mun = as.character(cod),
    sem_instrucao_e_fundamental_incompleto = if_else(is.na(as.numeric(sem_instrucao_e_fundamental_incompleto)), 0, as.numeric(sem_instrucao_e_fundamental_incompleto)),
    fundamental_completo_e_medio_incompleto = if_else(is.na(as.numeric(fundamental_completo_e_medio_incompleto)), 0, as.numeric(fundamental_completo_e_medio_incompleto)),
    medio_completo_e_superior_incompleto = if_else(is.na(as.numeric(medio_completo_e_superior_incompleto)), 0, as.numeric(medio_completo_e_superior_incompleto)),
    nao_determinado = if_else(is.na(as.numeric(nao_determinado)), 0, as.numeric(nao_determinado)),
    pop_sem_sup = sem_instrucao_e_fundamental_incompleto + fundamental_completo_e_medio_incompleto + medio_completo_e_superior_incompleto + nao_determinado
  ) %>%
  dplyr::rename('pop_sup_comp'='superior_completo', 'pop_total'='total') %>% 
  dplyr::filter(uf%in%c('41', '42', '43')) %>% 
  dplyr::group_by(cd_mun) %>% 
  dplyr::summarise(
    pop_sem_sup = sum(sem_instrucao_e_fundamental_incompleto, fundamental_completo_e_medio_incompleto, medio_completo_e_superior_incompleto, nao_determinado, na.rm = T),
    pop_sup_comp = pop_sup_comp,
    pop_total = pop_total,
    municipio = municipio
  ) %>% 
  dplyr::select(cd_mun, municipio, pop_total, pop_sup_comp, pop_sem_sup)

# Exportações
# http://www.mdic.gov.br/index.php/comercio-exterior/estatisticas-de-comercio-exterior/base-de-dados-do-comercio-exterior-brasileiro-arquivos-para-download

exp_comex <- vroom::vroom(file = "data/clean/EXP_COMPLETA_MUN.csv") %>% 
  janitor::clean_names() %>% 
  dplyr::filter(co_ano>=2010) %>%
  dplyr::filter(sg_uf_mun%in%c("SC", 'RS', 'PR')) %>%
  dplyr::mutate(sum_vl_fob=if_else(is.na(vl_fob), 0, vl_fob)) %>% 
  # dplyr::mutate("sh2" = substr(sh4, 1, 2)) %>%
  dplyr::group_by(co_mun) %>%
  dplyr::summarise(sum_vl_fob = sum(vl_fob)) %>% 
  dplyr::mutate(cd_mun=as.character(co_mun)) %>% 
  dplyr::select(cd_mun, sum_vl_fob) 


# Joins -------------------------------------------------------------------

data <- left_join(df, exp_comex, by=c("cd_mun")) %>% 
  na.omit()

data %>% arrange(desc(sum_vl_fob))
data %>% arrange(desc(pop_total))
data %>% arrange(desc(pop_sem_sup))
data %>% arrange(desc(pop_sup_comp))
# rm()
data$logexp <- log(data$sum_vl_fob)
data$logpop_sup_comp <- log(data$pop_sup_comp)
data$logpop_sem_sup <- log(data$pop_sem_sup)

matrixCorrelationPlot(data[, c('logexp', 'logpop_sup_comp', 'logpop_sem_sup')])

(data_poptotal_reg <- lm(log(sum_vl_fob) ~ log(pop_sem_sup), data = data))
hist(data_poptotal_reg$residuals, nclass = 30)
shapiro.test(data_poptotal_reg$residuals)
(data_poptotal_regsum <- summary(data_poptotal_reg))
confint(data_poptotal_reg, level = .9)

(data_popsup_reg <- lm(log(sum_vl_fob) ~ log(pop_sup_comp), data = data))
hist(data_popsup_reg$residuals, nclass = 30)
(data_popsup_regsum <- summary(data_popsup_reg))
confint(data_popsup_regsum, level = .98)
as.numeric(data_popsup_regsum$coefficients[2, 4])

(data_reg <- lm(log(sum_vl_fob) ~ log(pop_total) + log(pop_sup_comp), data = data))
hist(data_poptotal_reg$residuals, nclass = 30)
(data_reg <- summary(data_reg))
confint(data_poptotal_reg, level = .9)

lme
library("GGally")
ggcoef(tail(broom::tidy(data_poptotal_reg, conf.int = TRUE), 51), sort = "ascending")
dwplot(data_reg)
library(dotwhisker)
m2 <- update(m1, . ~ . + hp) # add another predictor
m3 <- update(m2, . ~ . + am) # and another 

dwplot(list(data_poptotal_reg, data_popsup_reg), conf.level = .8, show_intercept = TRUE)

anova(data_poptotal_reg, data_popsup_reg)
anova(data_poptotal_reg, data_popsup_reg,test="F")


# Dá pra fazer uma análise dos discrepantes
plot(sum_vl_fob ~ pop_sup_comp, data)
cor(data$sum_vl_fob, data$pop_sup_comp)
cor(data$sum_vl_fob, data$pop_total)

hist(log(data$pop_sup_comp))


# install.packages("dotwhisker")

t.val <- qt(0.975, n - 2) # Critical value of t
b1.conf.upper <- b1 + t.val * sqrt(mse) / sqrt(sum((x - mean(x))^2))
b1.conf.lower <- b1 - t.val * sqrt(mse) / sqrt(sum((x - mean(x))^2))


# R Script - vis_correlation_matrix.R
# Matrix de correlação

# Setup
if(!require("GGally")){install.packages("GGally")}
if(!require("ggfortify")){install.packages("ggfortify")}
if(!require("ggplot2")){install.packages("ggplot2")}

# Example
matrixCorrelationPlot <- function(df){
  GGally::ggpairs(data = df, upper = list(continuous = GGally::wrap("cor", size = 6)))+
    ggplot2::labs(title = "Matriz de correlação")+
    ggplot2::theme_gray()+
    ggplot2::theme(plot.title = ggplot2::element_text(size = 13, face = "bold", hjust = 0.5),
                   strip.text = ggplot2::element_text(size = 10, face = "bold"))
}

matrixCorrelationPlot(mtcars)
