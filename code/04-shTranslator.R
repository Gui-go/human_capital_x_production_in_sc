# R-script 01-comexstat-try.R

# Setup -------------------------------------------------------------------
# rm(list = ls())
gc()
options(stringsAsFactors = F)

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
if(!require(zoo)){install.packages("zoo")}

# Data --------------------------------------------------------------------

# Google: tabela sh4 >>> 2016 Antaq

tt <- read_excel("data/tabela_ncm.xls", skip = 4) %>% 
  janitor::clean_names() %>% 
  dplyr::rename("codsh2"="codigo_ncm_sh2", "descsh2"="descricao", "codsh4"="codigo_ncm_sh4", "descsh4"="descricao_1") %>% 
  dplyr::mutate(
    "codsh2"=zoo::na.locf(codsh2),
    "descsh2"=zoo::na.locf(descsh2)
  )

tsh2 <- tt %>% 
  dplyr::select(codsh2, descsh2) %>% 
  unique()

tsh4 <- tt %>% 
  dplyr::select(codsh4, descsh4)


