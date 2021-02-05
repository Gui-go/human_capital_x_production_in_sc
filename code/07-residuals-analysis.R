# R-script 07-residualas-analysis.R

# Setup -------------------------------------------------------------------
rm(list = ls())
gc()
options(stringsAsFactors = F)
ggplot2::theme_set(ggplot2::theme_minimal())
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

cent_as_cols <- function(polygonx, names = c("centlat", "centlng")){
  centroids_plus <- do.call(rbind, st_centroid(polygonx$geometry)) %>% 
    tibble::as_tibble() %>% stats::setNames(c(names[1],names[2])) %>% dplyr::bind_cols(polygonx, .)
  return(centroids_plus)
}

normalize <- function(x) {
  x <- x[!is.na(x)]
  return ((x - min(x)) / (max(x) - min(x)))
}

# Data --------------------------------------------------------------------

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
    suppressMessages() %>% 
    janitor::clean_names() %>% 
    dplyr::filter(co_ano>=2010) %>%
    dplyr::filter(sg_uf_mun%in%c('SC')) %>%
    dplyr::mutate(exp_fob=if_else(is.na(vl_fob), 0, vl_fob)) %>% 
    # dplyr::mutate("sh2" = substr(sh4, 1, 2)) %>%
    # dplyr::filter(sh2=='69') %>% 
    dplyr::group_by(co_mun) %>%
    dplyr::summarise(exp_fob = sum(exp_fob)) %>% 
    dplyr::mutate(cd_mun=as.character(co_mun)) %>% 
    dplyr::select(cd_mun, exp_fob)
)

# Dados espaciais das divisões regionais em SC (IBGE-API-Serviços)
suppressMessages(
  loc <- readr::read_csv("data/localizacoes_ibge_sc.csv") %>% 
    dplyr::mutate(
      cd_mun = as.character(cd_mun),
      cd_micro = as.character(cd_micro)
    )
)

# .shp Shapefile dos municípios de SC (IBGE-Geociencias-divisao_do_territorio)
sc_shp <- sf::st_read("data/raw/sc_municipios/") %>%
  janitor::clean_names() %>% 
  st_set_crs(4326)
# plot(sc_shp['cd_mun'])


# Join --------------------------------------------------------------------

micro_shp <- dplyr::left_join(sc_shp, loc) %>% 
  na.omit() %>% 
  group_by(cd_micro) %>% 
  summarise() %>% 
  cent_as_cols(.) %>% 
  st_set_crs(4326)

data <- dplyr::left_join(exp_comex, df, by=c("cd_mun")) %>%
  dplyr::left_join(., loc, by = "cd_mun") %>% 
  # stats::na.omit(.) %>% 
  dplyr::group_by(cd_micro) %>% 
  dplyr::summarise(
    nm_micro = first(nm_micro),
    pop_sup_comp = sum(pop_sup_comp, na.rm = T),
    log_pop_sup_comp = log(sum(pop_sup_comp, na.rm = T)),
    exp = sum(exp_fob, na.rm = T),
    log_exp = log(sum(exp_fob, na.rm = T))
  ) %>% 
  dplyr::left_join(., micro_shp, by = "cd_micro") %>% 
  sf::st_as_sf()

plot(data$exp, data$pop_sup_comp)
plot(data$log_exp, data$log_pop_sup_comp)


# Model -------------------------------------------------------------------
reg1 <- lm(log_exp ~ log_pop_sup_comp, data = data)
summary(reg1)
jtools::summ(reg1, digits = 5)

data$reg_res <- reg1$residuals
data$reg_res_norm <- normalize(reg1$residuals)
hist(data$reg_res_norm, 30)

nb <- spdep::poly2nb(data, queen=TRUE)
lw <- nb2listw(nb, style="W", zero.policy=TRUE)

spdep::moran.test(data$reg_res,lw)
hist(data$reg_res, 30)
plot(data['reg_res'])
plot(data['reg_res_norm'])
shapiro.test(data$reg_res)


# Spatial Model -----------------------------------------------------------
# install.packages("spgwr")
# library(spgwr)

dd <- sp::SpatialPointsDataFrame(
  data=data.frame(
    log_exp = data$log_exp,
    log_pop_sup_comp = data$log_pop_sup_comp
  ), 
  coords=cbind(data$centlng, data$centlat)
)

GWRbandwidth <- spgwr::gwr.sel(log_exp ~ log_pop_sup_comp, data=dd, adapt=T) 

gwr.model <- spgwr::gwr(
  log_exp ~ log_pop_sup_comp, data = dd, 
  adapt=GWRbandwidth, hatmatrix=TRUE, se.fit=TRUE
) 

results <- as.data.frame(gwr.model$SDF)
results$cd_micro <- data$cd_micro

dd2 <- SpatialPointsDataFrame(
  data=results, 
  coords=cbind(data$centlng, data$centlat)
) %>% sf::st_as_sf(.) %>% st_set_crs(4326)

shp_ff <- dplyr::left_join(micro_shp, results, by = 'cd_micro')
# class(shp_ff)
plot(shp_ff['log_pop_sup_comp'])
# plot(shp_ff['pred'])

