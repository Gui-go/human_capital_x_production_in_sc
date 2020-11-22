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

# rio::export(data, 'data/data_exp_edusup_sc.csv')
