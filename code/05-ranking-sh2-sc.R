library("dplyr")

suppressMessages(
  exp_comex <- vroom::vroom(file = "data/clean/EXP_COMPLETA_MUN.csv") %>% 
    janitor::clean_names() %>% 
    dplyr::filter(co_ano>=2010) %>%
    dplyr::filter(sg_uf_mun%in%c('SC')) %>%
    dplyr::mutate(exp_fob=if_else(is.na(vl_fob), 0, vl_fob)) %>% 
    dplyr::mutate("sh2" = substr(sh4, 1, 2)) %>%
    # dplyr::filter(sh2=='84') %>% 
    dplyr::group_by(sh2) %>%
    dplyr::summarise(exp_fob = sum(exp_fob)) #%>% 
    # dplyr::mutate(cd_mun=as.character(co_mun)) %>% 
    # dplyr::select(cd_mun, exp_fob)
)


exp_comex %>% arrange(desc(exp_fob)) %>% head(., 20)
