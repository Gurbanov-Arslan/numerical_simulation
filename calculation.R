library(tidyverse)

suma_fond <- 30000


clienti <- tibble(numar_clienti=c(0, 1, 2, 3), probabilitate=c(0.2, 0.2, 0.5, 0.1))
cerere <- tibble(suma_bani = c(1000, 2000, 3000), probabilitate = c(0.5, 0.3, 0.2))
nr_zile_solicitare <- tibble(nr_zile = c(2, 3, 4), probabilitate = c(0.3, 0.6, 0.1))

# clienti <- c("0"=0.2, "1"=0.2, "2"=0.5, "3"=0.1)
# cerere <- c("1000"=0.5, "2000"=0.3, "3000"=0.2)
# nr_zile_solicitare <- c("2"=0.3, "3"=0.6, "4"=0.1)
# # calcul prin interare 
# probabilitati_cumulate <- tibble(numar_clienti=c(), probabilitati_cumulate=c())
# probabilitate_cumulata = 0
# for (i in 1:nrow(clienti)) {
#   numar_clienti = clienti[i, 1]
#   probabilitate = clienti[i, 2]
#   probabilitate_cumulata = probabilitate_cumulata + probabilitate
#   probabilitati_cumulate = rbind(
#     probabilitati_cumulate, 
#     tibble(numar_clienti=c(numar_clienti), probabilitati_cumulate=c(probabilitate_cumulata)))
# }
# probabilitati_cumulate
# 
# 
# cerere %>%
#   mutate(prob_com = cumsum(prob_suma),
#          laguri= lag(prob_com),
#          prob_min = ifelse(is.na(laguri),0, laguri),
#          prob_max = prob_com) %>%
#   select (-c(prob_com, laguri))
# 
# nr_zile_solicitare %>%
#   mutate(prob_com_z = cumsum(probabilitate_z),
#          laguri = lag(prob_com_z),
#          prob_min = ifelse(is.na(laguri), 0, laguri),
#          prob_max = prob_com_z) %>%
#   select(Nr_zile, prob_min, prob_max)


calcul_interval_probabilitati <- function(df_probabiliati){
  intervale <- df_probabiliati %>%
    mutate(probabilitati_cumulate = cumsum(probabilitate),
           laguri = lag(probabilitati_cumulate),
           prob_min = ifelse(is.na(laguri), 0, laguri),
           prob_max = probabilitati_cumulate) %>%
    select(-c(probabilitati_cumulate, laguri))
  return(intervale)
}


calcul_numere <- function(valoare_aleatoare, df_probabilitati){
  intervale_probabilitati <- calcul_interval_probabilitati(df_probabilitati)
  numar_calculat <- intervale_probabilitati %>%
    filter(valoare_aleatoare >= prob_min) %>%
    filter(valoare_aleatoare < prob_max) %>%
    select(-c(prob_min, prob_max, probabilitate)) %>%
    pull()
  return(numar_calculat)
}



calcul_interval_probabilitati(nr_zile_solicitare)



random <- runif(1)
intervale_prob_clienti <- calcul_interval_probabilitati(clienti)
intervale_prob_clienti %>%
  filter(random >= prob_min) %>%
  filter(random < prob_max) %>%
  select(-c(prob_min, prob_max, probabilitate)) %>%
  pull()


calcul_numere <- function(valoare_aleatoare, df_probabilitati){
  intervale_probabilitati <- calcul_interval_probabilitati(df_probabilitati)
  numar_calculat <- intervale_probabilitati %>%
    filter(valoare_aleatoare >= prob_min) %>%
    filter(valoare_aleatoare < prob_max) %>%
    select(-c(prob_min, prob_max, probabilitate)) %>%
    pull()
  return(numar_calculat)
}

calcul_numere(runif(1), nr_zile_solicitare)


suma_ceruta = 2000
nr_zile_sol = 4
dobanda %>% 
  filter(suma == suma_ceruta) %>%
  filter(nr_zile == nr_zile_sol) %>%
  select(dobanda) %>%
  pull()


calcul_dobanda <- function(df_dobanda, suma_ceruta, nr_zile_solicitare){
  dobanda <- df_dobanda %>% 
    filter(suma == suma_ceruta) %>%
    filter(nr_zile == nr_zile_solicitare) %>%
    select(dobanda) %>%
    pull()
  return(dobanda/100)
}

calcul_dobanda(dobanda, 3000, 3)
