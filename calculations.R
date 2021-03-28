library(tidyverse)

suma_fond <- 30000


clienti <- tibble(numar_clienti=c(0, 1, 2, 3), probabilitate=c(0.2, 0.2, 0.5, 0.1))
cerere <- tibble(suma_bani=c(1000, 2000, 3000), probabilitate=c(0.5, 0.3, 0.2))
nr_zile_solicitare <- tibble(nr_zile=c(2, 3, 4), probabilitate=c(0.3, 0.6, 0.1))
dobanda <- tibble(
  suma=c(1000, 1000, 1000, 2000, 2000, 2000, 3000, 3000, 3000), 
  nr_zile=c(2, 3, 4, 2, 3, 4, 2, 3, 4),
  dobanda=c(0.1, 0.5, 1, 0.5, 1, 1.5, 1, 1.5, 2))


calcul_dobanda <- function(df_dobanda, suma_ceruta, nr_zile_solicitare){
  dobanda <- df_dobanda %>% 
    filter(suma == suma_ceruta) %>%
    filter(nr_zile == nr_zile_solicitare) %>%
    select(dobanda) %>%
    pull()
  return(dobanda/100)
}


calcul_interval_probabilitati <- function(df_probabiliati){
  intervale <- df_probabiliati %>%
    mutate(probabilitati_cumulate = cumsum(probabilitate),
           laguri = lag(probabilitati_cumulate),
           prob_min = ifelse(is.na(laguri), 0, laguri),
           prob_max = probabilitati_cumulate) %>%
    select(-c(probabilitati_cumulate, laguri))
  return(intervale)
}


calcul_numere <- function(valoare_aleatoare, df_intervale){
  numar_calculat <- df_intervale %>%
    filter(valoare_aleatoare >= prob_min) %>%
    filter(valoare_aleatoare < prob_max) %>%
    select(-c(prob_min, prob_max, probabilitate)) %>%
    pull()
  return(numar_calculat)
}


intervale_clienti <- calcul_interval_probabilitati(clienti)
intervale_cerere <- calcul_interval_probabilitati(cerere)
intervale_nr_zile <- calcul_interval_probabilitati(nr_zile_solicitare)


random_clienti <- runif(1)
numar_clienti <- calcul_numere(random_clienti, intervale_clienti)

valoarea_totala_dobanda = 0
if (numar_clienti > 0) {
  for (i in 1:numar_clienti) {
    random_cerere <- runif(1)
    valoare_cerere <- calcul_numere(random_cerere, intervale_cerere)
    random_nr_zile <- runif(1)
    nr_zile <- calcul_numere(random_nr_zile, intervale_nr_zile)
    procent_dobanda <- calcul_dobanda(dobanda, valoare_cerere, nr_zile)
    valoare_dobanda <- valoare_cerere*nr_zile*procent_dobanda
    valoarea_totala_dobanda = valoarea_totala_dobanda + valoare_dobanda
    print(valoare_dobanda)
    
  }
}





