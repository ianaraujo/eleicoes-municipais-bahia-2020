
library(geobr)
library(tidyverse)
library(electionsBR)

load(file = "data/dados_bahia.RData")

bahiaMun <- read_municipality(code_muni = "BA", year = 2019)

# ...

baPartidos <- bahiaMun %>%
  left_join(partidos, by = c('code_muni' = 'codibge')) %>%
  filter(apuracao == 100) %>%
  select(-turno, -apuracao, -fase, -compilado)

saveRDS(baPartidos, file = "data/baPartidos.rds")
