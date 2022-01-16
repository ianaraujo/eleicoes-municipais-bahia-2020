
library(geobr)
library(tidyverse)
library(electionsBR)

# ...

prefeitos2020 <- read_csv2(file = "data/prefeitos2020.csv")
vereadores2020 <- read_csv2(file = "data/vereadores2020.csv")
partidos2020 <- read_csv2(file = "data/partidos2020.csv")

# ...

bancos <- map(list(prefeitos2020, vereadores2020, partidos2020), ~ filter(.x, uf == "BA"))

prefeitos = bancos[[1]]
vereadores = bancos[[2]]
partidos = bancos[[3]]

save(prefeitos, vereadores, partidos, file = "data/dados_bahia.RData")

# ...

load(file = "data/dados_bahia.RData")
