
library(rvest)
library(tidyverse)

url = 'http://www.seplan.ba.gov.br/modules/conteudo/conteudo.php?conteudo=17'

read_html(url) %>%
  html_node('table') %>%
  html_table() %>%
  slice(-1) -> territorio_id

colnames(territorio_id) <- c("territorio", "municipio")

territorio_id <- territorio_id %>%
  separate_rows(municipio, sep = ",")

territorio_id$municipio <- sub("[[:punct:]]+$", "", territorio_id$municipio)

territorio_id$municipio <- sub("^-", "", territorio_id$municipio)

territorio_id$municipio <- str_trim(territorio_id$municipio, side = "both")

# ...

bahiaMunid <- bahiaMun[, -8] %>%
  left_join(territorio_id, by = c('name_muni' = 'municipio'))

write_csv(bahiaMunid, file = "data/bahia_territorios.csv")

# ...

baTerritorio <- read_csv(file = "data/territorio_update.csv")

# Recôncavo
