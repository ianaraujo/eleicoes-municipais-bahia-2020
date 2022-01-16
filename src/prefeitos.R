
library(geobr)
library(tidyverse)
library(electionsBR)

load(file = "data/dados_bahia.RData")

baTerritorio <- read_csv(file = "data/territorio_update.csv")

bahiaMun <- read_municipality(code_muni = "BA", year = 2019) %>%
  left_join(baTerritorio, by = c("name_muni" = "municipio"))

# ...

baPrefeitos <- bahiaMun %>%
  left_join(prefeitos, by = c('code_muni' = 'codibge')) %>%
  filter(apuracao == 100) %>%
  select(-apuracao, -fase, -turno, -compilado) %>%
  separate(partido, into = c("partido", "coligacao"), sep = " - ")

saveRDS(baPrefeitos, file = "data/baPrefeitos.rds")

# ...

# Cláudio André: zonas de identidade

baPrefeitos %>%
  mutate(situacao = case_when(
    nome %in% c("COLBERT MARTINS", "HERZEM GUSMÃO") ~ "Eleito",
    TRUE ~ situacao)) %>%
  filter(situacao == "Eleito") %>%
  group_by(partido) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  slice(1:4) %>%
  pull(partido) -> index_partido

baPrefeitos %>%
  mutate(situacao = case_when(
    nome %in% c("COLBERT MARTINS", "HERZEM GUSMÃO") ~ "Eleito",
    TRUE ~ situacao)) %>%
  filter(situacao == "Eleito") %>%
  mutate(partido2 = ifelse(partido %in% index_partido, partido, "Outros")) %>%
  mutate(partido2 = factor(partido2, levels = c("DEM", "PP", "PSD", "PT", "Outros"))) %>%
  ggplot(aes(fill = partido2)) +
  geom_sf(color = "grey25", size = .2) +
  scale_fill_manual(values = c("#29689D", "#45A2E6", "#C47934", "#955D28", "white")) +
  labs(fill = "", title = "Mapa eleitoral dos 4 partidos que mais elegeram prefeitos",
       subtitle = "Eleições Municipais da Bahia 2020", caption = "Fonte: TSE") +
  theme_void() +
  theme(plot.title = element_text(face = "bold"))

baPrefeitos %>%
  sf::st_drop_geometry() %>%
  group_by(partido, name_muni) %>%
  filter(situacao %in% c("Eleito", "2° turno")) %>%
  summarise(n = n()) %>%
  group_by(partido) %>%
  pivot_wider(names_from = partido, values_from = n) %>%
  mutate(PT = ifelse(is.na(PT), 0, PT)) %>%
  mutate(DEM = ifelse(is.na(DEM), 0, DEM)) %>%
  mutate(party_win = ifelse(DEM > PT, "DEM", "PT")) %>%
  right_join(bahiaMun) %>%
  ggplot(aes(fill = party_win)) +
  geom_sf(aes(geometry = geom), color = "grey25", size = .15) +
  scale_fill_manual(values = c("#3A7EB7", "#B73A3A", "grey")) +
  labs(fill = "", title = "Mapa eleitoral de votos para vereador (DEM x PT)",
       subtitle = "Resultado das eleições municipais 2020", caption = "Fonte: TSE") +
  theme_void() +
  theme(plot.title = element_text(face = "bold"))

# ...

## Número de vereados eleitos por partido ----

prefeitos %>%
  filter(apuracao == 100) %>%
  select(-apuracao, -fase, -turno, -compilado) %>%
  separate(partido, into = c("partido", "coligacao"), sep = " - ") %>%
  filter(situacao == "Eleito") %>%
  group_by(partido) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = reorder(partido, n), y = n)) +
  geom_col(fill = "#3A52B7") +
  geom_text(aes(label = signif(n)), nudge_y = 2.5, size = 3.5) +
  labs(x = "", y = "Número de prefeitos eleitos",
       title = "Número de prefeitos eleitos por partido",
       subtitle = "Eleições Municipais da Bahia 2020",
       caption = "Fonte: TSE") +
  scale_y_continuous(limits = c(0, 120)) +
  theme_classic() +
  theme(plot.title = element_text(face = "bold")) +
  coord_flip()


