
load(file = "data/dados_bahia.RData")

baTerritorio <- read_csv(file = "data/territorio_update.csv")

bahiaMun <- read_municipality(code_muni = "BA", year = 2019) %>%
  left_join(baTerritorio, by = c("name_muni" = "municipio"))

# ...

baVereadores <- bahiaMun %>%
  left_join(vereadores, by = c('code_muni' = 'codibge')) %>%
  filter(apuracao == 100) %>%
  select(-turno, -apuracao, -fase, -compilado)

saveRDS(baVereadores, file = "data/baVereadores.rds")

# Carregar arquivo baVereadores.rds

baVer <- read_rds(file = "data/baVereadores.rds")

## Mapa eleitoral do voto para vereadores (DEM x PT)

baVer %>%
  sf::st_drop_geometry() %>%
  filter(partido %in% c("DEM", "PT")) %>%
  group_by(partido, name_muni) %>%
  summarise(n = sum(votabs)) %>%
  group_by(name_muni) %>%
  pivot_wider(names_from = partido, values_from = n) %>%
  mutate(PT = ifelse(is.na(PT), 0, PT)) %>%
  mutate(DEM = ifelse(is.na(DEM), 0, DEM)) %>%
  mutate(party_win = ifelse(DEM > PT, "DEM", "PT")) %>%
  right_join(bahiaMun) %>%
  ggplot(aes(fill = party_win)) +
  geom_sf(aes(geometry = geom), color = "grey25", size = .15) +
  scale_fill_manual(values = c("#3A7EB7", "#B73A3A", "grey")) +
  labs(fill = "", title = "Mapa eleitoral de votos para vereador (DEM x PT)",
       subtitle = "Eleições Municipais da Bahia 2020", caption = "Fonte: TSE") +
  theme_void() +
  theme(plot.title = element_text(face = "bold"))

## Mapa eleitoral geral de votos por partidos (vereador)

baVer %>%
  group_by(partido, name_muni) %>%
  summarise(n = sum(votabs)) %>%
  group_by(name_muni) %>%
  slice_max(n) %>%
  group_by(partido) %>%
  summarise(n  = n()) %>%
  arrange(desc(n)) %>%
  slice(1:4) %>%
  pull(partido) -> index_partido

baVer %>%
  group_by(partido, name_muni) %>%
  summarise(n = sum(votabs)) %>%
  group_by(name_muni) %>%
  slice_max(n) %>%
  mutate(partido2 = ifelse(partido %in% index_partido, partido, "Outros")) %>%
  mutate(partido2 = factor(partido2, levels = c("DEM", "PP", "PSD", "PSB", "Outros"))) %>%
  ggplot(aes(fill = partido2)) +
  geom_sf(color = "grey25", size = .2) +
  scale_fill_manual(values = c("#29689D", "#45A2E6", "#C47934", "#955D28", "white")) +
  labs(fill = "", title = "Mapa eleitoral geral de votos para vereador (4 partidos mais votados)",
       subtitle = "Eleições Municipais da Bahia 2020", caption = "Fonte: TSE") +
  theme_void() +
  theme(plot.title = element_text(face = "bold"))

# Recôncavo

baVer %>%
  filter(territorio == "Recôncavo") %>%
  group_by(partido, name_muni) %>%
  summarise(n = sum(votabs)) %>%
  group_by(name_muni) %>%
  slice_max(n) %>%
  group_by(partido) %>%
  summarise(n  = n()) %>%
  arrange(desc(n)) %>%
  slice(1:5) %>%
  pull(partido) -> index_partido

baVer %>%
  filter(territorio == "Recôncavo") %>%
  group_by(partido, name_muni) %>%
  summarise(n = sum(votabs)) %>%
  group_by(name_muni) %>%
  slice_max(n) %>%
  mutate(partido2 = ifelse(partido %in% index_partido, partido, "Outros")) %>%
  # mutate(partido2 = factor(partido2, levels = c("DEM", "PSC", "PSD", "PT", "Outros"))) %>%
  ggplot(aes(fill = partido2)) +
  geom_sf(color = "grey25", size = .2) +
  #scale_fill_manual(values = c("#29689D", "#309F56", "#C1852D", "#9F5430", "white")) +
  labs(fill = "", title = "Mapa eleitoral geral de votos para vereador",
       subtitle = "Resultado das eleições municipais 2020", caption = "Fonte: TSE") +
  theme_void() +
  theme(plot.title = element_text(face = "bold"))

# Metropolitano

baVer %>%
  filter(territorio == "Metropolitano de Salvador") %>%
  group_by(partido, name_muni) %>%
  summarise(n = sum(votabs)) %>%
  group_by(name_muni) %>%
  slice_max(n) %>%
  group_by(partido) %>%
  summarise(n  = n()) %>%
  arrange(desc(n)) %>%
  slice(1:2) %>%
  pull(partido) -> index_partido

baVer %>%
  filter(territorio == "Metropolitano de Salvador") %>%
  group_by(partido, name_muni) %>%
  summarise(n = sum(votabs)) %>%
  group_by(name_muni) %>%
  slice_max(n) %>%
  mutate(partido2 = ifelse(partido %in% index_partido, partido, "Outros")) %>%
  mutate(partido2 = factor(partido2, levels = c("DEM", "MDB", "PL", "PSDB", "Outros"))) %>%
  ggplot(aes(fill = partido2)) +
  geom_sf(color = "grey25", size = .2) +
  scale_fill_manual(values = c("#29689D", "#2892C8", "white")) +
  labs(fill = "", title = "Mapa eleitoral geral de votos para vereador",
       subtitle = "Resultado das eleições municipais 2020", caption = "Fonte: TSE") +
  theme_void() +
  theme(plot.title = element_text(face = "bold"))

# Tabelinha Partidos x Território de Identidade

# ???? Pensar em um gráfico ????

baVer %>%
  group_by(partido, territorio) %>%
  summarise(geom = sf::st_union(geom),
            n = sum(votabs)) %>%
  group_by(territorio) %>%
  slice_max(n) %>%
  select(partido, territorio) %>%
  sf::st_drop_geometry() -> partido_territorio



paletinha <- c("#4c5c5b", "#437a63", "#62944d", "#a5a523", "#ffa600")

bahiaMun %>%
  select(name_muni, territorio, geom) %>%
  group_by(territorio) %>%
  summarise(geom = sf::st_union(geom)) %>%
  left_join(partido_territorio) %>%
  mutate(partido = factor(partido, levels = c("PSD", "PP", "DEM", "PT", "PL"))) %>%
  ggplot(aes(fill = partido)) +
  geom_sf(color = "grey95", size = .1) +
  scale_fill_manual(values = paletinha) +
  labs(fill = "", title = "Mapa eleitoral geral de votos para vereador por território de identidade",
       subtitle = "Eleições Municipais de Bahia 2020", caption = "Fonte: TSE") +
  theme_void() +
  theme(plot.title = element_text(face = "bold"))
