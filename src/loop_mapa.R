
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

# ...

baVer %>%
  filter(territorio == "Metropolitano de Salvador") %>%
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
  filter(territorio == "Metropolitano de Salvador") %>%
  group_by(partido, name_muni) %>%
  summarise(n = sum(votabs)) %>%
  group_by(name_muni) %>%
  slice_max(n) %>%
  mutate(partido2 = ifelse(partido %in% index_partido, partido, "Outros")) %>%
  mutate(partido2 = factor(partido2, levels = c("DEM", "MDB", "PL", "PSDB", "Outros"))) %>%
  ggplot(aes(fill = partido2)) +
  geom_sf(color = "grey25", size = .2) +
  scale_fill_manual(values = c("#4c5c5b", "#437a63", "#62944d", "#a5a523", "#ffa600", "white")) +
  labs(fill = "", title = "Mapa eleitoral geral de votos para vereador",
       subtitle = "Resultado das eleições municipais 2020", caption = "Fonte: TSE") +
  theme_void() +
  theme(plot.title = element_text(face = "bold"))

for (i in unique(baVer$territorio)) {
    baVer %>%
      filter(territorio == i) %>%
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
      filter(territorio == i) %>%
      group_by(partido, name_muni) %>%
      summarise(n = sum(votabs)) %>%
      group_by(name_muni) %>%
      slice_max(n) %>%
      mutate(partido2 = ifelse(partido %in% index_partido, partido, "Outros")) %>%
      mutate(partido2 = factor(partido2, levels = c(index_partido, "Outros"))) %>%
      ggplot(aes(fill = partido2)) +
      geom_sf(color = "grey25", size = .2) +
      scale_fill_manual(values = c("#377EB8", "#4DAF4A", "#E41A1C", "#984EA3", "#FF7F00", "white")) +
      labs(fill = "", title = paste0("Mapa eleitoral geral de votos para vereador", " (", i, ")"),
           subtitle = "Eleições Municipais por território de identidade 2020", caption = "Fonte: TSE") +
      theme_void() +
      theme(plot.title = element_text(face = "bold")) -> gg
    print(gg)
    ggsave(filename = paste0("viz/mapa/", tolower(i), ".png"))
}
