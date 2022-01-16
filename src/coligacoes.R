
baPrefeitos <- read_rds(file = "data/baPrefeitos.rds") %>%
  sf::st_drop_geometry()

baPrefeitos %>%
  select(name_muni, nome, partido, coligacao, situacao) %>%
  mutate(coligacao = case_when(
    is.na(coligacao) ~ "Partido isolado",
    TRUE ~ coligacao)) %>%
  separate_rows(coligacao, sep = " / ") %>%
  rename("apoio" = "coligacao") %>%
  filter(partido != apoio) %>%
  mutate(apoio = gsub("\\**", "", x = apoio)) %>%
  filter(partido %in% c("DEM", "PP", "PT", "PSD")) %>%
  filter(apoio %in% c("DEM", "PP", "PT", "PSD", "Partido isolado")) %>%
  mutate(apoio = factor(apoio, levels = c("DEM", "PP", "PSD", "PT", "Partido isolado"))) %>%
  group_by(partido, apoio) %>%
  count() %>%
  group_by(partido) %>%
  mutate(n = n/sum(n)) %>%
  ggplot(aes(x = partido, y = n, fill = apoio)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1L),
                     breaks = seq(0, .6, .1), limits = c(0, .6)) +
  scale_fill_manual(values = c("#4085E3", "#57ADCC", "#E39E40", "#C73D3D", "grey80")) +
  labs(x = "", y = "", fill = "Partidos coligados",
       title = "Composição das coligações das candidaturas à prefeitura de DEM, PP, PSD e PT",
       subtitle = "Eleições Municipais 2020 - Estado da Bahia",
       caption = "Fonte: TSE") +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        plot.title = element_text(face = "bold"))


