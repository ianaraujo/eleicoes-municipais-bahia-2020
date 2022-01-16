# Cruzamentos de vereadores eleitos por raça e gênero (Destaque: Recôncavo e Região Metropolitana)

result2020 <- read.csv2(file = "data/votacao_candidato_munzona_2020_BA.csv", encoding = "latin1")

cand2020 <- electionsBR::candidate_local(year = 2020, uf = "BA")

cand2020 <- cand2020[, c(1:62)]

# GÊNERO

ele2020 <- cand2020 %>%
  left_join(result2020, by = c("SEQUENCIAL_CANDIDATO" = "SQ_CANDIDATO"))

ele2020 %>%
  filter(DESCRICAO_CARGO == "VEREADOR") %>%
  filter(!is.na(DS_SIT_TOT_TURNO)) %>%
  mutate(DS_SIT_TOT_TURNO = fct_recode(DS_SIT_TOT_TURNO,
                                       "ELEITO" = "ELEITO POR MÉDIA",
                                       "ELEITO" = "ELEITO POR QP",
                                       "NÃO ELEITO" = "SUPLENTE")) %>%
  group_by(DS_SIT_TOT_TURNO, DESCRICAO_SEXO) %>%
  count() %>%
  group_by(DS_SIT_TOT_TURNO) %>%
  mutate(percent = n/sum(n)) %>%
  ggplot(aes(x = DS_SIT_TOT_TURNO, y = percent, fill = DESCRICAO_SEXO)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = scales::percent(percent)),
            size = 4, color = "white", position = position_stack(vjust = 0.5)) +
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, .2)) +
  scale_fill_manual(values = c("#C8903C", "#3C74C8")) +
  labs(x = "", y = "", fill = "Descrição Sexo",
       title = "Proporção de candidatos a vereador eleitos e não eleitos por gênero",
       subtitle = "Eleições Municipais da Bahia 2020",
       caption = "Fonte: TSE") +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        plot.title = element_text(face = "bold"))

# GÊNERO E TERRITÓRIO

baTerritorio <- read_csv(file = "data/territorio_update.csv")

baTerritorio$municipio <- toupper(baTerritorio$municipio)
baTerritorio$territorio <- toupper(baTerritorio$territorio)

ele2020 %>%
  left_join(baTerritorio, by = c("DESCRICAO_UE" = "municipio")) %>%
  filter(territorio %in% c("RECÔNCAVO", "METROPOLITANO DE SALVADOR")) %>%
  filter(DESCRICAO_CARGO == "VEREADOR") %>%
  filter(!is.na(DS_SIT_TOT_TURNO)) %>%
  mutate(DS_SIT_TOT_TURNO = fct_recode(DS_SIT_TOT_TURNO,
                                       "ELEITO" = "ELEITO POR MÉDIA",
                                       "ELEITO" = "ELEITO POR QP",
                                       "NÃO ELEITO" = "SUPLENTE")) %>%
  group_by(DS_SIT_TOT_TURNO, DESCRICAO_SEXO, territorio) %>%
  count() %>%
  group_by(DS_SIT_TOT_TURNO, territorio) %>%
  mutate(percent = n/sum(n)) %>%
  ggplot(aes(x = DS_SIT_TOT_TURNO, y = percent, fill = DESCRICAO_SEXO)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = scales::percent(percent)),
            size = 4, color = "white", position = position_stack(vjust = 0.5)) +
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, .2)) +
  scale_fill_manual(values = c("#C8903C", "#3C74C8")) +
  labs(x = "", y = "", fill = "Descrição Sexo",
       title = "Proporção de candidatos a vereador eleitos e não eleitos por gênero",
       subtitle = "Eleições Municipais da Bahia 2020",
       caption = "Fonte: TSE") +
  theme_bw() +
  facet_wrap(~territorio) +
  theme(panel.grid.major.x = element_blank(),
        plot.title = element_text(face = "bold"))

# RAÇA

ele2020 %>%
  filter(DESCRICAO_CARGO == "VEREADOR") %>%
  filter(!is.na(DS_SIT_TOT_TURNO)) %>%
  mutate(DS_SIT_TOT_TURNO = fct_recode(DS_SIT_TOT_TURNO,
                                       "ELEITO" = "ELEITO POR MÉDIA",
                                       "ELEITO" = "ELEITO POR QP",
                                       "NÃO ELEITO" = "SUPLENTE")) %>%
  mutate(DESCRICAO_COR_RACA = fct_recode(DESCRICAO_COR_RACA,
                                         "OUTROS" = "AMARELA",
                                         "OUTROS" = "INDÍGENA",
                                         "OUTROS" = "SEM INFORMAÇÃO")) %>%
  mutate(DESCRICAO_COR_RACA = fct_relevel(DESCRICAO_COR_RACA,
                                          levels = c("BRANCA", "PARDA", "PRETA", "OUTROS"))) %>%
  group_by(DS_SIT_TOT_TURNO, DESCRICAO_COR_RACA) %>%
  count() %>%
  group_by(DS_SIT_TOT_TURNO) %>%
  mutate(percent = n/sum(n)) %>%
  ggplot(aes(x = DS_SIT_TOT_TURNO, y = percent, fill = DESCRICAO_COR_RACA)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = ifelse(percent < .05, "", scales::percent(percent))),
            size = 4, color = "white", position = position_stack(vjust = 0.5)) +
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, .2)) +
  scale_fill_manual(values = c("#B849C1", "#498EC1", "#52C149", "#D48533")) +
  labs(x = "", y = "", fill = "Descrição Cor/Raça",
       title = "Proporção de candidatos a vereador eleitos e não eleitos por cor/raça",
       subtitle = "Eleições Municipais da Bahia 2020 (*Outros: Amarela, Indígena e Não Informada)",
       caption = "Fonte: TSE") +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        plot.title = element_text(face = "bold"))

# RAÇA E TERRITÓRIO

ele2020 %>%
  left_join(baTerritorio, by = c("DESCRICAO_UE" = "municipio")) %>%
  filter(territorio %in% c("RECÔNCAVO", "METROPOLITANO DE SALVADOR")) %>%
  filter(DESCRICAO_CARGO == "VEREADOR") %>%
  filter(!is.na(DS_SIT_TOT_TURNO)) %>%
  mutate(DS_SIT_TOT_TURNO = fct_recode(DS_SIT_TOT_TURNO,
                                       "ELEITO" = "ELEITO POR MÉDIA",
                                       "ELEITO" = "ELEITO POR QP",
                                       "NÃO ELEITO" = "SUPLENTE")) %>%
  mutate(DESCRICAO_COR_RACA = fct_recode(DESCRICAO_COR_RACA,
                                         "OUTROS" = "AMARELA",
                                         "OUTROS" = "INDÍGENA",
                                         "OUTROS" = "SEM INFORMAÇÃO")) %>%
  mutate(DESCRICAO_COR_RACA = fct_relevel(DESCRICAO_COR_RACA,
                                          levels = c("BRANCA", "PARDA", "PRETA", "OUTROS"))) %>%
  group_by(DS_SIT_TOT_TURNO, DESCRICAO_COR_RACA, territorio) %>%
  count() %>%
  group_by(DS_SIT_TOT_TURNO, territorio) %>%
  mutate(percent = n/sum(n)) %>%
  ggplot(aes(x = DS_SIT_TOT_TURNO, y = percent, fill = DESCRICAO_COR_RACA)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = ifelse(percent < .05, "", scales::percent(percent))),
            size = 4, color = "white", position = position_stack(vjust = 0.5)) +
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, .2)) +
  scale_fill_manual(values = c("#B849C1", "#498EC1", "#52C149", "#D48533")) +
  labs(x = "", y = "", fill = "Descrição Cor/Raça",
       title = "Proporção de candidatos a vereador eleitos e não eleitos por cor/raça",
       subtitle = "Eleições Municipais da Bahia 2020 (*Outros: Amarela, Indígena e Não Informada)",
       caption = "Fonte: TSE") +
  theme_bw() +
  facet_wrap(~territorio) +
  theme(panel.grid.major.x = element_blank(),
        plot.title = element_text(face = "bold"))

# Zona Identidade

ele2020 %>%
  left_join(baTerritorio, by = c("DESCRICAO_UE" = "municipio")) %>%
  filter(territorio %in% c("RECÔNCAVO")) %>%
  filter(DESCRICAO_CARGO == "VEREADOR") %>%
  filter(DESC_SIT_TOT_TURNO %in% c("ELEITO POR MÉDIA", "ELEITO POR QP")) %>%
  group_by(SIGLA_PARTIDO, territorio) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = reorder(SIGLA_PARTIDO, n), y = n)) +
  geom_col(fill = "#B7AB3A") +
  geom_text(aes(label = signif(n)), nudge_y = 1, size = 3.5) +
  labs(x = "", y = "Número de vereadores eleitos",
       title = "Número de vereadores eleitos por partido no Recôncavo Baiano",
       subtitle = "Eleições Municipais 2020",
       caption = "Fonte: TSE") +
  theme_classic() +
  theme(plot.title = element_text(face = "bold"),
        axis.title.x = element_text(vjust = -2)) +
  coord_flip()

ele2020 %>%
  left_join(baTerritorio, by = c("DESCRICAO_UE" = "municipio")) %>%
  filter(territorio %in% c("METROPOLITANO DE SALVADOR")) %>%
  filter(DESCRICAO_CARGO == "VEREADOR") %>%
  filter(DESC_SIT_TOT_TURNO %in% c("ELEITO POR MÉDIA", "ELEITO POR QP")) %>%
  group_by(SIGLA_PARTIDO, territorio) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = reorder(SIGLA_PARTIDO, n), y = n)) +
  geom_col(fill = "#4AB73A") +
  geom_text(aes(label = signif(n)), nudge_y = 4, size = 3.5) +
  labs(x = "", y = "Número de vereadores eleitos",
       title = "Número de vereadores eleitos por partido no Região Metropolitana de Salvador",
       subtitle = "Eleições Municipais 2020",
       caption = "Fonte: TSE") +
  theme_classic() +
  theme(plot.title = element_text(face = "bold"),
        axis.title.x = element_text(vjust = -2)) +
  coord_flip()


ele2020 %>%
  left_join(baTerritorio, by = c("DESCRICAO_UE" = "municipio")) %>%
  filter(territorio %in% c("RECÔNCAVO", "METROPOLITANO DE SALVADOR")) %>%
  filter(DESCRICAO_CARGO == "VEREADOR") %>%
  filter(DESC_SIT_TOT_TURNO %in% c("ELEITO POR MÉDIA", "ELEITO POR QP")) %>%
  #filter(SIGLA_PARTIDO %in% index_partido) %>%
  group_by(SIGLA_PARTIDO, territorio) %>%
  summarise(n = n()) %>%
  group_by(territorio) %>%
  mutate(n = n/sum(n)) %>%
  ggplot(aes(n, SIGLA_PARTIDO, color = territorio)) +
  geom_line(aes(group = SIGLA_PARTIDO), color = "grey70") +
  geom_point(size = 2) +
  labs(x = "Porcentagem de vereadores eleitos", y = "", color = "",
       title = "Porcentagem de vereadores eleitos por partido",
       subtitle = "Eleições Municipais no Recôncavo Baiano e Região Metropolitana de Salvador (2020)",
       caption = "Fonte: TSE") +
  scale_color_manual(values = c("#73BF32", "#3273BF", "#BF3273")) +
  scale_x_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(panel.grid.minor.x = element_blank(),
        plot.title = element_text(face = "bold"),
        axis.title.x = element_text(vjust = -2.5))
