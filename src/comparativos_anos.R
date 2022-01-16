
library(tidyverse)
library(electionsBR)

# Vereadores eleitos por partido (2012/2016/2020)

cand2020 <- electionsBR::candidate_local(year = 2020, uf = "BA")
cand2016 <- electionsBR::candidate_local(year = 2016, uf = "BA")
cand2012 <- electionsBR::candidate_local(year = 2012, uf = "BA")

cand2020 <- cand2020[, c(1:62)]
cand2016 <- cand2016[, c(1:62)]

cand2012 <- cand2012 %>%
  select(-EMAIL_CANDIDATO, -SIGLA_LEGENDA, -IDADE_DATA_ELEICAO)

cand2016 <- cand2016 %>%
  select(colnames(cand2012))

cand2020 <- cand2020 %>%
  select(colnames(cand2012))

cand_todos <- rbind(cand2020, cand2016, cand2012)

# Vereadores

cand_todos %>%
  filter(DESCRICAO_CARGO == "VEREADOR") %>%
  filter(DESC_SIT_TOT_TURNO %in% c("ELEITO POR MÉDIA", "ELEITO POR QP")) %>%
  mutate(SIGLA_PARTIDO = fct_recode(SIGLA_PARTIDO,
                                    "PMDB / MDB" = "PMDB",
                                    "PMDB / MDB" = "MDB",
                                    "PT do B / AVANTE" = "AVANTE",
                                    "PT do B / AVANTE" = "PT do B",
                                    "PTN / PODEMOS" = "PODE",
                                    "PTN / PODEMOS" = "PTN",
                                    "PPS / CIDADANIA" = "CIDADANIA",
                                    "PPS / CIDADANIA" = "PPS",
                                    "PRB / REPUBLICANOS" = "REPUBLICANOS",
                                    "PRB / REPUBLICANOS" = "PRB")) %>%
  group_by(SIGLA_PARTIDO, ANO_ELEICAO) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  filter(ANO_ELEICAO == 2020) %>%
  arrange(desc(n)) %>%
  slice(1:20) %>%
  pull(SIGLA_PARTIDO) -> index_partido

cand_todos %>%
  filter(DESCRICAO_CARGO == "VEREADOR") %>%
  filter(DESC_SIT_TOT_TURNO %in% c("ELEITO POR MÉDIA", "ELEITO POR QP")) %>%
  mutate(SIGLA_PARTIDO = fct_recode(SIGLA_PARTIDO,
                                    "PMDB / MDB" = "PMDB",
                                    "PMDB / MDB" = "MDB",
                                    "PT do B / AVANTE" = "AVANTE",
                                    "PT do B / AVANTE" = "PT do B",
                                    "PTN / PODEMOS" = "PODE",
                                    "PTN / PODEMOS" = "PTN",
                                    "PPS / CIDADANIA" = "CIDADANIA",
                                    "PPS / CIDADANIA" = "PPS",
                                    "PRB / REPUBLICANOS" = "REPUBLICANOS",
                                    "PRB / REPUBLICANOS" = "PRB")) %>%
  filter(SIGLA_PARTIDO %in% index_partido) %>%
  group_by(SIGLA_PARTIDO, ANO_ELEICAO) %>%
  summarise(n = n()) %>%
  ggplot(aes(n, fct_reorder2(SIGLA_PARTIDO, ANO_ELEICAO, n, .desc = FALSE), color = as.character(ANO_ELEICAO))) +
  geom_line(aes(group = SIGLA_PARTIDO), color = "grey70") +
  geom_point(size = 2) +
  labs(x = "Número de vereadores eleitos", y = "", color = "",
       title = "Número de vereadores eleitos por partido nas últimas três eleições",
       subtitle = "Considerando os 20 partidos que mais elegeram candidatos em 2020",
       caption = "Fonte: TSE") +
  scale_color_manual(values = c("#73BF32", "#3273BF", "#BF3273")) +
  theme_minimal() +
  theme(panel.grid.minor.x = element_blank(),
        plot.title = element_text(face = "bold"),
        axis.title.x = element_text(vjust = -2.5))

# Prefeitos eleitos por partido (2012/2016/2020)

cand_todos %>%
  filter(DESCRICAO_CARGO == "PREFEITO") %>%
  filter(DESC_SIT_TOT_TURNO %in% c("ELEITO")) %>%
  mutate(SIGLA_PARTIDO = fct_recode(SIGLA_PARTIDO,
                                    "PMDB / MDB" = "PMDB",
                                    "PMDB / MDB" = "MDB",
                                    "PT do B / AVANTE" = "AVANTE",
                                    "PT do B / AVANTE" = "PT do B",
                                    "PTN / PODEMOS" = "PODE",
                                    "PTN / PODEMOS" = "PTN",
                                    "PPS / CIDADANIA" = "CIDADANIA",
                                    "PPS / CIDADANIA" = "PPS",
                                    "PRB / REPUBLICANOS" = "REPUBLICANOS",
                                    "PRB / REPUBLICANOS" = "PRB")) %>%
  group_by(SIGLA_PARTIDO, ANO_ELEICAO) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  filter(ANO_ELEICAO == 2020) %>%
  arrange(desc(n)) %>%
  slice(1:15) %>%
  pull(SIGLA_PARTIDO) -> index_partido

cand_todos %>%
  filter(DESCRICAO_CARGO == "PREFEITO") %>%
  filter(DESC_SIT_TOT_TURNO %in% c("ELEITO")) %>%
  mutate(SIGLA_PARTIDO = fct_recode(SIGLA_PARTIDO,
                                    "PMDB / MDB" = "PMDB",
                                    "PMDB / MDB" = "MDB",
                                    "PT do B / AVANTE" = "AVANTE",
                                    "PT do B / AVANTE" = "PT do B",
                                    "PTN / PODEMOS" = "PODE",
                                    "PTN / PODEMOS" = "PTN",
                                    "PPS / CIDADANIA" = "CIDADANIA",
                                    "PPS / CIDADANIA" = "PPS",
                                    "PRB / REPUBLICANOS" = "REPUBLICANOS",
                                    "PRB / REPUBLICANOS" = "PRB")) %>%
  filter(SIGLA_PARTIDO %in% index_partido) %>%
  group_by(SIGLA_PARTIDO, ANO_ELEICAO) %>%
  summarise(n = n()) %>%
  mutate(n = ifelse(SIGLA_PARTIDO == "DEM" & ANO_ELEICAO == "2016", 37, n)) %>%
  ggplot(aes(n, fct_reorder2(SIGLA_PARTIDO, ANO_ELEICAO, n, .desc = FALSE), color = as.character(ANO_ELEICAO))) +
  geom_line(aes(group = SIGLA_PARTIDO), color = "grey70") +
  geom_point(size = 2) +
  labs(x = "Número de prefeitos eleitos", y = "", color = "",
       title = "Número de prefeitos eleitos por partido nas últimas três eleições",
       subtitle = "Considerando os 15 partidos que mais elegeram prefeitos em 2020",
       caption = "Fonte: TSE") +
  scale_color_manual(values = c("#73BF32", "#3273BF", "#BF3273")) +
  scale_x_continuous(limits = c(0, 120)) +
  theme_minimal() +
  theme(panel.grid.minor.x = element_blank(),
        plot.title = element_text(face = "bold"),
        axis.title.x = element_text(vjust = -2.5))

