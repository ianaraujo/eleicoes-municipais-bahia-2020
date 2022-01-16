
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

write_csv(cand_todos, file = "data/cand_todos.csv")

# ...

cand_todos <- read.csv("data/cand_todos.csv")

# TÉCNICO DE ENFERMAGEM E ASSEMELHADOS (EXCETO ENFERMEIRO)

# MÉDICO

# ENFERMEIRO

# AGENTE DE SAÚDE E SANITARISTA

cand_todos %>% mutate(DESCRICAO_OCUPACAO = fct_recode(DESCRICAO_OCUPACAO,
                                                      "MOTORISTA DE TRANSPORTE COLETIVO" = "MOTORISTA DE VEÍCULOS DE TRANSPORTE COLETIVO DE PASSAGEIROS",
                                                      "ESTUDANTE" = "ESTUDANTE, BOLSISTA, ESTAGIÁRIO E ASSEMELHADOS",
                                                      "PROFESSOR" = "PROFESSOR DE ENSINO FUNDAMENTAL",
                                                      "PROFESSOR" = "PROFESSOR DE ENSINO MÉDIO",
                                                      "APOSENTADO" = "APOSENTADO (EXCETO SERVIDOR PÚBLICO)")) %>%
  filter(DESCRICAO_OCUPACAO != "OUTROS") %>%
  group_by(DESCRICAO_OCUPACAO, ANO_ELEICAO) %>%
  count() %>%
  group_by(ANO_ELEICAO) %>%
  mutate(n = n/sum(n)) %>%
  slice_max(n, n = 14) %>%
  pull(DESCRICAO_OCUPACAO) -> index_ocup


cand_todos %>%
  mutate(DESCRICAO_OCUPACAO = fct_recode(DESCRICAO_OCUPACAO,
                                                     "MOTORISTA DE TRANSPORTE COLETIVO" = "MOTORISTA DE VEÍCULOS DE TRANSPORTE COLETIVO DE PASSAGEIROS",
                                                     "ESTUDANTE" = "ESTUDANTE, BOLSISTA, ESTAGIÁRIO E ASSEMELHADOS",
                                                     "PROFESSOR" = "PROFESSOR DE ENSINO FUNDAMENTAL",
                                                     "PROFESSOR" = "PROFESSOR DE ENSINO MÉDIO",
                                                     "APOSENTADO" = "APOSENTADO (EXCETO SERVIDOR PÚBLICO)")) %>%
  filter(DESCRICAO_OCUPACAO != "OUTROS" & DESCRICAO_CARGO == "VEREADOR") %>%
  group_by(DESCRICAO_OCUPACAO, ANO_ELEICAO) %>%
  count() %>%
  group_by(ANO_ELEICAO) %>%
  mutate(n = n/sum(n)) %>%
  filter(DESCRICAO_OCUPACAO %in% index_ocup) %>%
  ggplot(aes(x = fct_reorder(DESCRICAO_OCUPACAO, n, .desc = FALSE), y = n)) +
  geom_line(color = "grey75") +
  geom_point(aes(color = as.character(ANO_ELEICAO))) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(values = c("#73BF32", "#3273BF", "#BF3273")) +
  labs(x = "", y = "", color = "",
       title = "Porcentagem das profissões declaradas pelos candidatos",
       subtitle = "Eleições Municipais da Bahia (2012 - 2020)",
       caption = "Fonte: TSE") +
  theme_minimal() +
  theme(panel.grid.minor.x = element_blank(),
        plot.title = element_text(face = "bold")) +
  coord_flip()

# Linhas (SAÚDE)

# TÉCNICO DE ENFERMAGEM E ASSEMELHADOS (EXCETO ENFERMEIRO)

# MÉDICO

# ENFERMEIRO

# AGENTE DE SAÚDE E SANITARISTA

cand_todos %>%
  filter(DESCRICAO_CARGO == "VEREADOR") %>%
  group_by(DESCRICAO_OCUPACAO, ANO_ELEICAO) %>%
  count() %>%
  group_by(ANO_ELEICAO) %>%
  mutate(n = n/sum(n)) %>%
  filter(DESCRICAO_OCUPACAO %in% c("TÉCNICO DE ENFERMAGEM E ASSEMELHADOS (EXCETO ENFERMEIRO)",
                                   "MÉDICO", "ENFERMEIRO", "AGENTE DE SAÚDE E SANITARISTA")) %>%
  mutate(DESCRICAO_OCUPACAO = fct_recode(DESCRICAO_OCUPACAO,
                                         "TÉCNICO DE ENFERMAGEM" = "TÉCNICO DE ENFERMAGEM E ASSEMELHADOS (EXCETO ENFERMEIRO)")) %>%
  ggplot(aes(x = ANO_ELEICAO, y = n)) +
  geom_line() +
  geom_point(color = "red") +
  scale_y_continuous(limits = c(0, 0.025), labels = scales::percent_format(accuracy = .1L)) +
  scale_x_continuous(breaks = c(2012, 2016, 2020)) +
  facet_wrap(~DESCRICAO_OCUPACAO) +
  labs(x = "", y = "",
       title = "Comparativo da porcentagem das profissões da área da saúde declaradas pelos candidatos",
       subtitle = "Eleições Municipais da Bahia (2012 - 2020)",
       caption = "Fonte: TSE") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold"))


cand_todos %>%
  mutate(prof = case_when(
    str_detect(NOME_URNA_CANDIDATO, "^(DR|DRA|DOUTOR|DOUTORA)([:blank:]|[:punct:][:blank:])") ~ "DR./DOUTOR",
    TRUE ~ DESCRICAO_OCUPACAO)) %>%
  group_by(prof, ANO_ELEICAO) %>%
  count() %>%
  group_by(ANO_ELEICAO) %>%
  mutate(n = n/sum(n)) %>%
  filter(prof == "DR./DOUTOR") %>%
  ggplot(aes(x = ANO_ELEICAO, y = n)) +
  geom_line() +
  geom_point(color = "red") +
  scale_y_continuous(limits = c(0, 0.025), labels = scales::percent_format(accuracy = .1L)) +
  scale_x_continuous(breaks = c(2012, 2016, 2020)) +
  labs(x = "", y = "",
       title = "Porcentagem dos candidatos usando nomes de urna contendo \"Dr.\" ou \"Doutor\"",
       subtitle = "Eleições Municipais da Bahia (2012 - 2020)",
       caption = "Fonte: TSE") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold"))

