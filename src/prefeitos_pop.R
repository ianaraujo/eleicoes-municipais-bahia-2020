
library(geobr)
library(tidyverse)
library(electionsBR)

load(file = "data/dados_bahia.RData")

municipios_ibge <- read_csv2(file = "data/municipio_social_curso.csv")

ibge <- read_csv(file = "data/ibge.csv")

colegios <- read_csv(file = "data/quantidade_de_eleitores.csv")

baPrefeitos <- read_rds(file = "data/baPrefeitos.rds") %>%
  sf::st_drop_geometry()

# ...

prefeitos_ibge <- municipios_ibge %>%
  filter(SiglaUF == "BA") %>%
  left_join(baPrefeitos, by = c("IBGE7" = "code_muni"))

prefeitos_ibge %>%
  mutate(pop_class = case_when(
    pop_2010 < 5000 ~ "Até 5 mil",
    pop_2010 >= 5000 & pop_2010 < 50000 ~ "5 a 50 mil",
    pop_2010 >= 50000 & pop_2010 < 100000 ~ "50 a 100 mil",
    pop_2010 >= 100000 & pop_2010 < 200000 ~ "100 a 200 mil",
    pop_2010 >= 200000 ~ "+ 200 mil")) %>%
  mutate(pop_class = factor(pop_class,
                            levels = c("Até 5 mil", "5 a 50 mil",
                                       "50 a 100 mil", "100 a 200 mil", "+ 200 mil"))) %>%
  group_by(partido, pop_class) %>%
  summarise(n = sum(votabs)) %>%
  group_by(pop_class) %>%
  mutate(n = n/sum(n)) %>%
  filter(partido %in% c("PP",  "PSD", "DEM", "PT")) %>%
  ggplot(aes(x = partido, y = n, fill = partido)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("#4085E3", "#57ADCC", "#E39E40", "#C73D3D")) +
  labs(x = "", y = "", fill = "",
      title = "Porcentagem de votos para prefeito por populaçaõ do município",
      subtitle = "Eleições Municipais da Bahia 2020 (DEM, PP, PSD e PT)",
      caption = "Fonte: TSE e CENSO 2010") +
  facet_wrap(~pop_class, ncol = 5) +
  theme_bw() +
  theme(legend.position = "top",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.title = element_text(face = "bold"))

# ...

prefeitos_ibge$votpor <- str_trim(prefeitos_ibge$votpor, side = "both")

prefeitos_ibge$votpor <- as.numeric(gsub(",", ".", gsub("\\.", "", prefeitos_ibge$votpor)))

prefeitos_ibge %>%
  mutate(rpc_2010 = rpc_2010/12) %>%
  filter(partido %in% c("DEM", "PP", "PL", "PSD", "MDB", "PT")) %>%
  ggplot(aes(x = as.numeric(idh_2010), y = votpor)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~partido) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1L, scale = 1)) +
  labs(x = "IDH 2010 (renda, educação e saúde)", y = "% de votos para prefeito",
       title = "Relação entre a porcentagem de votos para prefeito e IDH dos municípios",
       subtitle = "Eleições Municipais da Bahia 2020",
       caption = "Fonte: TSE e CENSO 2010") +
  theme_bw() +
  theme(plot.margin = unit(c(.5, 1, .5, 0.5), "cm"),
        axis.title.x = element_text(vjust = -3),
        axis.title.y = element_text(vjust = 2),
        plot.title = element_text(face = "bold"))

# TABELINDA

vote2018 <- vote_mun_zone_fed(year = 2018, uf = "BA")

ibge$municipio <- toupper(ibge$municipio)

colegios <- colegios %>%
  mutate(Quantidade = as.numeric(str_replace(Quantidade, "[:punct:]", ""))) %>%
  mutate(Quantidade = as.numeric(str_replace(Quantidade, "[:punct:]", ""))) %>%
  arrange(desc(Quantidade)) %>%
  slice(1:30)

baPrefeitos %>%
  mutate(situacao = case_when(
    nome %in% c("COLBERT MARTINS", "HERZEM GUSMÃO") ~ "Eleito",
    TRUE ~ situacao)) %>%
  mutate(votabs = case_when(
    nome == "COLBERT MARTINS" ~ 164831,
    nome == "HERZEM GUSMÃO" ~ 97364,
    TRUE ~ votabs)) %>%
  mutate(votpor = case_when(
    nome == "COLBERT MARTINS" ~ "54,42",
    nome == "HERZEM GUSMÃO" ~ "54,00",
    TRUE ~ votpor)) %>%
  filter(situacao == "Eleito") %>%
  select(name_muni, nome, partido, votabs, votpor) %>%
  mutate(name_muni = toupper(name_muni)) -> pref_eleitos

vote2018 %>%
  filter(DESCRICAO_CARGO == "Deputado Federal") %>%
  group_by(NOME_MUNICIPIO) %>%
  mutate(percent = round(TOTAL_VOTOS/sum(TOTAL_VOTOS), 4) * 100) %>%
  slice_max(TOTAL_VOTOS, n = 1) %>%
  select(NOME_MUNICIPIO, SIGLA_PARTIDO, TOTAL_VOTOS, percent) -> dep_fed

colnames(dep_fed) <- c("municipio", "partido_dep", "n_votos_dep", "prcnt_votos_dep")

prefeitos_ibge %>%
  mutate(name_muni = toupper(name_muni)) %>%
  left_join(ibge, by = c("name_muni" = "municipio")) %>%
  select(name_muni, pop_2010, pib_percapita) %>%
  distinct(name_muni, pop_2010, pib_percapita) %>%
  right_join(colegios, by = c("name_muni" = "Abrangência")) %>%
  left_join(pref_eleitos) %>%
  select(-UF) %>%
  select(name_muni, partido, Quantidade, everything()) %>%
  rename(n_eleitores = Quantidade,
         nome_municipio = name_muni) %>%
  arrange(desc(n_eleitores)) -> tabelinha

colnames(tabelinha) <- c("municipio", "partido_pref", "n_eleitores", "pop_2010",
                         "pib_percapita", "nome", "n_votos_pref", "prcnt_votos_pref")

receita <- read.csv2(file = "data/receitas_candidatos_2020_BA.csv", encoding = "latin1")
despesa <- read.csv2(file = "data/despesas_contratadas_candidatos_2020_BA.csv", encoding = "latin1")

receita %>%
  filter(DS_CARGO == "Prefeito") %>%
  group_by(NR_CPF_CANDIDATO) %>%
  summarise(receita = sum(VR_RECEITA)) -> receita

despesa %>%
  filter(DS_CARGO == "Prefeito") %>%
  group_by(NR_CPF_CANDIDATO) %>%
  summarise(despesa_contratada = sum(VR_DESPESA_CONTRATADA)) -> despesa

cand2020 <- candidate_local(year = 2020, uf = "BA")

cand2020 <- cand2020[, c(1:62)]

cand2020 %>%
  mutate(CPF_CANDIDATO = as.numeric(CPF_CANDIDATO)) %>%
  left_join(receita, by = c("CPF_CANDIDATO" = "NR_CPF_CANDIDATO")) %>%
  left_join(despesa, by = c("CPF_CANDIDATO" = "NR_CPF_CANDIDATO")) %>%
  filter(DESCRICAO_CARGO == "PREFEITO") %>%
  select(NOME_URNA_CANDIDATO, SIGLA_PARTIDO, receita, despesa_contratada) -> despesas_cand

tabelinha <- tabelinha %>%
  left_join(dep_fed) %>%
  select(municipio, nome, partido_pref, n_eleitores, pop_2010, pib_percapita,
         n_votos_pref, prcnt_votos_pref, partido_dep, n_votos_dep, prcnt_votos_dep)

