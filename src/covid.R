
urnas <- read.csv2(file = "~/Documents/bweb_1t_BA_181120201549.csv", encoding = "latin1")
covid <- read_csv(file = "~/Documents/caso_full.csv")

# Abstenções

urnas %>%
  select(NM_MUNICIPIO, NR_SECAO, QT_APTOS, QT_ABSTENCOES) %>%
  unique() %>%
  group_by(NM_MUNICIPIO) %>%
  summarise(abst = sum(QT_ABSTENCOES),
            aptos = sum(QT_APTOS)) %>%
  ungroup() %>%
  mutate(pct_abs = abst/aptos) -> abstencoes_mun

covid %>%
  filter(state == "BA") %>%
  filter(date == "2020-11-12") %>%
  filter(!is.na(city)) %>%
  mutate(city = toupper(city)) %>%
  right_join(abstencoes_mun, by = c("city" = "NM_MUNICIPIO")) %>%
  drop_na(last_available_confirmed_per_100k_inhabitants) -> covid2

coef <- cor(covid2$last_available_confirmed_per_100k_inhabitants, covid2$pct_abs)

covid2 %>%
  ggplot(aes(x = last_available_confirmed_per_100k_inhabitants, y = pct_abs)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1L)) +
  annotate(x = 5000, y = .3, label = paste0("r = ", round(coef, 4)),
           geom = "text", size = 4.5, ) +
  labs(x = "Casos confirmados por 100 mil habitantes",
       y = "Porcentagem de abstenções",
       title = "Relação entre o número de casos por 100 mil habitantes e a porcentagem de abstenções",
       subtitle = "Panorama COVID-19 e Eleições Municipais na Bahia 2020",
       caption = "Fonte: TSE e Brasil.io") +
  theme_classic() +
  theme(plot.title = element_text(face = "bold"),
        axis.title.x = element_text(vjust = -2))

covid %>%
  filter(state == "BA") %>%
  filter(date == "2020-11-12") %>%
  filter(!is.na(city)) %>%
  mutate(city = toupper(city)) %>%
  right_join(abstencoes_mun, by = c("city" = "NM_MUNICIPIO")) %>%
  drop_na(last_available_confirmed_per_100k_inhabitants) %>%
  arrange(desc(last_available_confirmed_per_100k_inhabitants)) %>%
  slice(1:50) %>%
  ggplot(aes(x = last_available_confirmed_per_100k_inhabitants, y = pct_abs)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1L)) +
  labs(x = "Casos confirmados por 100 mil habitantes",
       y = "Porcentagem de abstenções",
       title = "Relação entre o número de casos por 100 mil habitantes e a porcentagem de abstenções",
       subtitle = "Panorama COVID-19 e Eleições Municipais 2020 (50 municípios com maior número de casos por 100 mil habitantes)",
       caption = "Fonte: TSE e Brasil.io") +
  theme_classic() +
  theme(plot.title = element_text(face = "bold"),
        axis.title.x = element_text(vjust = -2))

colegios <- read_csv(file = "data/quantidade_de_eleitores.csv")

colegios <- colegios %>%
  mutate(Quantidade = as.numeric(str_replace(Quantidade, "[:punct:]", ""))) %>%
  mutate(Quantidade = as.numeric(str_replace(Quantidade, "[:punct:]", ""))) %>%
  arrange(desc(Quantidade)) %>%
  slice(1:30)

covid %>%
  filter(state == "BA") %>%
  filter(date == "2020-11-12") %>%
  filter(!is.na(city)) %>%
  mutate(city = toupper(city)) %>%
  right_join(abstencoes_mun, by = c("city" = "NM_MUNICIPIO")) %>%
  drop_na(last_available_confirmed_per_100k_inhabitants) %>%
  right_join(colegios, by = c("city" = "Abrangência")) %>%
  ggplot(aes(x = last_available_confirmed_per_100k_inhabitants, y = pct_abs)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1L)) +
  labs(x = "Casos confirmados por 100 mil habitantes",
       y = "Porcentagem de abstenções",
       title = "Relação entre o número de casos por 100 mil habitantes e a porcentagem de abstenções",
       subtitle = "Panorama COVID-19 e Eleições Municipais da Bahia 2020 (30 maiores colégios eleitorais)",
       caption = "Fonte: TSE e Brasil.io") +
  theme_classic() +
  theme(plot.title = element_text(face = "bold"),
        axis.title.x = element_text(vjust = -2))

# Brancos e nulos

urnas %>%
  mutate(DS_TIPO_VOTAVEL = fct_recode(DS_TIPO_VOTAVEL,
                                      "B/N" = "Branco",
                                      "B/N" = "Nulo")) %>%
  select(NM_MUNICIPIO, NR_SECAO, QT_APTOS, DS_TIPO_VOTAVEL, QT_VOTOS) %>%
  unique() %>%
  group_by(NM_MUNICIPIO, DS_TIPO_VOTAVEL) %>%
  summarise(votos = sum(QT_VOTOS),
            aptos = sum(QT_APTOS)) %>%
  ungroup() %>%
  filter(DS_TIPO_VOTAVEL == c("B/N")) %>%
  group_by(NM_MUNICIPIO, DS_TIPO_VOTAVEL) %>%
  mutate(votos = sum(votos)) %>%
  mutate(pct_bn = votos/aptos) -> bn_mun

covid %>%
  filter(state == "BA") %>%
  filter(date == "2020-11-12") %>%
  filter(!is.na(city)) %>%
  mutate(city = toupper(city)) %>%
  right_join(bn_mun, by = c("city" = "NM_MUNICIPIO")) %>%
  drop_na(last_available_confirmed_per_100k_inhabitants) -> covid3

coef <- cor(covid3$last_available_confirmed_per_100k_inhabitants, covid3$pct_bn)

covid3 %>%
  ggplot(aes(x = last_available_confirmed_per_100k_inhabitants, y = pct_bn)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  scale_y_continuous(labels = scales::percent) +
  annotate(x = 5000, y = .06, label = paste0("r = ", round(coef, 4)),
           geom = "text", size = 4.5, ) +
  labs(x = "Casos confirmados por 100 mil habitantes",
       y = "Porcentagem de votos brancos/nulos",
       title = "Relação entre o número de casos por 100 mil habitantes e a porcentagem de votos brancos/nulos",
       subtitle = "Panorama COVID-19 e Eleições Municipais da Bahia 2020",
       caption = "Fonte: TSE e Brasil.io") +
  theme_classic() +
  theme(plot.title = element_text(face = "bold"),
        axis.title.x = element_text(vjust = -2))

covid %>%
  filter(state == "BA") %>%
  filter(date == "2020-11-12") %>%
  filter(!is.na(city)) %>%
  mutate(city = toupper(city)) %>%
  right_join(bn_mun, by = c("city" = "NM_MUNICIPIO")) %>%
  drop_na(last_available_confirmed_per_100k_inhabitants) %>%
  arrange(desc(last_available_confirmed_per_100k_inhabitants)) %>%
  slice(1:50) %>%
  ggplot(aes(x = last_available_confirmed_per_100k_inhabitants, y = pct_bn)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue", ) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Casos confirmados por 100 mil habitantes",
       y = "Porcentagem de votos brancos/nulos",
       title = "Relação entre o número de casos por 100 mil habitantes e a porcentagem de votos brancos/nulos",
       subtitle = "Panorama COVID-19 e Eleições Municipais da Bahia 2020 (50 municípios com maior número de casos por 100 mil habitantes)",
       caption = "Fonte: TSE e Brasil.io") +
  theme_classic() +
  theme(plot.title = element_text(face = "bold"),
        axis.title.x = element_text(vjust = -2))

colegios <- read_csv(file = "data/quantidade_de_eleitores.csv")

colegios <- colegios %>%
  mutate(Quantidade = as.numeric(str_replace(Quantidade, "[:punct:]", ""))) %>%
  mutate(Quantidade = as.numeric(str_replace(Quantidade, "[:punct:]", ""))) %>%
  arrange(desc(Quantidade)) %>%
  slice(1:30)

covid %>%
  filter(state == "BA") %>%
  filter(date == "2020-11-12") %>%
  filter(!is.na(city)) %>%
  mutate(city = toupper(city)) %>%
  right_join(bn_mun, by = c("city" = "NM_MUNICIPIO")) %>%
  drop_na(last_available_confirmed_per_100k_inhabitants) %>%
  right_join(colegios, by = c("city" = "Abrangência")) %>%
  ggplot(aes(x = last_available_confirmed_per_100k_inhabitants, y = pct_bn)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Casos confirmados por 100 mil habitantes",
       y = "Porcentagem de votos brancos/nulos",
       title = "Relação entre o número de casos por 100 mil habitantes e a porcentagem de votos brancos/nulos",
       subtitle = "Panorama COVID-19 e Eleições Municipais da Bahia 2020 (30 maiores colégios eleitorais)",
       caption = "Fonte: TSE e Brasil.io") +
  theme_classic() +
  theme(plot.title = element_text(face = "bold"),
        axis.title.x = element_text(vjust = -2))
