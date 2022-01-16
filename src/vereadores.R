
library(geobr)
library(tidyverse)
library(electionsBR)
library(ggcats)
library(RColorBrewer)

load(file = "data/dados_bahia.RData")

## Número de vereados eleitos por partido ----

vereadores %>%
  filter(situacao == "Eleito") %>%
  group_by(partido) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = reorder(partido, n), y = n)) +
  geom_col(fill = "#B73A3A") +
  geom_text(aes(label = signif(n)), nudge_y = 25, size = 3.5) +
  labs(x = "", y = "Número de vereadores eleitos",
       title = "Número de vereadores eleitos por partido",
       subtitle = "Eleições Municipais da Bahia 2020",
       caption = "Fonte: TSE") +
  theme_classic() +
  theme(plot.title = element_text(face = "bold"),
        axis.title.x = element_text(vjust = -2)) +
  coord_flip()


# Profissão dos candidatos 2020 (comparação 2016)

# Quantos candidatos adicionaram o termo DR. ou DRA. de 2016 para 2020 ----
