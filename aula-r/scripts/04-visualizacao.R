
library(tidyverse)


# importa a base  ---------------------------------------------------------

pinguins <- read_csv("aula-r/dados/pinguins/pinguins.csv")


# Camadas dos gráficos ----------------------------------------------------

cars %>%
  ggplot()

cars %>%
  ggplot() +
  aes(x = speed, y = dist)

cars %>%
  ggplot() +
  aes(x = speed, y = dist) +
  geom_point()



cars %>%
  ggplot() +
  aes(x = speed, y = dist) +
  geom_point(color = "darkblue") +
  geom_smooth(se = FALSE, color = "darkgrey", method="lm", formula = "y~x") +
  labs(
    title = "A velocidade influencia na distância de parada?",
    subtitle = "Distância necessária para parar o carro",
    caption = "Fonte: Ezekiel, M. (1930) Methods of Correlation Analysis. Wiley",
    x = "Velocidade",
    y = "Distância para parar"
  ) +
  theme_minimal()



# Gráfico de dispersão (pontos) -------------------------------------------

pinguins %>%
  ggplot() +
  aes(x = flipper_length_mm, y = body_mass_g) +
  geom_point()


# adiciona uma linha de tendência
pinguins %>%
  ggplot() +
  aes(x = flipper_length_mm, y = body_mass_g) +
  geom_point() +
  geom_smooth(method = "lm", formula = "y ~ x", se = FALSE)


# importante notar como a ordem das camadas
# é importante na construção de um gráfico
pinguins %>%
  ggplot() +
  aes(x = flipper_length_mm, y = body_mass_g) +
  geom_smooth(method = "lm", formula = "y ~ x", se = FALSE) +
  geom_point()

# adição de mais um atributo para ser mapeado
pinguins %>%
  ggplot() +
  aes(x = flipper_length_mm, y = body_mass_g) +
  geom_point(aes(color = species))


pinguins %>%
  ggplot() +
  aes(x = flipper_length_mm, y = body_mass_g) +
  geom_point(aes(color = species, shape = sex))


# Gráfico de linhas -------------------------------------------------------

pinguins %>%
  group_by(year) %>%
  summarise(
    mean_body_mass_g = mean(body_mass_g, na.rm = TRUE)
  ) %>%
  ggplot() +
  geom_line(aes(x = year, y = mean_body_mass_g))


# linhas + pontos no mesmo gráfico
pinguins %>%
  group_by(year) %>%
  summarise(
    mean_body_mass_g = mean(body_mass_g, na.rm = TRUE)
  ) %>%
  ggplot() +
  geom_line(aes(x = year, y = mean_body_mass_g)) +
  geom_point(aes(x = year, y = mean_body_mass_g))


# mais de um geom, pode ser interessante definir
# os atributos no aes() do ggplot()
pinguins %>%
  group_by(year) %>%
  summarise(
    mean_body_mass_g = mean(body_mass_g, na.rm = TRUE)
  ) %>%
  ggplot(aes(x = year, y = mean_body_mass_g)) +
  geom_line() +
  geom_point()


# se geom precisar de atributo que os outros não precisam
# esse atributo é especificado dentro da função aes() do próprio geom
pinguins %>%
  group_by(year) %>%
  summarise(
    mean_body_mass_g = round(mean(body_mass_g, na.rm = TRUE), 2)
  ) %>%
  ggplot(aes(x = year, y = mean_body_mass_g)) +
  geom_line() +
  geom_label(aes(label = mean_body_mass_g))


# Gráfico de barras -------------------------------------------------------

pinguins %>%
  count(species) %>%
  ggplot() +
  geom_col(aes(x = species, y = n))


# adicionando cores nas barras
# importante notar que o argumento é fill =
# nesse caso, o argumento color = colore apenas as bordas das barras
pinguins %>%
  count(species) %>%
  ggplot() +
  geom_col(
    aes(x = species, y = n, fill = species),
    show.legend = FALSE
  )


# inverte os eixos do gráfico
# constrói barras horizontais
pinguins %>%
  count(species) %>%
  ggplot() +
  geom_col(
    aes(x = species, y = n, fill = species),
    show.legend = FALSE
  ) +
  coord_flip()


# para ordenar colunas para fator: fct_reorder()
pinguins %>%
  count(species) %>%
  mutate(species = forcats::fct_reorder(species, n)) %>%
  ggplot() +
  geom_col(
    aes(x = species, y = n, fill = species),
    show.legend = FALSE
  ) +
  coord_flip()


# adiciona label (etiqueta) em cada barra
pinguins %>%
  count(species) %>%
  mutate(species = forcats::fct_reorder(species, n)) %>%
  ggplot() +
  aes(x = species) + #<< # atributo espécie é comum a todos geoms
  geom_col(aes(y = n, fill= species), show.legend = FALSE) +
  geom_label(aes(y = n/2, label = n)) +
  coord_flip()



# Histogramas -------------------------------------------------------------

# úteis para avaliar a distribuição de uma variável
pinguins %>%
  filter(species == "Adelie") %>%
  ggplot() +
  geom_histogram(aes(x = body_mass_g))


# binwidth = define o tamanho de cada intervalo
pinguins %>%
  filter(species == "Adelie") %>%
  ggplot() +
  geom_histogram(aes(x = body_mass_g),
                 # define o tamanho de cada intervalo
                 binwidth = 100,
                 # altera a cor das fordas de cada coluna
                 color = "white")

# Boxplot -----------------------------------------------------------------

# importantes para comparar várias distribuições
pinguins %>%
  ggplot() +
  aes(x = species, y = flipper_length_mm) +
  geom_boxplot(aes(color = species), show.legend = FALSE)



# Títulos, labels e escalas -----------------------------------------------

# labs() coloca títulos no gráfico/altera labels dos atritubos
# scale_* altera as escalar (textos e quebras) dos gráficos
# coord_cartesian() defina a porção do gráfico que deve ser mostrada

# adicionando títulos e labels
pinguins %>%
  ggplot() +
  aes(x = flipper_length_mm, y = body_mass_g) +
  geom_point(aes(color = species)) +
  labs(x = "Comprimento da nadadeira (mm)", y = "Massa corporal (mm)",
       color = "Espécies", title = "Gráfico de dispersão",
       subtitle = "Comprimento da nadadeira vs Massa corporal",
       caption = "{palmerpenguins} R package")


# alterando as quebras dos eixos x e y
pinguins %>%
  group_by(year) %>%
  summarise(
    mean_body_mass_g = mean(body_mass_g, na.rm = TRUE)
  ) %>%
  ggplot() +
  geom_line(aes(x = year, y = mean_body_mass_g)) +
  scale_x_continuous(breaks = seq(2007, 2009, 1)) +
  scale_y_continuous(breaks = seq(4000, 4250, 25))


# Cores -------------------------------------------------------------------

# scale_color_manual() e scale_fill_manual() escole manualmente as cores do gráfico
# scale_color_discrete() e scale_fill_discrete() troca o nome nas legendas geradas

# substituindo as cores
pinguins %>%
  count(species) %>%
  mutate(species = forcats::fct_reorder(species, n)) %>%
  ggplot() +
  aes(x = species, y = n) +
  geom_col(aes(fill = species), show.legend = FALSE)+
  coord_flip() +
  scale_fill_manual(values = c("red", "blue", "purple"))


# também pode usar códigos hexadecimais
pinguins %>%
  count(species) %>%
  mutate(species = forcats::fct_reorder(species, n)) %>%
  ggplot() +
  aes(x = species, y = n) +
  geom_col(aes(fill = species), show.legend = FALSE)+
  coord_flip() +
  scale_fill_manual(values = c("#ff0000", "#0000ff", "#a020f0"))


# adiciona transparência (alpha)
# o argumento position = "identity" mantem as variáveis na ordem
pinguins %>%
  ggplot(aes(x = flipper_length_mm)) +
  geom_histogram(aes(fill = species),
                 alpha = 0.5, position = "identity") +
  scale_fill_manual(values = c("darkorange","purple","cyan4")) +
  labs(x = "Comprimento da nadadeira (mm)", y = "Frequência",
       title = "Comprimento da nadadeira dos Pinguins") +
  theme_minimal()


pinguins %>%
  ggplot() +
  aes(x = species, y = flipper_length_mm, color = species) +
  geom_boxplot(width = 0.3, show.legend = FALSE) +
  geom_jitter(alpha = 0.5, show.legend = FALSE,
              position = position_jitter(width = 0.2, seed = 0)) +
  scale_color_manual(values = c("darkorange","purple","cyan4")) +
  labs(x = "Espécies", y = "Comprimento da nadadeira (mm)") +
  theme_minimal()


# Paletas de cores prontas:
# Escalas qualitativas: utilizado para variáveis nominais (sexo, cor/raça)
# Escalas divergenstes: utilizado para variáveis que têm um centro neutro
# (favorável/neutro/desfavorável, correlação)
# Escalas sequenciais: utilizado para variáveis ordinais (faixa etária, renda)
# Viridis muito últil para comunicar com pessoas com daltonismo

# Paletas de cores no ggplot2:
# scale_*_brewer(): utilizada para variáveis discretas.
# Possui três tipos: divergente, qualitativa e sequencial.
# scale_*_distiller(): utilizada para variáveis contínuas.
# Interpola as cores do brewer para lidar com todos os valores.
# scale_*_fermenter(): utilizada para variáveis contínuas,
# que são transformadas em discretas (binned).
# scale_*_viridis_[cdb]: Escala viridis para variáveis contínuas, discretas ou binned.
# scale_*_manual(): inclui um conjunto de cores manualmente.

# Paletas de outros pacotes:
# {ggthemr} tem um monte de paletas, mas está um pouco desatualizado.
# {hrbrthemes} contém uma lista de temas escolhidos pelo Bob Rudis.
# {ghibli} tem paletas de cores relacionadas ao Studio Ghibli.
# {paletteer} tem uma coleção de cores de vários outros pacotes de paletas.
# {RColorBrewer} tem uma coleção de paletas
# (sequenciais, qualitativas e divergentes) para gráficos e mapas.



# Trocando os textos da legenda -------------------------------------------

pinguins %>%
  ggplot() +
  aes(x = flipper_length_mm, y = body_mass_g) +
  geom_point(aes(color = species)) +
  scale_color_discrete(
    labels = c(
      "Pinguim de Adelia", "Pinguim de Barbicha", "Pinguim Gentoo"
    )
  )


# Temas -------------------------------------------------------------------

# função theme_*()
# é possível criar temas próprios usando a função theme()

# tema padrão
pinguins %>%
  ggplot() +
  aes(x = flipper_length_mm, y = body_mass_g) +
  geom_point()

# tema bw
pinguins %>%
  ggplot() +
  aes(x = flipper_length_mm, y = body_mass_g) +
  geom_point() +
  theme_bw()

# tema classic
pinguins %>%
  ggplot() +
  aes(x = flipper_length_mm, y = body_mass_g) +
  geom_point() +
  theme_classic()

# tema light
pinguins %>%
  ggplot() +
  aes(x = flipper_length_mm, y = body_mass_g) +
  geom_point() +
  theme_light()

# thema minimal
pinguins %>%
  ggplot() +
  aes(x = flipper_length_mm, y = body_mass_g) +
  geom_point() +
  theme_minimal()

# tema void - interessante para mapas
pinguins %>%
  ggplot() +
  aes(x = flipper_length_mm, y = body_mass_g) +
  geom_point() +
  theme_void()

# tema dark
pinguins %>%
  ggplot() +
  aes(x = flipper_length_mm, y = body_mass_g) +
  geom_point() +
  theme_dark()


# criando um próprio tema
pinguins %>%
  ggplot() +
  aes(x = flipper_length_mm, y = body_mass_g) +
  geom_point(aes(color = species)) +
  labs(title = "Gráfico de dispersão",
       subtitle = "Comprimento da nadadeira vs Massa corporal") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.title = element_text(color = "red"),
        panel.background = element_rect(fill = "lightgrey"),
        panel.grid = element_blank())



# Mapas -------------------------------------------------------------------


# importa a base ----------------------------------------------------------

# importa a base do ideb para o ensino fundamental
ideb_fundamental <- read_csv("aula-r/dados/educacionais/ideb_fundamental.csv")

# download dos dados da malha dos municipios brasileiros
mapa_muni <- geobr::read_municipality(year = 2020)

# junta as duas bases de dados em uma só
ideb_mapa <- left_join(
  mapa_muni, ideb_fundamental, by = c("code_muni" = "id_municipio")
)


# há duas colunas importantes para o desenvolvimento do mapa: geom e ideb
# geom fornece as geometrias (bordas) do conjunto de municípios
# ideb preenche os espaços dos municípios com as cores relativas às notas do ideb
glimpse(ideb_mapa)



# mapa do Brasil ----------------------------------------------------------

ggplot(ideb_mapa) +
  aes(geometry = geom) +
  geom_sf(aes(fill = ideb), color = NA) +
  scale_fill_viridis_c(
    na.value = "grey90", alpha = 0.8, name = "Ideb"
  ) +
  labs(title = "Notas médias do Ideb para o Ensino Fundamental",
       subtitle = "nos municípios brasileiros em 2021",
       caption = paste(
         "Fonte: Bases dos Dados | Inep | {geobr} R package",
         "\nDuzentos e quarenta e um (241) municípios brasileiros",
         "\nnão possuíam notas do Ideb no ano de 2021"
       )) +
  theme_void() +
  theme(
    plot.caption = element_text(lineheight = 1.1),
    legend.position = c(0.2, 0.25)
  )



# mapa do Rio Grande do Sul -----------------------------------------------

ideb_mapa %>%
  filter(abbrev_state == "RS") %>%
  ggplot(aes(geometry = geom)) +
  geom_sf(aes(fill = ideb)) +
  scale_fill_viridis_c(
    na.value = "grey90", alpha = 0.6, name = "Ideb"
  ) +
  labs(title = "Notas médias do Ideb para o Ensino Fundamental",
       subtitle = "nos municípios do Rio Grande do Sul em 2021",
       caption = paste(
         "Fonte: Bases dos Dados | Inep | {geobr} R package",
         "\nTrinta e sete (37) municípios gaúchos não possuíam",
         "\nnotas do Ideb no ano de 2021"
       )) +
  theme_void() +
  theme(plot.caption = element_text(lineheight = 1.1),
        legend.position = c(0.2, 0.2))



# mapa do RS + cidades ----------------------------------------------------


# para adicionar os pontos referentes às cidades gaúchas,
# é preciso criar um data frame com as coordenadas (longitude e latitude)
# respectivas de cada cidade retratada.
cidade_rs <- data.frame(
  abbrev_state = rep("RS", 5),
  name_muni = c(
    "Porto Alegre", "Caxias do Sul", "Canoas", "Pelotas", "Santa Maria"
  ),
  long = c(-30.032778,  -29.167778, -29.92, -31.771944, -29.683889),
  lat = c(-51.23, -51.178889, -51.18, -52.342778, -53.806944))

# esse data frame será incluído como base para a construção dos
# pontos e dos nomes das cidades.
# geom_point é a função responsável por adicionar os pontos de cada cidade
# geom_text_repel do pacote {ggrepel} é responsável por adicionar os nomes
# de cada uma das cidades.

ideb_mapa %>%
  filter(abbrev_state == "RS") %>%
  ggplot() +
  geom_sf(aes(geometry = geom, fill = ideb)) +
  scale_fill_viridis_c(alpha = 0.5, na.value = "grey90") +
  geom_point(data = cidade_rs, aes(x = lat, y = long), size = 1.5) +
  ggrepel::geom_text_repel(data = cidade_rs,
                           aes(x = lat, y = long, label = name_muni),
                           fontface = "bold", size = 4,
                           nudge_x = c(3, 2, 2, 2, -3),
                           nudge_y = c(0, 1, 0.5, -0.5, 1.5)) +
  labs(title = "Notas médias do Ideb para o Ensino Fundamental",
       subtitle = "nos municípios do Rio Grande do Sul em 2021",
       caption =paste("Fonte: Bases dos Dados | Inep | {geobr} R package",
                      "\nDentre as 5 principais cidades do RS, Caxias do Sul atingiu",
                      "\na maior nota com 5.83, seguida de Santa Maria, Pelotas,",
                      "\nCanoas e Porto Alegre. \nTrinta e sete (37) municípios gaúchos",
                      "\nnão possuíam notas do Ideb no ano de 2021")) +
  theme_void() +
  theme(plot.caption = element_text(lineheight = 1.1),
        legend.position = c(0.2, 0.2))










