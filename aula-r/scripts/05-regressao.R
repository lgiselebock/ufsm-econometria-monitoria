
library(tidyverse)


# DADOS EDUCACIONAIS ------------------------------------------------------

### importa a base de dados

ideb_fund <- read_csv(
  "aula-r/dados/educacionais/ideb_fundamental.csv",
  col_types = cols(id_municipio = col_character(), ano = col_integer())
)

ideb_medio <- read_csv(
  "aula-r/dados/educacionais/ideb_medio.csv",
  col_types = cols(id_municipio = col_character(), ano = col_integer())
)

indic_fund <- read_csv(
  "aula-r/dados/educacionais/indicadores_fundamental.csv",
  col_types = cols(id_municipio = col_character(), ano = col_integer())
)

indic_medio <- read_csv(
  "aula-r/dados/educacionais/indicadores_medio.csv",
  col_types = cols(id_municipio = col_character(), ano = col_integer())
)

pib_pop <- read_csv(
  "aula-r/dados/educacionais/pib_pc.csv",
  col_types = cols(id_municipio = col_character(), ano = col_integer())
)


head(ideb_fund, n = 5) # primeiras 5 linhas de ideb_fund

head(ideb_medio, n = 5) # primeiras 5 linhas de ideb_medio

head(indic_fund, n = 5) # primeiras 5 linhas de indic_fund

head(indic_medio, n = 5) # primeiras 5 linhas de indic_medio

head(pib_pop, n = 5) # primeiras 5 linhas de pip_pop

###########################################################################################

## DICIONÁRIO

# BASES IDEB_FUND E IDEB_MEDIO

# Fund: Fundamental
# Medio: Médio

# 1. `id_municipio`: código do IBGE de identificação do município
# 2. `tx_aprov`: taxa de aprovação média do município
# 3. `ideb`: índice de desenvolvimento da educação básica
# - é calculado a partir dos dados de aprovação escolar (obtidos pelo Censo Escolar)
# e das médias de desempenho no Sistema de Avaliação da Educação Básica (Saeb)

# BASES INDIC_FUND E INDIC_MEDIO

# Indic: Indicadores
# Fund: Fundamental
# Medio: Médio

# 1. `id_municipio`: código do IBGE de identificação do município
# 2. `tx_aband`: taxa de abandono escolar média do município
# 3. `hrs_aula`: média das horas de aula por município
# 4. `tdi`: taxa de distorção idade-série
# - proproção de alunos com mais de 2 anos de atraso escolar

# PIB

# 1. `id_municipio`: código do IBGE de identificação do município
# 2. `pib`: pib dos municípios em 2020
# 3. `populacao`: população dos municípios em 2020

###########################################################################################

### padronizando as bases

ideb_fundamental <- ideb_fund %>%
  select(id_municipio, tx_aprov, ideb)

# select(ideb_fund, id_municipio, tx_aprov, ideb)

ideb_fundamental


ideb_medio <- ideb_medio %>%
  select(id_municipio, tx_aprov, ideb)

ideb_medio


indicadores_fundamental <- indic_fund %>%
  select(id_municipio, tx_aband, hrs_aula, tdi)

indicadores_fundamental


indicadores_medio <- indic_medio %>%
  select(id_municipio, tx_aband, hrs_aula, tdi)

indicadores_medio


pib_pc <- pib_pop %>%
  mutate(pib_pc = pib/populacao) %>% # cria a coluna pib_pc
  select(id_municipio, pib_pc)

pib_pc


### juntando as bases de dados: fundamental

# 2 `left-join()` seguidos

fund <- left_join(
  ideb_fundamental, indicadores_fundamental, by = "id_municipio"
) %>%
  left_join(pib_pc, by = "id_municipio")

fund


# purrr::reduce (programação funcional - é o mais indicado!)

fund <- list(ideb_fundamental, indicadores_fundamental, pib_pc) %>%
  reduce(left_join)

fund


### análises descritivas: fundamental

# resumo

summary(fund)


# histogram

fund %>%
  ggplot() +
  aes(x = ideb) +
  geom_histogram()


# boxplot (variável no eixo x)

fund %>%
  ggplot() +
  aes(x = ideb) +
  geom_boxplot()


# boxplot (variável no eixo y)

fund %>%
  ggplot() +
  aes(y = ideb) +
  geom_boxplot()


### regressão linear: fundamental

# Será que a taxa de distorção idade-série e a taxa de
# abandono escolar possuem relação positiva?

# sem linha de tendência

fund %>%
  ggplot(aes(x = tdi, y = tx_aband)) +
  geom_point()


# com linha de tendência

fund %>%
  ggplot(aes(x = tdi, y = tx_aband)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)


# modelo

modelo1 <- lm(tx_aband ~ tdi, data = fund)

summary(modelo1)

# A relação entre a taxa de distorção idade-série (tdi)
# e a taxa de abandono escolar (tx_aband) é positiva,
# mas não parece ser muito forte.

# A variável tdi tem significância estatística muito próxima a zero.
# E o R² é igual a 0.3074, indicando que 30.74% da equação
# é explicada pela variável explicativa.

# EQUAÇÃO DE REGRESSÃO: tx_aband = −0.568 + 0.124 tdi

# O aumento de 1 p.p. na taxa de distorção idade-série (tdi)
# retorna um aumento de 0.12 p.p. na taxa de abandono escolar (tx_aband).

# Percebe-se que quanto maior for a distorção idade-série (tdi),
# maiores serão as taxas de abandono escolar (tx_aband).


# Qual a relação entre tdi, tx_aprov, hrs_aula e a tx_aband?

# modelo

modelo2 <- lm(tx_aband ~ tdi + tx_aprov + hrs_aula, data = fund)

summary(modelo2)

# A taxa de distorção idade-série (dti) e as horas-aula (hrs_aula)
# possuem relação positiva em relação à taxa de abandono (tx_aband),
# sendo a primeira variável com significância estatística muito próxima
# de zero, ou seja, significativa estatisticamente; e a segunda variável
# significativa estatisticamente para 10%.
# A taxa de aprovação (tx_aprov) apresenta uma relação negativa com a taxa de
# abandono (tx_aband), tendo significância estatística próxima a zero.
# O R² é igual a 0.525, ou seja, 52.5% da equação é explicada pelos parâmetros.
# O Teste F é estatisticamente significativo, indicando que
# todos os parâmetros são conjuntamente significativos.

# EQ. REGRESSÃO: tx_aband = 22.093 + 0.059 tdi −0.228 tx_aprov + 0.040 hrs_aula

# Um aumento de 1 p.p. na taxa de distorção idade-série (tdi) gera um aumento
# de 0.059 p.p. na taxa de abandono escolar (tx_aband), ceteris paribus.

# Um aumento de 1 p.p. na taxa de aprovação (tx_aprov) resulta em uma redução
# de 0.228 p.p. na taxa de abandono (tx_aband), ceteris paribus.

# Um aumento de 1 hora-aula (hrs_aula) resulta em um aumento de 0.04 p.p.
# na taxa de abandono escolar (tx_aband), ceteris paribus.


# Qual a relação entre tdi, tx_aprov, pib_pc e a tx_aband?

# modelo

modelo3 <- lm(tx_aband ~ tdi + tx_aprov + pib_pc, data = fund)

summary(modelo3)

# Enquanto a taxa de distorção idade-série (tdi) tem uma relação positiva
# com a taxa de abandono escola (tx_aband), as variáveis de taxa de aprovação
# escolar (tx_aprov) e de Pib per capita possuem uma relação negativa.
# Todos os parâmetros são estatisticamente significantes. O R² é igual
# a 0.5293, indicando que 52.93% da equação é explicada pelo modelo.
# Teste F tem p-valor muito próximo a zero, ou seja,
# todos os parâmetros são conjuntamente significativos.

# EQ. REGRESSÃO: tx_aband = 22.438 + 0.056 tdi − 0.229 tx_aprov − 0.000004 pib_pc

# O pib per capita (pib_pc) possui uma relação negativa com a taxa de
# abandono escolar (tx_aband), isto é, quanto maior for o pib per capita
# do município, menor será a taxa de abandono escolar.

# O aumento de 1 Real no pib per capita gera uma queda de 0.000004 p.p.
# na taxa de abandono escolar. Convertendo o valor do pib_pc para 10 mil Reais,
# isso corresponde a um decréscimo de 0.04 p.p. na taxa de abandono escolar.

# Isso indica que municípios mais ricos possuem valores consideravelmente
# menores de abandono escolar quando comparados com municípios mais pobres,
# ceteris paribus.


### juntando as bases de dados: médio

# 2 `left-join()` seguidos

medio <- left_join(
  ideb_medio, indicadores_medio, by = "id_municipio"
) %>%
  left_join(pib_pc, by = "id_municipio")

medio


# purrr::reduce (programação funcional - é o mais indicado!)

medio <- list(ideb_medio, indicadores_medio, pib_pc) %>%
  reduce(left_join)

medio


### análises descritivas: médio

# resumo

summary(medio)


# histogram

medio %>%
  ggplot() +
  aes(x = ideb) +
  geom_histogram()


# boxplot (variável no eixo x)

medio %>%
  ggplot() +
  aes(x = ideb) +
  geom_boxplot()


# boxplot (variável no eixo y)

medio %>%
  ggplot() +
  aes(y = ideb) +
  geom_boxplot()


### regressão linear: médio

# Qual é a relação entre o ideb e a tx_aband?

# sem linha de tendência

medio %>%
  ggplot(aes(x = ideb, y = tx_aband)) +
  geom_point()


# com linha de tendência

medio %>%
  ggplot(aes(x = ideb, y = tx_aband)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)


# Quanto maior for o ideb, menor será a taxa de abandono.
# Há uma relação forte com a queda na taxa de abandono.


# Qual a relação entre a tx_aprov e a tx_aband?

# sem linha de tendência

medio %>%
  ggplot(aes(x = tx_aprov, y = tx_aband)) +
  geom_point()


# com linha de tendência

medio %>%
  ggplot(aes(x = tx_aprov, y = tx_aband)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)


# Qual a relação entre a hrs_aula e a tx_aband?

# sem linha de tendência

medio %>%
  ggplot(aes(x = hrs_aula, y = tx_aband)) +
  geom_point()


# com linha de tendência

medio %>%
  ggplot(aes(x = hrs_aula, y = tx_aband)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)


# Qual a relação entre tdi, hrs_aula, pib_pc e a tx_aband?

# modelo

modelo4 <- lm(tx_aband ~ tx_aprov + tdi + hrs_aula + pib_pc,data = medio)

summary(modelo4)

# ED. REGRESSÃO: tx_aband = 49.94 − 0.504 tx_aprov + 0.093 tdi − 0.136 hrs_aula − 0.00001 pibpc

# O aumento de 1 p.p. na taxa de aprovação (tx_aprov) reduz em 0.50 p.p.
# a taxa de abandono escolar (tx_aband), ceteris paribus.

# O aumento de 1 p.p. na taxa de distorção idade-série (tdi) aumenta em
# 0.082 p.p. a taxa de abandono escolar (tx_aband), ceteris paribus.

# O aumento de 1 hora-aula (hrs_aula) diminui em 0.136 p.p. a taxa de
# abandono escolar (tx_aband), ceteris paribus.

# O aumento de 1 Real no pib per capita gera uma queda de 0.00001 p.p. na
# taxa de abandono escolar, ceteris paribus. Convertendo o valor do pib_pc
# para 10 mil Reais, isso corresponde a uma redução de 0.1 p.p. na
# taxa de abandono escolar, ceteris paribus.

# Quanto mais tempo o adolescente passa na escola, menor é a queda na
# taxa de abandono escolar. As taxas de abandono no ensino médio são maiores
# quando comparado com o ensino fundamental. O pib per capita afeta mais os
# dados do ensino médio do que do ensino fundamental.


### Base dos Dados

# Importando a base da BigQuery para o R:

# install.packages("basedosdados")
library(basedosdados)

# faz a ligação entre o R e a BigQuery do Google
set_billing_id("ID-PROJECT")

# sem filtro
"SELECT coluna1, coluna2, coluna3,
coluna4 AS apelido_da_coluna
FROM `tabela`"

# com filtro
"SELECT coluna1, coluna2, coluna3,
coluna4 AS apelido_da_coluna
FROM `tabela`
WHERE coluna1 = valor AND coluna2 = valor"


# ATLAS DO DESENVOLVIMENTO HUMANO -----------------------------------------

# importa os dados

adh <-
"SELECT id_municipio, ano, mortalidade_1, mortalidade_5,
taxa_freq_liquida_pre, indice_gini,prop_pobreza_extrema_criancas,
prop_pobreza_criancas, prop_vulner_pobreza_criancas,
taxa_agua_esgoto_inadequados, taxa_criancas_fora_escola_4_5
FROM `basedosdados.mundo_onu_adh.municipio`" %>%
  read_sql()

glimpse(adh)


# Será que a mortalidade infantil de crianças (até 1 ano) se
# relaciona com a taxa de água e esgoto inadequados?

# sem linha de tendência

adh %>%
  ggplot() +
  aes(x = taxa_agua_esgoto_inadequados, y = mortalidade_1) +
  geom_point()

# com linha de tendência

adh %>%
  ggplot() +
  aes(x = taxa_agua_esgoto_inadequados, y = mortalidade_1) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)


# Apesar dos pontos estarem espalhados no gráfico, é possível
# perceber uma relação positiva entre as variáveis.

# modelo

modelo5 <- lm(mortalidade_1 ~ taxa_agua_esgoto_inadequados, data=adh)

summary(modelo5)

# A taxa de pessoas em domicílios com abastecimento de água e esgoto
# sanitário inadequados (taxa_agua_esgoto_inadequados) se relaciona
# positivamente com a mortalidade de crianças com até um (1) ano de
# idade (mortalidade_1). É observado que a variável independente
# (taxa_agua_esgoto_inadequados) possui uma significância estatística
# muito próxima à zero, sendo significativa estatisticamente.
# O R² é igual a 0.3976, ou seja, 39.76% da equação é explicada pelo modelo.

# EQ. REGRESSÃO: mortalidade_1 = 24.099 + 0.689 taxa_agua_esgoto_inadequados

# Um aumento de 1 p.p. na taxa de água e esgoto inadequados
# (taxa_agua_esgoto_inadequados) gera um aumento de 0.689 p.p. na mortalidade
# de crianças com até um (1) ano de idade (mortalidade_1).



# Relação entre a proporção de extrema pobreza em crianças
# e a mortalidade infantil (até 5 anos)

# sem linha de tendência

adh %>%
  ggplot() +
  aes(x = prop_pobreza_extrema_criancas, y = mortalidade_5) +
  geom_point()


# com linha de tendência

adh %>%
  ggplot() +
  aes(x = prop_pobreza_extrema_criancas, y = mortalidade_5) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)


# É possível perceber uma relação positiva entre a proporção de extrema
# pobreza em crianças e a mortalidade infantil.

# modelo

modelo6 <- lm(mortalidade_5 ~ prop_pobreza_extrema_criancas, data = adh)

summary(modelo6)

# A proporção de crianças extremamente pobres (prop_pobreza_extrema_criancas)
# se relaciona positivamente com a mortalidade de crianças com até cinco (5)
# anos de idade (mortalidade_5). A variável dependente
# (prop_pobreza_extrema_criancas) apresenta uma significância estatística
# muito próxima à zero, sendo, assim, significativa estatisticamente.
# O R² é igual a 0.6447, ou seja, 64.47% da equação é explicada pelo modelo linear.

# EQ. REGRESSÃO: mortalidade_5 = 13.712 + 0.954 prop_pobreza_extrema_criancas

# Um aumento de 1 p.p. na proporção de crianças extremamente pobres
# (prop_pobreza_extrema_criancas) acarreta um aumento de 0.954 p.p. na
# mortalidade de crianças com até cinco (5) anos de idade (mortalidade_5).


# Será que há um padrão regional para a relação entre a proporção de
# extrema pobreza em crianças e a mortalidade infantil (até 5 anos)?

# Primeiro é preciso baixar os dados referentes as regiões do Brasil e,
# depois, fazer um left_join() com a base adh.
# Então, a base dados tem os dados de adh e os dados de regiao.

regiao <-
"SELECT id_municipio, nome_regiao
FROM `basedosdados.br_bd_diretorios_brasil.municipio`" %>%
  read_sql()

dados <- left_join(adh, regiao, by = "id_municipio")

# sem linha de tendência

dados %>%
  ggplot() +
  aes(x = prop_pobreza_extrema_criancas, y = mortalidade_5,
      color = nome_regiao) +
  geom_point() +
  scale_color_brewer(palette = "Set2")

# com linha de tendência

dados %>%
  ggplot() +
  aes(x = prop_pobreza_extrema_criancas, y = mortalidade_5,
      color = nome_regiao) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_brewer(palette = "Set2")


# Municípios do Norte e Nordeste têm altos índices de mortalidade infantil
# até 5 anos e de pobreza extrema entre as crianças.


# Relação entre crianças fora da escola (4 e 5 anos)
# e a proporção de crianças na extrema pobreza

dados %>%
  ggplot(aes(x = prop_pobreza_extrema_criancas,
             y = taxa_criancas_fora_escola_4_5, color = nome_regiao)) +
  geom_point(alpha = 0.5) +
  scale_color_brewer(palette = "Set2") +
  # cria uma linha de tendência para cada região do Brasil
  geom_smooth(method = "lm", se = FALSE)

# É possível ver que algumas regiões têm uma relação mais inclinada que outras.
# Por exemplo, um aumento na taxa de pobreza extrema das crianças gera
# aumentos muito mais fortes na taxa de crianças fora da escola
# na Região Sul do que no Nordeste.


# Média da Mortalidade Infantil (até 1 ano) por Região Brasileira em 1991, 2000 e 2010

# gráfico de pontos

dados %>%
  group_by(ano, nome_regiao) %>%
  summarise(media = mean(mortalidade_1, na.rm = TRUE)) %>%
  ggplot() +
  aes(x = media, y = nome_regiao, color = as.factor(ano)) +
  geom_point(size = 4)

# gráfico de barras

dados %>%
  group_by(ano, nome_regiao) %>%
  summarise(media = mean(mortalidade_1, na.rm = TRUE)) %>%
  ggplot() +
  aes(x = nome_regiao, y = media, fill = as.factor(ano)) +
  # indica como as barras devem ser posicionadas
  geom_bar(stat = "identity", position = "dodge")


# Os gráficos apresentam duas formas diferentes de apresentar a média da
# Mortalidade Infantil para crianças com até um (1) ano de idade por
# região brasileira em três diferentes anos de observação (1991, 2000 e 2010).
# Em ambos, é possível identificar que os níveis de Mortalidade em crianças
# com até cinco (5) anos diminuiu ao longo dos anos observados para
# todas as regiões brasileiras.

# Na Região Centro-Oeste, a redução foi de 4.91% entre os anos
# de 1991 e 2000; e 10.82% entre 2000 e 2010.

# Na Região Nordeste, a redução foi de 27.11% entre 1991 e 2000;
# e 21.9% entre os anos de 2000 e 2010.

# Na Região Norte, a redução foi de 18.52% de 1991 para 2000;
# e 16.64% de 2000 para 2010.

# Na Região Sudeste, a redução foi de 8.77% entre 1991 e 2000;
# e 9.08% entre 2000 e 2010.

# Na Região Sul, a redução foi de 9.04% entre os anos de 1991 e 2000;
# e 6.55% entre os anos de 2000 e 2010.


### Mapas

# Para criar mapas, é necessário realizar o download dos dados espaciais
# através do pacote {geobr}. E, depois, unir as bases com os dados espaciais
# dos municípios e com os dados do adh.

muni <- geobr::read_municipality(year = 2010) %>%
  mutate(code_muni = as.character(code_muni))

muni_dados <- left_join(
  muni, dados, by = c("code_muni" = "id_municipio")
)

# A nova base possui as seguintes colunas:

  ##  [1] "code_muni"                     "name_muni"
  ##  [3] "code_state"                    "abbrev_state"
  ##  [5] "ano"                           "mortalidade_1"
  ##  [7] "mortalidade_5"                 "taxa_freq_liquida_pre"
  ##  [9] "indice_gini"                   "prop_pobreza_extrema_criancas"
  ## [11] "prop_pobreza_criancas"         "prop_vulner_pobreza_criancas"
  ## [13] "taxa_agua_esgoto_inadequados"  "taxa_criancas_fora_escola_4_5"
  ## [15] "nome_regiao"                   "geom"


# mortalidade (até 1 ano)

muni_dados %>%
  filter(ano == 2010) %>%
  ggplot() +
  aes(geometry = geom, fill = mortalidade_1) +
  geom_sf(color = NA) +
  scale_fill_viridis_c(na.value = "grey90")

# pobreza extrema crianças

muni_dados %>%
  filter(ano == 2010) %>%
  ggplot() +
  aes(geometry = geom, fill = prop_pobreza_extrema_criancas) +
  geom_sf(color = NA) +
  scale_fill_viridis_c(na.value = "grey90")

# tx. crianças fora da escola

muni_dados %>%
  filter(ano == 2010) %>%
  ggplot() +
  aes(geometry = geom, fill = taxa_criancas_fora_escola_4_5) +
  geom_sf(color = NA) +
  scale_fill_viridis_c(na.value = "grey90")

# tx. água e esgoto inadequados

muni_dados %>%
  filter(ano == 2010) %>%
  ggplot() +
  aes(geometry = geom, fill = taxa_agua_esgoto_inadequados) +
  geom_sf(color = NA) +
  scale_fill_viridis_c(na.value = "grey90")



# PACOTE WOOLDRIDGE -------------------------------------------------------

# Este pacote contém a documentação completa para cada conjunto
# e todos os dados utilizados no livro Introdução à Econometria
# de Jeffrey M. Wooldridge

### importa dados

wage <- wooldridge::wage1

glimpse(wage)


# A relação entre educação e salário

# regressão

mod <- lm(wage ~ educ, data = wage)

summary(mod)

# A relação entre os anos de estudo (educ) e o salário médio
# por hora (wage) é positivamente inclinada. O parâmetro de
# educ é estatisticamente significativo. E o R² é igual a 0.1648,
# isto é, 16.48% da equação é explicada pela variável independente (educ).

# EQ. REGRESSÃO: wage = − 0.905 + 0.541 educ

# A cada um 1 ano que aumenta a educação (educ),
# o salário do indivíduo aumenta (em média) 0.54 doláres.

# gráfico

wage %>%
  ggplot() +
  aes(x = educ, y = wage) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE)


# A relação entre educação e log-salário

# regressão

mod1 <- lm(lwage ~ educ, data = wage)

summary(mod1)

# A relação entre os anos de estudo (educ) e o log-salário é
# positivamente inclinado. A variável de educação (educ) é significativa
# estatisticamente, uma vez que possui significância estatística muito
# próxima à zero. E o R² é igual a 0.1858, ou seja, 18.58% da equação é
# explicada pela variável independente (educ).

# EQ. REGRESSÃO: lwage = 0.584 + 0.083 educ

# A cada um (1) ano que aumenta a educação (educ),
# o salário do indivíduo aumenta em 8.30% (β • 100).

# gráfico

ggplot(wage) +
  aes(x = educ, y = lwage) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)


# Mincer: o salário é estimado por meio da sua experiência e educação

# regressão

mincer <- lm(lwage ~ educ + exper, data = wage)

summary(mincer)

# Os anos de estudo e de experiência possuem relação positiva com o
# log-salário. Ambas variáveis independentes, educ e exper têm significância
# estatística muito próxima à zero. Cerca de 24.93% da equação é explicada
# pelo modelo, uma vez que o R² é igual a 0.2493.

# EQ. REGRESSÃO: lwage = 0.217 + 0.098 educ + 0.010 exper

# O aumento de 1 ano de estudo (educ) gera um aumento de,
# aproximadamente, 9.79% do salário, ceteris paribus.

# O aumento de 1 ano de experiêcia (exper) resulta em um
# aumento de 1.03% do salário, ceteris paribus.

# gráfico

ggplot(wage) +
  aes(x = educ, y = lwage) +
  geom_point(aes(color = exper), alpha = 0.7) +
  scale_color_viridis_c() +
  geom_smooth(method = "lm", se = FALSE)


# Mincer: Relação entre salário, educação e experiência ao longo da vida

# regressão

mincer2 <- lm(lwage ~ educ + exper + expersq, data = wage)

summary(mincer2)

# EQ. REGRESSÃO: lwage = 0.128 + 0.090 educ + 0.041 exper − 0.0007 expersq

# Os anos de estudo (educ) e de experiência (exper) possuem relação
# positiva com o log-salário. Entretanto, como é esperado, ao longo do tempo,
# conforme o indivíduo envelhece e a sua experiência (expersq) aumenta,
# o log-salário (lwage) aumenta até atingir um certo ponto e, depois,
# começa a diminuir. Todos os parâmetros são estatisticamente significantes,
# sendo muito próximos a zero. O Teste F tem significância estatística
# (tem p-valor próximo a zero), indicando que os parâmetros são conjuntamente
# significativos. O R² é igual a 0.3003, indicando que a equação é explicada
# por 30.03% das variáveis explicativas.

# gráfico

ggplot(wage) +
  aes(x = educ, y = lwage) +
  geom_point(aes(color = exper, size = expersq), alpha = 0.7) +
  scale_color_viridis_c() +
  geom_smooth(method = "lm", se = FALSE)

