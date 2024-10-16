

# Pacotes necessários -----------------------------------------------------

library(tidyverse)


# Importando as bases -----------------------------------------------------

pinguins <- read_csv("aula-r/dados/pinguins/pinguins.csv")
pinguins_raw <- read_csv("aula-r/dados/pinguins/pinguins_raw.csv")

head(pinguins)
head(pinguins_raw)

# Pacote dplyr ------------------------------------------------------------

# SELECT()
# seleciona colunas

# seleciona uma coluna
select(pinguins, species)

# seleciona mais de uma coluna
select(pinguins, species, sex, body_mass_g)

# seleciona colunas consecutivas
select(pinguins, island:body_mass_g)


# Conjunto de funções auxiliares úteis para a seleção de colunas.

# starts_with(): para colunas que começam com um texto padrão
# ends_with(): para colunas que terminam com um texto padrão
# contains(): para colunas que contêm um texto padrão

# seleciona todas as colunas que terminam com "bico"
select(pinguins, ends_with("mm"))

# exclui as colunas ano e ensino
select(pinguins, -year)
select(pinguins, -c(year, sex))


# ARRANGE()
# ordena as linhas

# ordena as linhas em ordem crescente
arrange(pinguins, body_mass_g)

# ordena as linhas em ordem decrescente
arrange(pinguins, desc(body_mass_g))

# ordena as linhas de mais de uma coluna
# neste caso, uma em ordem descrescente e outra em ordem crescente
arrange(pinguins, desc(bill_length_mm), bill_depth_mm)


##################################################################
# sem pipe
arrange(select(pinguins, species, body_mass_g), body_mass_g)

# com pipe - LEITURA MAIS INTUITIVA
pinguins %>%
  select(species, body_mass_g) %>%
  arrange(body_mass_g)
##################################################################

# FILTER()
# filtra os valores (linhas) de uma coluna

# filtra as linhas da coluna comprimento_nadadeira acima de 140 mm
filter(pinguins, flipper_length_mm > 140)

# seleciona apenas as colunas da espécie e comprimento_nadadeira
# e filtra as nadadeiras com mais de 140 mm
pinguins %>%
  select(species, flipper_length_mm) %>%
  filter(flipper_length_mm > 140)

# estende o filtro para mais de uma coluna
# cada operação é separada por uma vírgula
pinguins %>%
  filter(island == "Torgersen", bill_depth_mm > 15)

# é possível realizar operações com as colunas dentro da função filter()
pinguins %>%
  filter(body_mass_g > mean(body_mass_g, na.rm = TRUE))



# MUTATE()
# cria novas colunas ou
# modifica colunas já existentes

# modifica uma coluna já existente
pinguins %>%
  mutate(bill_length_mm = bill_length_mm/10)

# cria uma coluna (variável) nova
pinguins %>%
  mutate(bill_length_cm = bill_length_mm/10)

# é possível criar/modificar mais de uma coluna
#dentro de um mesmo mutate()
pinguins %>%
  mutate(
    mean_bill_length_mm = mean(bill_length_mm, na.rm = TRUE),
    country = "Antarctica"
  ) %>%
  select(species, mean_bill_length_mm, country)



# SUMMARISE() ou SUMMARIZE()
# resume um conjunto de dados

# resume a coluna ideb pela sua média
pinguins %>%
  summarise(mean_body_mass_g = mean(body_mass_g, na.rm = TRUE))


# várias sumarizações podem ser realizadas na mesma função
# cada sumarização será uma coluna
pinguins %>%
  summarise(
    mean_flipper_length_mm = mean(flipper_length_mm, na.rm = TRUE),
    median_flipper_length_mm = median(flipper_length_mm, na.rm = TRUE),
    var_flipper_length_mm = var(flipper_length_mm, na.rm = TRUE)
  )


# é possível sumarizar várias colunas
pinguins %>%
  summarise(
    mean_flipper_length_mm = mean(flipper_length_mm, na.rm = TRUE),
    mean_bill_length_mm = mean(bill_length_mm, na.rm = TRUE),
    mean_bill_depth_mm = median(bill_depth_mm, na.rm = TRUE)
  )


# GROUP_BY() + SUMMARISE()
# sumariza uma coluna agrupada por
# categorias de outra coluna

# calcula a média do ideb para cada categoria de ensino
pinguins %>%
  group_by(island) %>%
  summarise(
    mean_body_mass_g = mean(body_mass_g, na.rm = TRUE)
  ) %>%
  arrange(desc(mean_body_mass_g))


# a única mudança que a função group_by() faz na base
# é a marcação de que a base está agrupada
pinguins %>%
  group_by(island)


# RELOCATE()
# reordena as colunas por nome ou posição
pinguins %>%
  relocate(sex, year, .after = island)


# RENAME()
# renomeia as colunas de uma tabela
pinguins %>%
  rename(
    comprimento_bico_mm = bill_length_mm,
    profundidade_bico_mm = bill_depth_mm,
    comprimento_nadadeira_mm = flipper_length_mm,
    massa_corporal_g = body_mass_g
  )


# SLICE_HEAD()
# seleciona as primeiras linhas da tabela
pinguins %>%
  slice_head(n = 8)



# SLICE_MAX()
# seleciona linhas por valores de uma coluna
pinguins %>%
  slice_max(bill_depth_mm, n = 5)



# SLICE_SAMPLE()
# seleciona linhas aleatoriamente
pinguins %>%
  slice_sample(n = 12)


# DISTINCT()
# retira as linhas com valores duplicados
pinguins %>%
  distinct(body_mass_g, .keep_all = TRUE)


# COUNT()
# conta valores de uma coluna
pinguins %>%
  count(species)

# conta valores de mais de uma coluna
pinguins %>%
  count(species, island)


# BIND_ROWS()
# combina dados por linhas

# selecionar as linhas para dois tibbles
pinguins_01 <- slice(pinguins, 1:5) %>% select(1:3)
pinguins_02 <- slice(pinguins, 51:55) %>% select(4:6)

# combinar as linhas
bind_rows(pinguins_01, pinguins_02, .id = "id")


# BIND_COLS()
#combina dados por colunas

# selecionar as linhas para dois tibbles
pinguins_01 <- slice(pinguins, 1:5)
pinguins_02 <- slice(pinguins, 51:55)

## combinar as colunas
bind_cols(pinguins_01, pinguins_02, .name_repair = "unique")


# *_JOIN()
# combina pares de dados tabulares por uma ou mais chaves

####################################################################################################
# left_join(x, y): mantém as observações em x
# right_join(x, y): mantém as observaçõesm em y
# inner_join(x, y): mantém apenas as observações em x e em y (remove NAs)
# full_join(x, y): mantém todas as observações em x e em y (adiciona NAs onde não tem informações)
####################################################################################################


# LEFT_JOIN()

# cria tabela com coordenadas das ilhas
## coordenadas
pinguins_ilhas <- tibble(
  ilha = c("Torgersen", "Biscoe", "Dream", "Alpha"),
  longitude = c(-64.083333, -63.775636, -64.233333, -63),
  latitude = c(-64.766667, -64.818569, -64.733333, -64.316667))

pinguins_ilhas

# une as duas tabelas pela chave "ilha"
pinguins_left_join <- left_join(
  pinguins, pinguins_ilhas, by = c("island" = "ilha")
)

glimpse(pinguins_left_join)



# Pacote tidyr ------------------------------------------------------------

# O pacote `tidyr` tem a função de tornar um conjunto de dados tidy (organizados),
# sendo esses fáceis de manipular, modelar e visualizar.

# - cada coluna é uma variável;
# - cada linha é uma observação;
# - cada célula é um único valor.


########################################################################
# As principais funções do tidyr são:
# unite(): junta dados de múltiplas colunas em uma
# separate(): separa caracteres em múlplica colunas
# separate_rows(): separa caracteres em múlplica colunas e linhas
# drop_na(): retira linhas com NA
# replace_na(): substitui NA
# pivot_wider(): long para wide
# pivot_longer(): wide para long
########################################################################


# UNITE()
# une duas colunas em uma única coluna
especie_ilha <- pinguins %>%
  unite(
    col = "especie_ilha",
    species:island,
    sep = " - "
  )

head(especie_ilha)


# SEPARATE()
# separa uma coluna em duas ou mais colunas
especie_ilha %>%
  separate(
    col = especie_ilha,
    into = c("especie", "ilha"),
    sep = " - ",
  )


# DROP_NA()
# remove as linhas que contém NA de todas as colunas
pinguins %>%
  drop_na()


# PIVOT_LONGER()
# o banco de dados se torna mais longo (longer)
# em relação aos dados originais.

# cols: colunas para girar em formato mais longo
# names_to: nome da coluna que receberá os nomes
# values_to: nome da coluna que receberá os valores

pivot_longer(
  data = pinguins_raw[, c(2, 3, 10:13)],
  cols = `Culmen Length (mm)`:`Body Mass (g)`,
  names_to = "medidas",
  values_to = "valores"
)


# PIVOT_WIDER()
# o banco de dados se torna mais largo (wider)
# em relação aos dados originais.

# names_from: variável categórica que definirá os nomes das colunas
# values_from: variável numérica que preencherá os dados
# values_fill: valor para preencher os NAs

pivot_wider(
  data = pinguins_raw[, c(2, 3, 13)],
  names_from = Species,
  values_from = `Body Mass (g)`
)

