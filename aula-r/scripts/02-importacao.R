
library(tidyverse)


# Caminhos até o arquivo --------------------------------------------------

# Caminhos absolutos - NÃO É UMA BOA PRÁTICA!
"G:/Meu Drive/Repositorio/ufsm-monitoria-curso-r/dados/pinguins/pinguins.csv"

# Caminhos Relativos
"dados/pinguins/pinguins.csv"

# DICA: navegação entre as aspas clicando TAB

"./" # relativo
"../" # sobe um nível nas pastas - relativo
"/" # raiz do computador


# Tibbles -----------------------------------------------------------------

mtcars

class(mtcars)

as_tibble(mtcars) # coerção de data frame para tibble

class(as_tibble(mtcars))

tibble::as_tibble(mtcars, rownames = "cars")

----

airquality

class(airquality)

as_tibble(airquality) # coerção de data frame para tibble

class(as_tibble(airquality))


# Importação de arquivos (dados) ------------------------------------------

# Lendo arquivos de texto:

# library(readr)
# read_csv()
# read_csv2()
# read_delim()
# read_log()
# read_rds()

# .csv - separado por vírgula
pinguins_csv <- read_csv("aula-r/dados/pinguins/pinguins.csv")

pinguins_csv

################################################################################
# No Brasil, as vírgulas são usadas para separar as casas decimais dos números

# OBSERVAÇÃO:
# R$ 1000,00 # como usamos no Brasil
# U$ 1000.00 # como o R usa
################################################################################

# Portanto, alguns .csv utilizam ; na separação entre colunas
# .csv - separado por ponto-e-vírgula
pinguins_csv2 <- read_csv2("aula-r/dados/pinguins/pinguins2.csv")


# .txt - separado por tabulação (tecla TAB)
pinguins_txt <- read_delim("aula-r/dados/pinguins/pinguins.txt", delim = "\t")

# a função read_delim() funciona para qualquer tipo de separador:
pinguins_delim <- read_delim("aula-r/dados/pinguins/pinguins.csv", delim = ",")
pinguins_delim2 <- read_delim("aula-r/dados/pinguins/pinguins2.csv", delim = ";")

# importando dados direto da internet
titanic_csv_url <- read_csv(
  "https://raw.githubusercontent.com/beatrizmilz/2020-R-Ladies-SP-Basico/master/docs/data/titanic.csv"
)


# Interface point and click do RStudio também é útil!


imdb2 <- read_delim("aula-r/dados/imdb2.csv", delim = ";",
                    escape_double = FALSE, trim_ws = TRUE)
View(imdb2)

"aula-r/dados/imdb2.csv"

# Lendo arquivos do Excel (.xlsx ou .xls) ---------------------------------

library(readxl)

pinguins_xlsx <- read_excel("aula-r/dados/pinguins/pinguins.xlsx")

################################################################################
# Argumentos úteis:
# sheet = define em qual aba estão os dados
# col_names = indica se a primeira linha representa o nome das colunas
# col_types = define a classe das colunas
# skip = pular linhas
# NA = indica quais strings devem ser interpretadas como NA
################################################################################


# Exportação de arquivos  -------------------------------------------------

# Para cada função read_*() existe uma função write_*().
# É preciso passar para elas o objeto que se quer gravar
# e o caminho/nome do arquivo que será criado
# (o nome do arquivo deve conter a extensão).

# São funções que iniciam com 'write'

# .csv - vírgula
write_csv(pinguins, "aula-r/dados/pinguins/pinguins.csv")

# .csv - ponto-e-vírgula
write_csv2(pinguins, "aula-r/dados/pinguins/pinguins2.csv")

# .txt - tabulação
write_delim(pinguins, "aula-r/dados/pinguins/pinguins.txt", delim = "\t")

# .xlsx ou .xls - excel
writexl::write_xlsx(pinguins, "aula-r/dados/pinguins/pinguins.xlsx")



# Para criar uma pasta
dir.create("dados-saida") # criar a pasta dados-saidas


# A extensão .rds ---------------------------------------------------------

# Representa uma estrutura binária de arquivos.
# Pode ser compactada para gerar arquivos menores.
# É possível salvar qualquer objeto do R em formato .rds


write_rds(pinguins, "dados/pinguins.rds") # escrevendo SEM compactação

write_rds(pinguins, "dados/pinguins.rds", compress = "gz") # escrevendo COM compactação

read_rds("dados/pinguins.rds") # lendo a base

pinguins_rds <- read_rds("dados/pinguins.rds") # importanto a base



# Visualização de um objeto -----------------------------------------------

# Para visualizar um objeto em aba separada
View(pinguins_csv)

# Para visualizar um objeto no Console
dplyr::glimpse(pinguins_csv)


# BASE DOS DADOS ----------------------------------------------------------

# Organização não-governamental, sem fins lucrativos e open source
# que atua para universalizar o acesso a dados de qualidade
# Repositório com dados brasileiros já tratados, limpos e padronizados.
# para todo usuário é disponibilizado 1 TB gratuito por mês para consulta de dados.

# instala o pacote
install.packages("basedosdados")

# carrega o pacote
library(basedosdados)

# é necessário identificar o projeto sempre que iniciar uma nova sessão no R
set_billing_id("id-do-projeto")

################################################################################
# PRINCIPAIS PALAVRAS-CHAVE:

# SELECT: seleciona as colunas (é o principal comando usado no SQL)
# FROM: especifica a tabela utilizada para selecionar os dados
# WHERE: especifica as condições/filtros que devem reunir os dados selecionados
# GROUP BY: separa os dados selecionados em grupos específicos
# ORDER BY: ordena os dados selecionados em uma ordem específica
# AND - E: avalia as condições e devolve um valor verdadeiro caso ambos sejam corretos
# OR - OU: avalia as condições e devolve um valor verdadeiro se algum for correto
# AVG: calcula a média dos valores de um campo determinado
# SUM: devolve a soma de todos os valores de um campo determinado
# < - MENOR
# > - MAIOR
# <= - MENOR OU IGUAL
# >= - MAIOR OU IGUAL
# = - IGUAL
# <> - DIFERENTE
# BETWEEN: especifica valores dentro de um intervalo fechado
# AS: define um "apelido" para a coluna e/ou para a tabela
# LEFT JOIN:une duas tabelas em uma mesma base de dados.
# ON: define a "chave" que liga as duas tabelas
################################################################################


"SELECT id_municipio, nome_regiao
FROM `basedosdados.br_bd_diretorios_brasil.municipio`"


# Para selecionar todas as colunas de uma tabela: SELECT *

"SELECT *
FROM `basedosdados.br_bd_diretorios_brasil.municipio`"

# Filtra os dados (importante pra tabelas grandes e dados desnecessários)

"SELECT id_municipio, ano, AVG(taxa_aprovacao) as tx_aprov,
ensino, AVG(ideb) as ideb
FROM `basedosdados.br_inep_ideb.municipio`
WHERE ano = 2021
GROUP BY id_municipio, ano, ensino"

# JOIN LEFT une duas tabelas em apenas uma

"SELECT pib.id_municipio, pib.ano, pop.id_municipio, pop.populacao
FROM `basedosdados.br_ibge_pib.municipio` AS pib
LEFT JOIN `basedosdados.br_ibge_populacao.municipio` AS pop
ON pib.id_municipio = pop.id_municipio"

# Para que o R consiga importar a base de dados desejada,
# é necessário incluir a função read_sql() ao final da query.

# Para isso, há duas formas:

# 1. criando um objeto intermediário (obj_intermediario) 

obj_intermediario <- "SELECT id_municipio, nome_regiao
FROM `basedosdados.br_bd_diretorios_brasil.municipio`"

base_final <- read_sql(obj_intermediario)


# 2. utilizando o pipe (%>%) e criando apenas o objeto final

base_final <- "SELECT id_municipio, nome_regiao
FROM `basedosdados.br_bd_diretorios_brasil.municipio`" |> 
  read_sql()