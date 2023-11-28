# Carragando os pacotes
library(tidyverse)
library(tidytext)
library(stopwords)
# devtools::install_github("dfalbel/ptstem")
library(ptstem)

# se estiver com problemas para instalar,
# use o pak:
# install.packages("pak")
# pak::pkg_install("dfalbel/ptstem")

# importando os dados
resultados_enquete_raw <- read_rds("dados-brutos/resultados_enquete.rds")

# Conhecendo um pouco a base
resultados_enquete <- resultados_enquete_raw |>
  # separar a coluna data em data e hora
  separate(data, into = c("data", "hora"), sep = " ") |> 
  # converter a data que está em texto na classe data
  mutate(data = lubridate::dmy(data)) |> 
  # gerar um id para o comentário
  rowid_to_column("id_comentario")

# espiando os dados
glimpse(resultados_enquete)


# quantidade de comentários
# cada linha é um comentário
nrow(resultados_enquete)

# quantidade de curtidas nos comentários
sum(resultados_enquete$qtd_curtidas)

# quantidade de curtidas por posicionamento
resultados_enquete |> 
  group_by(posicionamento) |> 
  summarise(total_curtidas = sum(qtd_curtidas)) |> 
  mutate(porc_curtidas = total_curtidas / sum(total_curtidas),
         porc_curtidas = scales::percent(porc_curtidas)) 

resultados_enquete |> 
  count(data, posicionamento, sort = TRUE) |> 
  ggplot() +
  aes(x = data, y = n) +
  geom_line(aes(color = posicionamento)) 

# Vamos começar a explorar o texto dos comentários ---------------------------

# Vamos usar o pacote tidytext para fazer a tokenização
# token sendo uma palavra
tokens_enquete <- resultados_enquete  |> 
  unnest_tokens(output = palavra, input = conteudo) 

# Palavras mais frequentes

tokens_enquete |> 
  count(palavra, sort = TRUE) 

# tem muitas palavras que não são relevantes para a análise
# chamamos de STOP WORDS!

# Removendo stopwords ------------------------

# Ir para arquivo : scripts/stop-words.R

source("scripts/stop-words.R")


tokens_sem_stopwords <- tokens_enquete |> 
  filter(!palavra %in% stop_words_completo) |> 
  # Removendo números
  filter(!str_detect(palavra, "[0-9]"))

# Pausa para o str_detect

exemplo <- resultados_enquete$conteudo[1:2]

# view ajuda a visualizar o padrão
str_view(exemplo, pattern = "impostos")

# saber se o padrão é encontrado no texto
str_detect(exemplo, pattern = "impostos")

# extrair padrões encontrados
str_extract_all(exemplo, pattern = "impostos")


# Regex - regular expression / expressões regulares
# Números
str_view(exemplo, pattern = "[0-9]")

str_view(exemplo, pattern = "revendem.*caro")



# Palavras mais frequentes sem stopwords

tokens_sem_stopwords |> 
  count(palavra, sort = TRUE) |> View()

# Problemas:
# - palavras com a mesma raiz são contadas separadamente
# ex: "votar", "votou", "votando", "votaram"
# - palavras singular/plural são contadas separadamente
# ex: imposto, impostos

# Erros de português
tokens_sem_stopwords |> 
  mutate(
    palavras2 = case_when(
      palavra == "abussivos" ~ "abusivos",
      .default = palavra
    )
  ) |> View()


# Stemming ------------------------

# Stemming é o processo de reduzir palavras flexionadas 
# (ou às vezes derivadas) ao seu tronco (stem), base ou raiz,
# geralmente uma forma da palavra escrita.

# Vamos usar o pacote ptstem para fazer o stemming
# É um processo que pode demorar
# Vamos buscar os tokens únicos, e depois unir com os dados

tokens_arrumados <- tokens_sem_stopwords

length(tokens_arrumados$palavra)
length(unique(tokens_arrumados$palavra))

stems <- tokens_arrumados |> 
  distinct(palavra) |> 
  mutate(stem = ptstem::ptstem(palavra))

tokens_stem <- tokens_arrumados |>
  left_join(stems, by = "palavra")

tokens_stem |> 
  count(stem, sort = TRUE) 

rm_accent <- function(x){
    stringi::stri_trans_general(x, "Latin-ASCII")
}


tokens_final <- tokens_stem |> 
  mutate(palavra_limpo = rm_accent(palavra),
         stem_limpo = rm_accent(stem)) 

fs::dir_create("dados")


# Agora sim!
tokens_final |> 
  write_rds("dados/tokens_preparados.rds")
