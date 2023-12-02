library(tidyverse)
library(tidytext)
library(stopwords)

resultados_enquete <- read_rds("dados-brutos/resultados_enquete.rds")

# Vamos usar o pacote tidytext para fazer a tokenização
# a função é a mesma que usavamos antes, 
# mas agora vamos informar os argumentos
# token e n (número de palavras por token)
tokens_bigram <- resultados_enquete  |> 
  unnest_tokens(output = bigram, input = conteudo, token = "ngrams", n = 2) 

# preparando os dados

tokens_bigram_separados <- tokens_bigram |> 
    separate(bigram, c("palavra1", "palavra2"), sep = " ", remove = FALSE) 


# Removendo stopwords ------------------------
# Carregando as stopwords em português
source("scripts/stop-words.R")

# Removendo stopwords e números
tokens_sem_stopwords <- tokens_bigram_separados |> 
  filter(!palavra1 %in% stop_words_completo) |> 
  filter(!palavra2 %in% stop_words_completo) |> 
  filter(!str_detect(bigram, "\\d")) |> 
  mutate(bigrama_sem_acentos = rm_accent(bigram))

tokens_sem_stopwords |> 
  count(bigram, posicionamento, sort = TRUE) 


# tá mais interessante sem acentos
tokens_sem_stopwords |> 
  count(bigrama_sem_acentos, posicionamento, sort = TRUE) 



tokens_sem_stopwords |> 
  filter(posicionamento == "negativo") |> 
  count(bigrama_sem_acentos, sort = TRUE) |> 
  mutate(bigrama_sem_acentos = forcats::fct_reorder(bigrama_sem_acentos, n)) |> 
  slice(1:10) |> 
  ggplot() +
  geom_col(
    aes(x = n, y = bigrama_sem_acentos)
  )

tokens_sem_stopwords |> 
  filter(posicionamento == "positivo") |> 
  count(bigrama_sem_acentos, sort = TRUE) |> 
  mutate(bigrama_sem_acentos = forcats::fct_reorder(bigrama_sem_acentos, n)) |> 
  slice(1:10) |> 
  ggplot() +
  geom_col(
    aes(x = n, y = bigrama_sem_acentos)
  )
  

# Sexta-feira
# Visualizando o tf-idf ------------------------
# https://www.tidytextmining.com/tfidf

# tf = term frequency - frequência que o termo aparece no documento
# termo nesse caso seria o token!
# idf = inverse document frequency - frequência inversa do documento


# Existem palavras que aparecem bastante mas não são relevantes
# o idf diminui o peso dessas palavras,
# e aumenta o peso das palavras que aparecem pouco em um conjunto de texto.


# tf-idf - frequencia do termo, ajusta a quao "rara" é utilizada

# tf-idf = o objetivo é medir o quão importante é uma palavra 
# para um documento em um conjunto de documentos


# Calculando o tf-idf por posicionamento
tf_idf <- tokens_sem_stopwords |> 
  # contagem de ocorrencia do bigrama pelo posicionamento
  count(bigram, posicionamento, sort = TRUE) |> 
  # calculando o tf-idf e adicionando as colunas na base
  bind_tf_idf(bigram, posicionamento, n) |> 
  # ordenando de forma decrescente
  arrange(desc(tf_idf))

# Visualizando os resultados
tf_idf |>
  group_by(posicionamento) |>
  # considerando que ja ta ordenado
  # slice(1:10)
  slice_max(tf_idf, n = 10, with_ties = FALSE) |>
  mutate(bigram = reorder(bigram, tf_idf)) |>
  ggplot() +
  aes(x = tf_idf, y = bigram) +
  geom_col() +
  facet_wrap( ~ posicionamento, scales = "free")
  