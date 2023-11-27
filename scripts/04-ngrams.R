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
   filter(!is.na(bigram)) |> 
    separate(bigram, c("palavra1", "palavra2"), sep = " ", remove = FALSE) 


# Removendo stopwords ------------------------

snowball <- stopwords(source = "snowball", language = "pt")
extra_stop_words <- c("é",
                      "ser",
                      "pra",
                      "vai",
                      "portanto",
                      "quer",
                      "querer",
                      "quase",
                      "pois",
                      "algo",
                      "assim",
                      "ai",
                      "ainda",
                      "algum",
                      "sendo",
                      "existe",
                      "disso",
                      "deveria",
                      "deveriam",
                      "disso"
                      )

stop_words_completo <- c(snowball, extra_stop_words)


tokens_sem_stopwords <- tokens_bigram_separados |> 
  filter(!palavra1 %in% stop_words_completo) |> 
  filter(!palavra2 %in% stop_words_completo) |> 
  filter(!str_detect(bigram, "[0-9]")) 

tokens_sem_stopwords |> 
  count(bigram, posicionamento, sort = TRUE) 

# Visualizando o tf-idf ------------------------

tf_idf <- tokens_sem_stopwords |> 
  count(bigram, posicionamento, sort = TRUE)   |> 
  bind_tf_idf(bigram, posicionamento, n) |> 
  arrange(desc(tf_idf))

tf_idf |>
  group_by(posicionamento) |> 
  slice_max(tf_idf, n = 10, with_ties = FALSE) |> 
  mutate(bigram = reorder(bigram, tf_idf)) |>
  ggplot() +
  aes(x = tf_idf, y = bigram) +
  geom_col() +
  facet_wrap(~posicionamento, scales = "free")
  