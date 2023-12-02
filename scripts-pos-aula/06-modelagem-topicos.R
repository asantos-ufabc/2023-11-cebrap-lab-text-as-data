# instalar pacotes:
# install.packages("topicmodels")
# install.packages("tidytext")
# https://www.tidytextmining.com/topicmodeling
library(tidyverse)
library(tidytext)

# ler os tokens
tokens <- readRDS("dados/tokens_preparados.rds")

# nesse caso, estamos considerando o posicionamento como o documento
# e o termo, o stem

# preparando a document term matrix
document_term_matrix <- tokens |>
  dplyr::count(posicionamento, stem) |>
  tidytext::cast_dtm(term = stem,
                     document = posicionamento,
                     value = n)

# <<DocumentTermMatrix (documents: 2, terms: 2445)>>
# Non-/sparse entries: 3359/1531
# Sparsity           : 31%
# Maximal term length: 21
# Weighting          : term frequency (tf)


agrupamentos <- topicmodels::LDA(document_term_matrix,
                                 k = 3,
                                 control = list(seed = 1234))

# A LDA_VEM topic model with 2 topics.


# broom::tidy() - limpar resultados de modelos


# beta: method for extracting the per-topic-per-word probabilities
topicos_tidy <- tidytext::tidy(agrupamentos, matrix = "beta")

termos_topicos <- topicos_tidy |>
  group_by(topic) |> 
  slice_max(beta, n = 10) |> 
  ungroup() |> 
  arrange(topic, -beta)


termos_topicos |> 
  mutate(term = reorder_within(term, beta, topic)) |>
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

  
