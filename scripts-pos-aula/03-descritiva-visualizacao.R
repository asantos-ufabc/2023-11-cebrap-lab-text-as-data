library(tidyverse)
library(wordcloud2)

tokens <- read_rds("dados/tokens_preparados.rds")

tokens |> 
  count(stem, sort = TRUE)

mais_frequentes <- tokens |> 
  # a função count permite contar linhas considerando mais que uma categoria
  count(posicionamento, stem, sort = TRUE) 

mais_frequentes_positivo <- mais_frequentes |> 
  filter(posicionamento == "positivo") |> 
  slice_head(n = 10)

mais_frequentes_negativo <- mais_frequentes |> 
  filter(posicionamento == "negativo") |> 
  slice_head(n = 10)


mais_frequentes_filtrado <- mais_frequentes_positivo |> 
  bind_rows(mais_frequentes_negativo)

# 
# # 10 palavras mais frequentes por posicionamento
# mais_frequentes <- tokens |>
#   count(posicionamento, stem, sort = TRUE)
# 
# stems_mais_frequentes_tbl <- mais_frequentes |> 
#   group_by(posicionamento) |> 
#   slice_max(order_by = n, n = 15) |>
#   ungroup() 
# 
# 
# stems_mais_frequentes <- stems_mais_frequentes_tbl |> 
#   distinct(stem) |> 
#   pull(stem)
# 
# # seria interessante saber quais são essas palavras que são mais
# # frequentes nos dois posicionamentos
# janitor::get_dupes(stems_mais_frequentes_tbl, stem)
# 


mais_frequentes_filtrado |> 
  mutate(posicionamento = str_to_title(posicionamento),
         stem = str_to_title(stem),
         stem = tidytext::reorder_within(stem, n, within = posicionamento)) |> 
  ggplot() +
  geom_col(aes(x = n, y = stem, fill = posicionamento), show.legend = FALSE ) +
  facet_wrap(~posicionamento, scales = "free") +
  theme_minimal() +
  scale_fill_viridis_d() +
  labs(
    x = "Frequencia de ocorrência",
    y = "Stem"
  ) +
  tidytext::scale_y_reordered() # Usar essa função para não ficar com o nome do grupo duplicado

# Gráfico
# mais_frequentes |>
#   filter(stem %in% stems_mais_frequentes) |>
#   mutate(stem = fct_reorder(stem, n)) |>
#   ggplot() +
#   aes(y = stem, x = n) +
#   geom_col(aes(fill = posicionamento)) 





# Nuvem de palavras por posicionamento

library(wordcloud2)

stems_por_posicionamento <- tokens |>
  group_by(stem, posicionamento) |>
  summarise(freq = n(),
            soma_curtidas = sum(qtd_curtidas)) |> 
  ungroup() |> 
  rename(word = stem) 


stems_por_posicionamento |>
  filter(posicionamento == "positivo") |> 
  arrange(desc(freq)) |> 
  slice(1:50) |> 
  select(-posicionamento) |> 
  wordcloud2()


stems_por_posicionamento |>
  filter(posicionamento == "negativo") |> 
  arrange(desc(freq)) |> 
  slice(1:50) |> 
  select(-posicionamento) |> 
  wordcloud2()

# Considerando o número de curtidas


stems_por_posicionamento |>
  filter(posicionamento == "positivo") |> 
  select(word, freq = soma_curtidas) |> 
  arrange(desc(freq)) |> 
  slice(1:50) |> 
  wordcloud2()


stems_por_posicionamento |>
  filter(posicionamento == "negativo") |> 
  select(word, freq = soma_curtidas) |> 
  arrange(desc(freq)) |> 
  slice(1:50) |> 
  wordcloud2()
