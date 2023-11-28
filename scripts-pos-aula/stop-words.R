# Sources que tem pt: "snowball", "stopwords-iso", "nltk"

# Quais são as fontes de conjuntos de stop words disponíveis
# stopwords::stopwords_getsources()
# [1] "snowball"      "stopwords-iso" "misc"          "smart"        
# [5] "marimo"        "ancient"       "nltk"          "perseus"     

# stopwords::stopwords_getlanguages("smart")

# Vamos usar o snowball
snowball <-
  stopwords::stopwords(source = "snowball", language = "pt")

# snowball
# 
# tokens_enquete |> 
#   filter(!palavra %in% snowball) |> 
#   count(palavra, sort = TRUE)



# Vamos adicionar algumas palavras que não estão no snowball
# Mas que olhando os resultados, aparecem muito
# e não são relevantes para a análise
extra_stop_words <- c(
  "é",
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
  "disso",
  "á"
)

stop_words_completo <- c(snowball, extra_stop_words)
