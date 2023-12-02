# Importar dados textuais podem ser um desafio!
# Nesse exemplo, queremos chegar até o token, 
# que é a menor unidade de texto que faz sentido para a análise.
# A partir do token, podemos usar os conceitos usados nas aulas anteriores.

# Pacote readtext é útil
# https://cran.r-project.org/web/packages/readtext/vignettes/readtext_vignette.html

# Importar texto de sites: técnicas de web scraping, como 
# rvest::html_text() (mais avançado)

library(readtext)
arquivo_pdf <- readtext::readtext("referencias/1859-Texto do artigo-3971-2-10-20210608.pdf")


fs::dir_create("textos")

download.file(
  "https://sigrh.sp.gov.br/public/uploads/records//CBH-PCJ/24978/ata_29-ord-comitespcj_30-03-23.pdf",
  destfile = "textos/ata_29-ord-comitespcj_30-03-23.pdf"
)

download.file(
  "https://sigrh.sp.gov.br/public/uploads/records//CBH-PCJ/23767/ata_28-reuniaoord-comitespcj_07-12-22.pdf",
  destfile = "textos/ata_28-reuniaoord-comitespcj_07-12-22.pdf"
)

download.file(
  "https://sigrh.sp.gov.br/public/uploads/records//CBH-PCJ/23435/ata_27-extra-comitespcj_04-10-22.pdf",
  destfile = "textos/ata_27-extra-comitespcj_04-10-22.pdf"
)


atas <- readtext("textos/") 

atas_preparando <- atas |> 
  mutate(data_texto = stringr::str_extract(doc_id, "[0-9]{2}-[0-9]{2}-[0-9]{2}"),
         data = lubridate::dmy(data_texto),
         text = str_squish(text)) 

atas_preparando |> 
  unnest_tokens(input = text, output = "word") |> 
  count(word, sort = TRUE) 


# -----------

# tentando com o pdftools

library(pdftools)

extrair_textos_pdf <- function(arquivo) {
  textos_pdf <- pdftools::pdf_text(arquivo)
  # gerar um vetor de tamanho n, sendo n o número da página!
  
  df_texto_limpo <- textos_pdf |>
    tibble::as_tibble() |>
    mutate(doc_id = arquivo, .before = tidyselect::everything()) |>
    rowid_to_column("n_pagina") |>
    mutate(value = str_split(value, "\n")) |>
    unnest(value) |>
    mutate(value = str_trim(value)) |>
    filter(value != "") |>
    group_by(n_pagina) |>
    mutate(n_linha = row_number()) |>
    ungroup()
  
  df_texto_limpo
}

textos_varios_pdfs <- list.files("textos", full.names = TRUE) |> 
  purrr::map(extrair_textos_pdf)


df_varios_pdfs <- textos_varios_pdfs |> 
  purrr::list_rbind()


df_varios_pdfs |>
  unnest_tokens(input = value, output = "word") |> 
  count(word, sort = TRUE) 
    
