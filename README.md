
- [cebrap.lab online - Análise quantitativa de
  texto](#cebraplab-online---análise-quantitativa-de-texto)
  - [Informações gerais](#informações-gerais)
  - [Pacotes para instalar](#pacotes-para-instalar)
  - [Materiais](#materiais)
    - [Slides](#slides)
    - [Scripts pós aula](#scripts-pós-aula)
    - [Exercícios](#exercícios)
  - [Materiais para revisão do conteúdo
    básico](#materiais-para-revisão-do-conteúdo-básico)
  - [Materiais para ir além](#materiais-para-ir-além)

<!-- README.md is generated from README.Rmd. Please edit that file -->

# cebrap.lab online - Análise quantitativa de texto

Para saber de próximos oferecimentos, entre em contato com
<cebrap.lab@cebrap.org.br>

## Informações gerais

- **Ministrante:** [Beatriz Milz](https://beamilz.com/)

- **Data/horário:** 27 de novembro a 1 de dezembro/2023;

  - Segunda, Quarta e Sexta: aula ao vivo, online, das 19h00 às 22h00.
  - Terça e quinta: haverão tarefas para casa, para praticar os
    conceitos.

- **Inscrições:** <https://cursos.cebrap.org.br/>

## Pacotes para instalar

- Para ser instalado via CRAN:

``` r
pacotes <-
  c("tidyverse",
    "tidytext",
    "stopwords",
    "wordcloud2",
    "janitor",
    "remotes",
    "readtext",
    "fs")
install.packages(pacotes)
```

- Para ser instalado via GitHub:

``` r
remotes::install_github("dfalbel/ptstem")
```

## Materiais

### Slides

- [Introdução ao
  curso](https://beatrizmilz.github.io/2023-11-cebrap-lab-text-as-data/slides/introducao-ao-curso.html)

- [Introdução: Análise quantitativa de
  texto](https://beatrizmilz.github.io/2023-11-cebrap-lab-text-as-data/slides/intro-text-as-data.html)

### Scripts pós aula

#### Segunda-feira

- [01-download-dados.R](https://github.com/beatrizmilz/2023-11-cebrap-lab-text-as-data/blob/main/scripts-pos-aula/01-download-dados.R) -
  exemplo simplificado do download dos dados.

- [01-download-dados-extra.R](https://github.com/beatrizmilz/2023-11-cebrap-lab-text-as-data/blob/main/scripts-pos-aula/01-download-dados-extra.R) -
  arquivo mais extenso com o exemplo do download dos dados.

- [02-pre-processamento.R](https://github.com/beatrizmilz/2023-11-cebrap-lab-text-as-data/blob/main/scripts-pos-aula/02-pre-processamento.R) -
  leitura, limpeza, conhecendo os dados, tokenização, removendo stop
  words, removendo números, stemming, salvando a base pré-processada
  para a análise na aula seguinte.

- [stop-words.R](https://github.com/beatrizmilz/2023-11-cebrap-lab-text-as-data/blob/main/scripts-pos-aula/stop-words.R) -
  script usado para gerar a lista de stop words usada no exercício.

#### Quarta-feira

- [03-descritiva-visualizacao.R](https://github.com/beatrizmilz/2023-11-cebrap-lab-text-as-data/blob/main/scripts-pos-aula/03-descritiva-visualizacao.R) -
  buscando palavras mais frequentes por grupo, visualização com
  wordcloud, visualização com gráfico de barras.

- [04-ngrams.R](https://github.com/beatrizmilz/2023-11-cebrap-lab-text-as-data/blob/main/scripts-pos-aula/04-ngrams.R) -
  bigramas, visualização com gráfico de barras, tf-idf.

#### Sexta-feira

- [05-importacao.R](https://github.com/beatrizmilz/2023-11-cebrap-lab-text-as-data/blob/main/scripts-pos-aula/05-importacao.R) -
  importação de arquivos (exemplo com PDFs).

- [06-modelagem-topicos.R](https://github.com/beatrizmilz/2023-11-cebrap-lab-text-as-data/blob/main/scripts-pos-aula/06-modelagem-topicos.R) -
  modelagem de tópicos (LDA).

### Exercícios

#### Terça-feira

- [exercicio-1](https://github.com/beatrizmilz/2023-11-cebrap-lab-text-as-data/blob/main/exercicios/exercicio-1.md)

- [exercicio-1 - com
  respostas](https://github.com/beatrizmilz/2023-11-cebrap-lab-text-as-data/blob/main/exercicios/exercicio-1-respondido.md)

#### Quinta-feira

- [exercicio-2](https://github.com/beatrizmilz/2023-11-cebrap-lab-text-as-data/blob/main/exercicios/exercicio-2.md)

## Materiais para revisão do conteúdo básico

- [Material do curso de Introdução ao R - 2023 (ministrado por Beatriz
  Milz)](https://beatrizmilz.github.io/2023-06-cebrap-lab-intro-R/)

- [Material do curso de Visualização de dados - 2023 (ministrado por
  Beatriz Milz)](https://beatrizmilz.github.io/2023-06-cebrap-lab-viz/)

- [Material da disciplina “Análise de Dados para as Ciências Sociais” -
  tutoriais 1 até 6.](https://jonnyphillips.github.io/Ciencia_de_Dados/)

- [Livro online, gratuito e em inglês: R for Data
  Science](https://r4ds.hadley.nz/) - A versão preliminar da tradução
  deste livro está disponível em:
  <https://cienciadedatos.github.io/pt-r4ds/>

- [Livro online e gratuito: Ciência de Dados em
  R](https://livro.curso-r.com/7-2-dplyr.html)

## Materiais para ir além

- Principal referência do curso: [Text Mining with
  R](https://www.tidytextmining.com/) - Julia Silge and David Robinson

- [Slides sobre o pacote
  stringr](https://curso-r.github.io/202308-r4ds-2/materiais/slides/02_strings.html#1)

- Referência mais avançada: [Livro Supervised Machine Learning for Text
  Analysis in R](https://smltar.com/) - Emil Hvitfeldt and Julia Silge

- [Capítulo sobre strings -
  R4DS](https://cienciadedatos.github.io/pt-r4ds/strings.html)

- [Capítulo sobre expressões regulares -
  R4DS](https://cienciadedatos.github.io/pt-r4ds/regexps.html)
