# Arquivo: obter_tesesUSP.R
# Exemplo no final do arquivo
# Pacotes utilizados
library(tidyverse) # Carrega alguns pacotes, entre eles, dplyr e stringr
library(rvest) # Para fazer web scraping
library(httr) # Para fazer requisições ao site de interesse

# Tipo de funcao: auxiliar
# Objetivo: Criar um vetor numérico para ser colocado na url
obter_pagina <- function(request){
  pagina <- request %>% 
    httr::content(as = 'parsed') %>% 
    html_node(xpath = '//div[@class="dadosLinha"]') %>%
    html_text() %>% 
    str_split(pattern = " ") %>%
    purrr::pluck(1) %>%
    as.numeric() %>%
    sort() %>%
    dplyr::last()
  pagina <- ceiling(pagina/10)
  pagina <- seq(1, pagina, 1)
  return(pagina)
}

# Tipo de funcao: auxiliar
# Objetivo: Colocar um parametro especifico da url conjunto com o vetor obtido pela funcao obter_pagina
incluir_pag <- function(paginas, request){
  purrr::map_chr(paginas, ~request %>% purrr::pluck('url') %>% 
                   str_c("&pagina=", as.character(.x)))
}

# FUNCAO SEM USO
# ler_html <- function(links){
#   Sys.sleep(2)
#   node <- read_html(links)
#   return(node)
# }

# Tipo de funcao: auxiliar
# Objetivo: Estruturar as informações das teses em linhas de um dataframe a partir dos links obtidos na funcao busca_avancada
parser_teses_usp_principal <- function(links){
  #print(links)
  Sys.sleep(1)
  id <- links %>% 
    str_extract("tde.*\\/") %>% str_replace_all("[^0-9]", "")
  node <- links %>% read_html()
  key <- node %>% rvest::html_nodes(xpath = '//div[@class="DocumentoTituloTexto"]') %>% rvest::html_text(trim = T)
  val <- node %>% rvest::html_nodes(xpath = '//div[@class="DocumentoTexto"]') %>% rvest::html_text(trim = T)
  key2 <- node %>% rvest::html_nodes(xpath = '//div[@class="DocumentoTituloTexto2"]') %>% rvest::html_text(trim = T) %>% 
    .[stringr::str_detect(., "[R-r]esumo em")]
  val2 <- node %>% rvest::html_nodes(xpath = '//div[@class="DocumentoTextoResumo"]') %>% rvest::html_text(trim = T)
  tab_verificacao <- table(key == key2)
  tab_verificacao <- tab_verificacao[names(tab_verificacao) == T] > 0
  if(purrr::is_empty(tab_verificacao)){
    key <- key[which(!(key %in% key2))]
  }
  key <- c(key, key2)
  val <- c(val, val2)
  tabela <- tibble(key, val) %>% 
    mutate(key = stringr::str_to_lower(key),
           key = stringr::str_replace_all(key, "[[:punct:]]", " "),
           key = abjutils::rm_accent(key),
           key = stringr::str_replace_all(key, " +", "_"),
           val = stringr::str_to_lower(val)) %>% 
    spread(key, val) %>% 
    mutate(links = links,
           id = id)
  return(tabela)
}

# Tipo de funcao: auxiliar
# Objetivo: Fazer uma tabela de referencia para obter os links para informacoes detalhadas que serão usadas na funcao parser_teses_usp_principal
parser_teses_usp_referencia <- function(links){
  # print(links)
  node <- links %>% xml2::read_html()
  x1 <- node %>% html_nodes(xpath = '//div[@class="dadosDocNome"]') %>% html_text() %>% 
    .[2:length(.)]
  x2 <- node %>% html_nodes(xpath = '//div[@class="dadosDocTitulo"]') %>% html_text()%>% 
    .[2:length(.)]
  x3 <- node %>% html_nodes(xpath = '//div[@class="dadosDocArea"]') %>% html_text()%>% 
    .[2:length(.)]
  x4 <- node %>% html_nodes(xpath = '//div[@class="dadosDocTipo"]') %>% html_text()%>% 
    .[2:length(.)]
  x5 <- node %>% html_nodes(xpath = '//div[@class="dadosDocUnidade"]') %>% html_text()%>% 
    .[2:length(.)]
  x6 <- node %>% html_nodes(xpath = '//div[@class="dadosDocAno"]') %>% html_text()%>% 
    .[2:length(.)]
  x7 <- node %>% html_nodes(xpath = '//div[@class="dadosDocNome"]/a') %>% html_attr('href')
  x8 <- node %>% html_nodes(xpath = '//div[@class="dadosDocNome"]/a') %>% html_attr('href') %>% 
    str_extract("tde.*\\/") %>% str_replace_all("[^0-9]", "")
  
  tabela <- tibble(nome = x1, titulo = x2, area = x3 ,Tipo = x4, unidade = x5, ano = x6, link_detalhe = x7, id = x8)
  return(tabela)
}

# Tipo de funcao: auxiliar
# Objetivo: Baixar todos os arquivos de interesse e estrutura-los em uma tabela utilizada posteriormente em obter_teses_usp 
busca_avancada <- function(area_conhecimento =NULL , ano_defesa = NULL){
  url <- "http://www.teses.usp.br/index.php"
  if(is.null(ano_defesa) & is.null(area_conhecimento) == F){
    query <- list(
      "option"="com_jumi",
      "fileid"=19,
      "Itemid"=87,
      "lang"="pt-br",
      "g"=1,
      "b0"=area_conhecimento,
      "c0"="c",
      "o0"="AND")    
  } else if(is.numeric(ano_defesa) & is.null(area_conhecimento) == F){
    query <- list(
      "option"="com_jumi",
      "fileid"=19,
      "Itemid"=87,
      "lang"="pt-br",
      "g"=1,
      "b0"=area_conhecimento,
      "c0"="c",
      "o0"="AND",
      "b1"=ano_defesa,
      "c1"="a",
      "o1"="AND"
    )
  } else if(is.null(ano_defesa) == FALSE & is.null(area_conhecimento) & is.numeric(ano_defesa)){
    query <- list(
      "option"="com_jumi",
      "fileid"=19,
      "Itemid"=87,
      "lang"="pt-br",
      "g"=1,
      "b0"=ano_defesa,
      "c0"="a",
      "o0"="AND"
    )
  }
  
  request <- GET(url, query = query)
  paginas <- obter_pagina(request)
  links <- incluir_pag(paginas, request)
  
  tab <- purrr::map_dfr(links, ~parser_teses_usp_referencia(.x))
  return(tab)
}

# Tipo de funcao: principal
# Objetivo: Baixar os arquivos e estrutura-los em dois modelos de analise. Modelo 1: As palavras são separadas por virgula. Modelo 2: Spread por palavra chava, ou seja, se a tese i tiver n palavras chaves, logo, teremos n observações para tese i
obter_teses_usp <- function(area_conhecimento = NULL, ano_defesa = NULL, modelo = 1){
  tabela <- suppressMessages(suppressWarnings(busca_avancada(area_conhecimento = area_conhecimento, ano_defesa = ano_defesa)))
  tabela_principal <- suppressMessages(suppressWarnings(purrr::map_dfr(tabela$link_detalhe, ~parser_teses_usp_principal(links = .x))))
  if(modelo == 1){
    tabela_principal %>%  
      select(area_do_conhecimento, nome_completo, data_de_defesa, data_de_publicacao, documento, doi, orientador, palavras_chave_em_portugues,
             titulo_em_portugues, unidade_da_usp, resumo_em_portugues, resumo_em_ingles, id, links) %>% 
      mutate(palavras_chave_em_portugues = stringr::str_replace_all(palavras_chave_em_portugues, "\\n\\n", ", "))
  } else if (modelo == 2){
    tabela_principal %>% tidyr::separate_rows(palavras_chave_em_portugues, sep = "\\n\\n", convert = T) %>% 
      select(area_do_conhecimento, nome_completo, data_de_defesa, data_de_publicacao, documento, doi, orientador, palavras_chave_em_portugues,
             titulo_em_portugues, unidade_da_usp, resumo_em_portugues, resumo_em_ingles, id, links) 
  }
}

############
# EXEMPLOS #
############

## Busca por area do conhecimento e ano da defesa
#obter_teses_usp(area_conhecimento = "Geologia", ano_defesa = 2000, modelo = 1)
#obter_teses_usp(area_conhecimento = "Geologia", ano_defesa = 2000, modelo = 2)

## Busca por ano da defesa
#obter_teses_usp(ano_defesa = 1975, modelo = 1)
#obter_teses_usp(ano_defesa = 1975, modelo = 2)

## Busca por area do conhecimento
#obter_teses_usp(area_conhecimento = "Fisioterapia", modelo = 1)
#obter_teses_usp(area_conhecimento = "Fisioterapia", modelo = 2)
