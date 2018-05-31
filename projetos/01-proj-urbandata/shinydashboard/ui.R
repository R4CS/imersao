library("shinydashboard")
library("shiny")
library(DT)


header <- dashboardHeader(title = "Busca Teses USP", 
                          dropdownMenu(type = "notifications",
                                       notificationItem(text = "versão 1.0")
                          )
)

sidebar <- dashboardSidebar(disable = TRUE)

body <- dashboardBody(
  fluidRow(
    box(
      title = "As buscas podem ser feitas juntas ou separadas",
      width = 5,
      status = "primary",
      solidHeader = TRUE,
      textInput(inputId = "busca_area", label = "Buscar por área do conhecimento"),
      textInput(inputId = "busca_ano_defesa", "Buscar por ano da defesa"),
      hr(),
      actionButton(inputId = "botao_buscar", label = "Buscar", width = "20%"),
      actionButton(inputId = "botao_limpar", label = "Limpar", width = "20%")
    ),
    fluidRow(
      box(width = 5,
          infoBox("Número de linhas", icon = icon(name = "search"), fill = TRUE, width = "10%"),
          infoBox("Número de Colunas", icon = icon(name = "search"), fill = TRUE, width = "10%")
      )
    )
  ),
  box(title = "Resultado", 
      width = 1/2, 
      height = "600px",
      status = "primary",
      solidHeader = TRUE,
      DT::dataTableOutput(outputId = "tabela_resultado", width = "20%", height = "20%")
  )
)

ui <- dashboardPage(header, sidebar, body)