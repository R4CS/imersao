
server <- function(input, output) {
  output$tabela_resultado = DT::renderDataTable({
    tab
  })
}

shinyApp(ui, server)
