
tabItem(
  tabName = 'home',
  
  fluidRow(
    h3("Busca - Dados de negociação e empresas")
  ), 
  fluidRow(
    column(10, textInput('search_terms', label = "", 
                         placeholder = "Tickers de ações, nomes de empresas, áreas de atuação...",
                         width = "100%")),
    column(2, br(),  actionButton('do_search', 'Buscar'))
  ), 
  fluidRow(
    column(12, uiOutput('search_res'))
  )
)