
tabItem(
  tabName = 'home',
  
  fluidRow(
    h3("Busca - Dados de negociação e empresas")
  ), 
  fluidRow(
    column(8, textInput('search_terms', label = "", 
                        placeholder = "Tickers de ações, opções, futuros, índices, nomes de empresas...",
                        width = "100%")),
    column(3, br(),  actionButton('do_search', 'Buscar'))
  ), 
  fluidRow(
    column(12, uiOutput('search_result'))
  )
)