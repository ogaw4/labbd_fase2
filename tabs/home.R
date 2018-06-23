
tabItem(
  tabName = 'home',
  
  fluidRow(
    h3("Busca - Dados de negociação e empresas")
  ), 
  fluidRow(
    column(8, textInput('search_terms', label = "", 
                        placeholder = "Tickers de ações, nomes de empresas, áreas de atuação...",
                        width = "100%")),
    column(2, radioButtons('search_type', 'Tipo de busca',
                           choices = list( `Exata` = 'exact',
                                           `Parcial` = 'partial'
                           ), 
                           selected = 'exact')),
    column(2, br(),  actionButton('do_search', 'Buscar'))
  ), 
  fluidRow(
    column(12, uiOutput('search_result'))
  )
)