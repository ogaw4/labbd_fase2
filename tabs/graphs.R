
tabItem(
  tabName = 'graphs',
  
  fluidRow(
    h3("Gráficos - Dados de negociação")
  ), 
  fluidRow(
    column(4, dateRangeInput("graph_dt_range", "Período de referência", start=add.bizdays(dates=Sys.Date(), n=-50),
                             end=add.bizdays(dates=Sys.Date(), n=-30))),
    column(3, radioButtons('graph_type', 'Tipo do gráfico de preços',
                           choices = list( `Candlestick` = 'candle',
                                           `Linha` = 'line'
                           ), 
                           selected = 'candle')),
    column(2, textInput("qsearch_terms", label = "", placeholder = 'Busca rápida (exata)')), 
    column(2, br(), actionButton('qsearch', 'Ir'))
  ), 
  fluidRow(
    column(6, DT::DTOutput('instr_info')), 
    column(6, DT::DTOutput('company_info'))
  ),
  fluidRow(
    column(12, br(), br(), plotlyOutput("main_plot", height = "600px"))
  ), 
  fluidRow(
    column(12, br(), br(), plotlyOutput("balance_report", height = "400px"))
  ), 
  fluidRow(
    column(12, br(), br(), plotlyOutput("capital_report", height = "400px"))
  )
)