
tabItem(
  tabName = 'home',
  
  fluidRow(
    h3("Protótipo - Dados de negociação")
  ), 
  fluidRow(
    column(2, textInput('instr', 'Instrumento: ')),
    column(3, dateRangeInput("graph_dt_range", "Período de referência", start=add.bizdays(dates=Sys.Date(), n=-30),
                             end=add.bizdays(dates=Sys.Date(), n=-1))),
    column(2, br(),  actionButton('create_graphs', 'Popular gráficos')), 
    column(5, verbatimTextOutput('search_result'))
  ), 
  fluidRow(
    column(3, radioButtons('graph_type', 'Tipo do gráfico de preços',
                           choices = list( `Candlestick` = 'candle',
                                            `Linha` = 'line'
                                          ), 
                           selected = 'candle')),
    column(9, verbatimTextOutput('instr_info'))
  ),
  fluidRow(
    column(12, plotlyOutput("main_plot", height = "600px"))
  )
)