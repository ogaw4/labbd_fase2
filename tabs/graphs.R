
tabItem(
  tabName = 'graphs',
  
  fluidRow(
    h3("Gráficos - Dados de negociação")
  ), 
  fluidRow(
    column(6, dateRangeInput("graph_dt_range", "Período de referência", start=add.bizdays(dates=Sys.Date(), n=-30),
                             end=add.bizdays(dates=Sys.Date(), n=-1))),
    column(3, radioButtons('graph_type', 'Tipo do gráfico de preços',
                           choices = list( `Candlestick` = 'candle',
                                           `Linha` = 'line'
                           ), 
                           selected = 'candle'))
  ), 
  fluidRow(
    column(9, verbatimTextOutput('instr_info'))
  ),
  fluidRow(
    column(12, plotlyOutput("main_plot", height = "600px"))
  )
)