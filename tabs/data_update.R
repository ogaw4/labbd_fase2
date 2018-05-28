
tabItem(
  tabName = 'data_update',
  fluidRow(
    column(12, h2('Inserção de dados de negociação'))
  ),
  fluidRow(
    column(6, dateRangeInput("dt_range", "Período para inserção", start=add.bizdays(dates=Sys.Date(), n=-1),
                             end=add.bizdays(dates=Sys.Date(), n=-1)))
  ),
  fluidRow(
    column(4, actionButton("run_update_neg", "Baixar arquivos e atualizar banco"))
  ), 
  fluidRow(
    column(6, htmlOutput('execution_log_neg'))
  ),
  fluidRow(
    column(12, h2('Inserção de dados de empresas (Não implementado!)'))
  ),
  fluidRow(
    column(4, actionButton("run_update_corp", "Obter dados e atualizar banco"))
  ), 
  fluidRow(
    column(6, htmlOutput('execution_log_corp'))
  )
)