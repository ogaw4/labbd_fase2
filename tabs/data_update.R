
tabItem(
  tabName = 'data_update',
  fluidRow(
    column(12, h3('Inserção de dados de negociação'))
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
    column(12, h3('Inserção de dados de empresas'))
  ),
  fluidRow(
    column(4, actionButton("run_update_corp", "Obter dados e atualizar banco"))
  ), 
  fluidRow(
    column(6, htmlOutput('execution_log_corp'))
  ),
  fluidRow(
    column(12, h3('Alteração de dados'))
  ),
  fluidRow(
    column(3, textInput("from_cpy", "Nome da empresa a ser alterada")), 
    column(3, textInput("to_cpy", "Nome novo")), 
    column(3, br(), actionButton("run_update_cpy", "Alterar empresa"))
  ), 
  fluidRow(
    column(6, htmlOutput('execution_log_updt'))
  )
)