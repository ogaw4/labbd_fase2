
tabItem(
  tabName = 'data_update',
  fluidRow(
    column(6, dateRangeInput("dt_range", "Período para inserção", start=add.bizdays(dates=Sys.Date(), n=-1),
                             end=add.bizdays(dates=Sys.Date(), n=-1)))
  ),
  fluidRow(
    column(4, checkboxGroupInput("content_to_update", "Dados a serem atualizados:", 
                                 choices = list(`Dados de negociação e cadastros` = "neg", 
                                                `Relatórios de empresas (Não implementado)` = "rel")))
  ),
  fluidRow(
    column(4, actionButton("run_update", "Baixar arquivos e atualizar banco"))
  ), 
  fluidRow(
    column(6, htmlOutput('execution_log'))
  )
)