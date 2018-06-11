
tagList(
  dashboardPage(
    skin='blue',
    
    dashboardHeader(title = 'Dados B3'), 
    
    dashboardSidebar(
      br(),
      sidebarMenu(
        id = 'tabs', 
        menuItem('Busca', tabName = 'home', icon = icon('search')),
        menuItem('Gráficos', tabName = 'graphs', icon = icon('stats', lib = 'glyphicon')), 
        menuItem('Inserção de dados', tabName = 'data_update', icon = icon('database'))
      )
    ), 
    
    dashboardBody(
      includeCSS(file.path("www", "custom.css")), 
      do.call( # conteudo das abas
        tabItems,
        lapply(list.files("tabs"), function(file) {
          source(file.path("tabs", file), local = TRUE, encoding = "UTF-8")$value
        })
      )
    )
  ), 
  tags$footer("Esta página não tem associação alguma à B3 e não garante precisão dos dados informados.", 
              align = 'center', 
              style = "
              bottom:0;
              width:100%;
              height:50px;   /* Height of the footer */
              color: white;
              padding: 10px;
              background-color: black;
              z-index: 1000;")
)
