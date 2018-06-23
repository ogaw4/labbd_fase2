
observeEvent(
  { 
    values$cur_ticker
    input$graph_dt_range
  }, 
  {
    ticker <- values$cur_ticker
    instr_src <- values$ticker_src
    dr <- input$graph_dt_range
    if (!is.null(ticker)) {
      curr_instr <- (all_instr_list() %>% 
        filter(code == ticker, src == instr_src))$code[1]
      
      curr_company <- NULL
      cname <- ""
      
      output$instr_info <- renderDT({
        switch(
          instr_src, 
          option = {
            instr <- opts_list() %>% filter(code == curr_instr)
            DT::datatable(data.frame(`Código` = instr$code, `Underlying` = instr$cod_underlying, 
                       `Vencimento` = instr$maturity, `Strike` = instr$strike, 
                       `Tipo` = instr$type, `Estilo` = instr$model), editable = F, rownames = F,  options = list(dom = "t"),
                       filter = 'none')
          }, 
          future = {
            instr <- futs_list() %>% filter(code == curr_instr)
            DT::datatable(data.frame(`Código` = instr$code, `Objeto` = instr$objeto, `Vencimento` = instr$maturity_date),
                          editable = F, rownames = F,  options = list(dom = "t"),
                          filter = 'none')
          }, 
          stock = {
            instr <- stocks_list() %>% filter(ticker == curr_instr)
            DT::datatable(data.frame(`Código` = instr$ticker, `Objeto` = instr$nome, `Tipo` = instr$type),
            editable = F, rownames = F,  options = list(dom = "t"),
            filter = 'none')
          }, 
          idx = {
            instr <- idx_list() %>% filter(code == curr_instr)
            DT::datatable(data.frame(`Código` = instr$code, `Descrição` = instr$desc),
            editable = F, rownames = F,  options = list(dom = "t"),
            filter = 'none')
            
          }, 
          company = {
            DT::datatable(data.frame(`Vazio` = "Não há dados de instrumento."),
                          editable = F, rownames = F,  options = list(dom = "t"),
                          filter = 'none')
          }
        )
      })
      
      
      output$company_info <- renderDT({
        switch(
          instr_src, 
          option = {
            instr <- opts_list() %>% filter(code == curr_instr)
            ulyg <- instr$cod_underlying
            if (ulyg %in% stocks_list()$ticker) {
              cmpny <- first((stocks_list() %>% filter(ticker == ulyg))$cnpj)
              c_data <- companies_list() %>% filter(cnpj == cmpny) 
              curr_company <<- cmpny
              cname <<- first(c_data$name)
              DT::datatable(data.frame(`Empresa` = first(c_data$name), `CNPJ` = format_cnpj(first(c_data$cnpj)), `Setores` = first(c_data$sectors), 
                                       `Atividades` = first(c_data$actv), `Site` = first(c_data$site), `Ações` = paste(c_data$ticker, collapse = ";")),
                            editable = F, rownames = F,  options = list(dom = "t"),
                            filter = 'none')
            } else {
              DT::datatable(data.frame(`Vazio` = "Não há dados de empresa associada a essa opção."),
                            editable = F, rownames = F,  options = list(dom = "t"),
                            filter = 'none')
            }
          }, 
          future = {
            DT::datatable(data.frame(`Vazio` = "Não há dados de empresa associada a futuro."),
                          editable = F, rownames = F,  options = list(dom = "t"),
                          filter = 'none')
          }, 
          stock = {
            instr <- stocks_list() %>% filter(ticker == curr_instr)
            if (is.na(instr$cnpj)) {
              DT::datatable(data.frame(`Vazio` = "Não há dados de empresa associada a essa ação."),
                            editable = F, rownames = F,  options = list(dom = "t"),
                            filter = 'none')
              
            } else {
              cmpny <- first(instr$cnpj)
              curr_company <<- cmpny
              c_data <- companies_list() %>% filter(cnpj == cmpny) 
              cname <<- first(c_data$name)
              DT::datatable(data.frame(`Empresa` = first(c_data$name), `CNPJ` =  format_cnpj(first(c_data$cnpj)), `Setores` = first(c_data$sectors), 
                                       `Atividades` = first(c_data$actv), `Site` = first(c_data$site), `Ações` = paste(c_data$ticker, collapse = "; ")),
                            editable = F, rownames = F,  options = list(dom = "t"),
                            filter = 'none')
            }
          }, 
          idx = {
            DT::datatable(data.frame(`Vazio` = "Não há dados de empresa associada a índice"),
                          editable = F, rownames = F,  options = list(dom = "t"),
                          filter = 'none')
          }, 
          company = {
            c_data <- companies_list() %>% filter(name == curr_instr)
            curr_company <<- first(c_data$cnpj)
            cname <<- curr_instr
            DT::datatable(data.frame(`Empresa` = first(c_data$name), `CNPJ` =  format_cnpj(first(c_data$cnpj)), `Setores` = first(c_data$sectors), 
                                     `Atividades` = first(c_data$actv), `Site` = first(c_data$site), `Ações` = paste(c_data$ticker, collapse = "; ")),
                          editable = F, rownames = F,  options = list(dom = "t"),
                          filter = 'none')
          }
        )
      })
      
      
      
      output$main_plot <- suppressWarnings(renderPlotly({
        plt <- gen_main_plot(curr_instr, instr_src, dr, input$graph_type)
        shiny::validate(need(!is.null(plt), 'Não foram encontrados dados para o período e instrumento dados.'))
        plt
      }))
      
      output$balance_report <- suppressWarnings(renderPlotly({
        shiny::validate(need(!is.null(curr_company), 'Não há dados de empresa associados ao instrumento.'))
        gen_balance_plot(curr_company, cname)
      }))
      
      output$capital_report <- suppressWarnings(renderPlotly({
        shiny::validate(need(!is.null(curr_company), 'Não há dados de empresa associados ao instrumento.'))
        gen_capital_plot(curr_company, cname)
      }))
    }
})

observeEvent(input$qsearch, {
  search_instr_graphs()
})
