
observe(
  { 
    values$cur_ticker
    input$graph_dt_range
  }, 
  {
    ticker <- values$cur_ticker
    instr_src <- values$ticker_src
    dr <- input$graph_dt_range
    if (!is.null(ticker)) {
      curr_instr <- all_instr_list() %>% 
        filter(code == ticker, src == instr_src)
      
      output$instr_info <- renderText({
        switch(
          instr_src, 
          option = {
            instr <- opts_list %>% filter(code == curr_instr)
            sprintf('Código: %s\nUnderlying: %s\nVencimento: %s\nStrike: %s\nTipo: %s\nEstilo: %s', 
                    instr$code, instr$cod_underlying, instr$maturity, instr$strike, instr$type, instr$model)
          }, 
          future = {
            instr <- futs_list %>% filter(code == curr_instr)
            sprintf('Código: %s\nObjeto: %s\nVencimento: %s', 
                    instr$code, instr$objeto, instr$maturity_date)
          }, 
          stock = {
            instr <- stocks_list %>% filter(ticker == curr_instr)
            sprintf('Código: %s\nEmpresa: %s\nTipo: %s', 
                    instr$ticker, instr$nome, instr$type)
          }, 
          idx = {
            instr <- idx_list %>% filter(code == curr_instr)
            sprintf('Código: %s\nDescrição: %s', 
                    instr$code, instr$desc)
            
          }
        )
      })
      
      output$main_plot <- suppressWarnings(renderPlotly({
        plt <- gen_main_plot(curr_instr, instr_src, dr, input$graph_type)
        validate(need(!is.null(plt), 'Não foram encontrados dados para o período e instrumento dados.'))
        plt
      }))
    }
})
