

log.info <- function(msg, src) {
  if (src == 'neg') {
    values$execution_log_neg <- paste0(values$execution_log_neg, '<br><span style="color:green"> <b>[INFO]</b> ', msg, '</span>')
  } else {
    values$execution_log_corp <- paste0(values$execution_log_corp, '<br><span style="color:green"> <b>[INFO]</b> ', msg, '</span>')
  }
}

log.error <- function(msg, src) {
  if (src == 'neg') {
    values$execution_log_neg <- paste0(values$execution_log_neg, '<br><span style="color:red"> <b>[ERRO]</b> ', msg, '</span>')
  } else {
    values$execution_log_corp <- paste0(values$execution_log_corp, '<br><span style="color:red"> <b>[ERRO]</b> ', msg, '</span>')
  }
}


observeEvent(input$create_graphs, {
  withProgress(message = 'Gerando gráfico...', value = 0, {
    target_instr <- input$instr
    dr <- input$graph_dt_range
    
    stocks_list <- list_stocks() %>% mutate(src = 'stock')
    opts_list <- list_options(maturity_filter = as.character(dr[1])) %>% mutate(src = 'option')
    futs_list <- list_futures(maturity_filter = as.character(dr[1])) %>% mutate(src = 'future')
    idx_list <- list_index() %>% mutate(src = 'index')
    
    instrument_list <- stocks_list %>% select(code = ticker, src) %>%
      rbind(opts_list %>% select(code, src)) %>%
      rbind(futs_list %>% select(code, src)) %>% 
      rbind(idx_list %>% select(code, src))
    
    # Tenta match exato e depois parcial se não encontra exato
    wanted <- instrument_list[instrument_list$code == target_instr, ]
    
    if (nrow(wanted) == 0) { wanted <- instrument_list %>% filter(grepl(target_instr, instrument_list$code, ignore.case = T)) }
    if (nrow(wanted) == 0) {
      output$search_result <- renderText({
        paste0("Nenhum instrumento contendo ", target_instr, " foi encontrado.")
      })
    } else {
      if (nrow(wanted) > 1) {
        output$search_result <- renderText({
          paste0("Mais de um instrumento foi encontrado, utilizando o primeiro (na versão final a busca será melhorada)")
        })
      }
      curr_instr <- wanted[1, ]$code
      instr_src <- wanted[1, ]$src
      
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
      
      incProgress(amount = 1/2)
      output$main_plot <- suppressWarnings(renderPlotly({
        plt <- gen_main_plot(curr_instr, instr_src, dr, input$graph_type)
        validate(need(!is.null(plt), 'Não foram encontrados dados para o período e instrumento dados.'))
        plt
      }))
      
    }
  })
  
})
