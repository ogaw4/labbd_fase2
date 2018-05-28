
observeEvent(input$run_update_neg, {
  
  dr <- input$dt_range
  values$execution_log_neg <- ""
  
  withProgress(message = 'Atualizando dados...', value = 0, {
    all_dates <- bizseq(dr[1], dr[2])
    n_steps <- length(all_dates)
    for (data_ref in as.character(all_dates)) {
      d_dir <- paste0(dest_dir, data_ref)
      if (!dir.exists(d_dir)) dir.create(d_dir)
      log.info(paste0("Baixando arquivos com data de referência ", data_ref, "."), 'neg')
      #---------------------------------------------------------
      tryCatch({
        download_data(d_dir, data_ref)
      }, error = function(e) {
        log.error(paste("Erro baixando arquivos,", e), 'neg')
      })
      log.info("Arquivos baixados.", 'neg')
      log.info(paste0("Inserindo dados com data de referência ", data_ref, "."), 'neg')
      tryCatch({
        insert_data(d_dir, data_ref)
      }, error = function(e) {
        log.error(paste("Erro inserindo dados,", e), 'neg')
      })
      log.info("Dados inseridos.", 'neg')
      incProgress(amount = 1/n_steps)
    }
  })
  log.info("Dados atualizados.", 'neg')
  
  return()
  
})


output$execution_log_neg <- renderText({
  values$execution_log_neg
})



observeEvent(input$run_update_corp, {
  
  dr <- input$dt_range
  values$execution_log_corp <- ""
  log.error("Obtenção de dados de empresas não implementada ainda!", 'corp')
  
  
  # withProgress(message = 'Obtendo dados...', value = 0, {
  #   all_dates <- bizseq(dr[1], dr[2])
  #   n_steps <- length(all_dates)
  #   for (data_ref in as.character(all_dates)) {
  #     d_dir <- paste0(dest_dir, data_ref)
  #     if (!dir.exists(d_dir)) dir.create(d_dir)
  #     log.info(paste0("Baixando arquivos com data de referência ", data_ref, "."), 'neg')
  #     #---------------------------------------------------------
  #     tryCatch({
  #       download_data(d_dir, data_ref)
  #     }, error = function(e) {
  #       log.error(paste("Erro baixando arquivos,", e), 'neg')
  #     })
  #     log.info("Arquivos baixados.", 'neg')
  #     log.info(paste0("Inserindo dados com data de referência ", data_ref, "."), 'neg')
  #     tryCatch({
  #       insert_data(d_dir, data_ref)
  #     }, error = function(e) {
  #       log.error(paste("Erro inserindo dados,", e), 'neg')
  #     })
  #     log.info("Dados inseridos.", 'neg')
  #     incProgress(amount = 1/n_steps)
  #   }
  # })
  # log.info("Dados atualizados.", 'neg')
  
  return()
  
})


output$execution_log_corp <- renderText({
  values$execution_log_corp
})