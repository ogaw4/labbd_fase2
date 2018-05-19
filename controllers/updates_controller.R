
observeEvent(input$run_update, {
  
  dr <- input$dt_range
  values$execution_log <- ""
  wanted_data <- input$content_to_update
  
  if (length(wanted_data) == 0) {
    log.info("Nada escolhido para ser atualizado!")
    return()
  }
  
  withProgress(message = 'Atualizando dados...', value = 0, {
    all_dates <- bizseq(dr[1], dr[2])
    n_steps <- length(all_dates)
    for (data_ref in as.character(all_dates)) {
      d_dir <- paste0(dest_dir, data_ref)
      if (!dir.exists(d_dir)) dir.create(d_dir)
      log.info(paste0("Baixando arquivos com data de referência ", data_ref, "."))
      #---------------------------------------------------------
      tryCatch({
        download_data(d_dir, data_ref, wanted_data)
      }, error = function(e) {
        log.error(paste("Erro baixando arquivos,", e))
      })
      log.info("Arquivos baixados.")
      log.info(paste0("Inserindo dados com data de referência ", data_ref, "."))
      tryCatch({
        insert_data(d_dir, data_ref, wanted_data)
      }, error = function(e) {
        log.error(paste("Erro inserindo dados,", e))
      })
      log.info("Dados inseridos.")
      incProgress(amount = 1/n_steps)
    }
  })
  log.info("Dados atualizados.")
  
  return()
  
})


output$execution_log <- renderText({
  values$execution_log
})