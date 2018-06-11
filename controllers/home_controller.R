

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

# TODO: adicionar dados de empresas e outras colunas (empresa, area...)
all_instr_list <- reactive({
  stocks_list <- list_stocks() %>% mutate(src = 'stock')
  opts_list <- list_options() %>% mutate(src = 'option')
  futs_list <- list_futures() %>% mutate(src = 'future')
  idx_list <- list_index() %>% mutate(src = 'index')
  
  instrument_list <- stocks_list %>% select(code = ticker, src) %>%
    rbind(opts_list %>% select(code, src)) %>%
    rbind(futs_list %>% select(code, src)) %>% 
    rbind(idx_list %>% select(code, src))
})

obs_list <- list()

observeEvent(input$do_search, {
  withProgress(message = 'Buscando...', value = 0, {
    target_instr <- input$search_terms
    instrument_list <- all_instr_list()
    
    # TODO: Alterar match parcial para olhar mais colunas (empresa, área...)
    if (input$search_type == "exact") {
      wanted <- instrument_list[instrument_list$code == target_instr, ]
    } else {
      wanted <- instrument_list %>% 
        filter(grepl(target_instr, instrument_list$code, ignore.case = T))
    }
    
    if (nrow(wanted) == 0) {
      output$search_result <- renderUI({
        h3(paste0("Nenhum instrumento contendo ", target_instr, " foi encontrado."))
      })
    } else {
      
      output$search_result <- renderUI({
        buttons <- as.list(1:nrow(wanted))
        buttons <- lapply(buttons, function(i) {
          cd <- wanted[i, ]$code
          sr <- wanted[i, ]$src
          nm <- paste0("search_btn_", i)
          obs_list[[nm]] <<- observeEvent(input[[nm]], {
            values$ticker_src <- sr
            values$cur_ticker <- cd
            updateTabItems(session, "tabs", selected = "graphs")
          })
          fluidRow(
            column(6, span(style = "color:blue; font-size: 120%;", cd)), 
            column(2, actionButton(nm, "Ver Dados"))
          )
        })
      })
      
    }
  })
  
})
