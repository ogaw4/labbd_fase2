

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

all_instr_list <- reactive({
  change <- values$updated_database
  
  instrument_list <- stocks_list() %>% select(code = ticker, src) %>%
    rbind(opts_list() %>% select(code, src)) %>%
    rbind(futs_list() %>% select(code, src)) %>% 
    rbind(idx_list() %>% select(code, src)) %>% 
    rbind(companies_list() %>% select(code = name, src) %>% group_by(code) %>% summarise(src = first(src)))
  
})

companies_list <- reactive({
  change <- values$updated_database
  list_companies() %>% mutate(src = 'company') 
})

stocks_list <- reactive({
  change <- values$updated_database
  list_stocks() %>% mutate(src = 'stock')
})

opts_list <- reactive({
  change <- values$updated_database
  list_options() %>% mutate(src = 'option')
})

futs_list <- reactive({
  change <- values$updated_database
  list_futures() %>% mutate(src = 'future')
})

idx_list <- reactive({
  change <- values$updated_database
  list_index() %>% mutate(src = 'index') 
})

observeEvent(input$do_search, {
  search_instr_home()
})
