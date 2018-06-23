
shinyServer(function(input, output, session) {
  
  values <- reactiveValues(execution_log = "", update_files = FALSE, 
                           cur_ticker = NULL, updated_database = FALSE)
  
  
  obs_list <- list()
  
  observeEvent(input$key_pressed, {
    if (input$key_pressed == 13) {
      if (input$tabs == 'home') {
        search_instr_home()
      } else if (input$tabs == 'graphs') {
        search_instr_graphs()
      }
    }
  })
  
  search_instr_graphs <- function() {
    
    withProgress(message = 'Buscando...', value = 0, {
      target_instr <- input$qsearch_terms
      instrument_list <- all_instr_list()
      
      wanted <- instrument_list %>% 
        filter(toupper(instrument_list$code) == toupper(target_instr))
      
      if (nrow(wanted) == 0) {
        updateTextInput(session, 'qsearch_terms', value = paste0(target_instr, ' não encontrado!'))
      } else {
        
        values$ticker_src <- wanted[1, ]$src
        values$cur_ticker <- wanted[1, ]$code
        
      }
    })
  }
  
  search_instr_home <- function() {
    
    withProgress(message = 'Buscando...', value = 0, {
      target_instr <- input$search_terms
      instrument_list <- all_instr_list()
      
      wanted <- instrument_list %>% 
        filter(grepl(target_instr, instrument_list$code, ignore.case = T))
      
      
      if (nrow(wanted) == 0) {
        output$search_result <- renderUI({
          h3(paste0("Nenhum instrumento contendo ", target_instr, " foi encontrado."))
        })
      } else {
        
        
        output$search_result <- renderUI({
          dict <- c(stock = "Ação", option = "Opção", future = "Futuro", index = "Índice", company = "Empresa")
          buttons <- as.list(1:nrow(wanted))
          buttons <- lapply(buttons, function(i) {
            cd <- wanted[i, ]$code
            sr <- wanted[i, ]$src
            nm <- paste0("search_btn_", i)
            obs_list[[nm]] <<- observeEvent(input[[nm]], {
              updateTextInput(session, 'qsearch_terms', value = paste0(cd))
              updateTabItems(session, "tabs", selected = "graphs")
              values$ticker_src <- sr
              values$cur_ticker <- cd
            })
            
            div(style = "margin-top:1em", 
                fluidRow(
                  column(3, span(style = "color:rgb(66, 134, 244); font-size: 150%;", cd)), 
                  column(3, span(style = "color:rgb(66, 134, 244); font-size: 150%;", dict[sr])),
                  column(2, actionButton(nm, "Ver Dados"))
                )
            )
          })
        })
        
      }
    })
    
  }
  
  server_env <- environment()
  
  lapply(list.files("controllers/"),
         function(x)  { 
           source(paste0("controllers/", x), local = server_env, encoding = 'UTF-8') 
        })
  
  cancel.onSessionEnded <- session$onSessionEnded(function() {
    dbDisconnect(DB_CONNECTION)
  })
  
  session$onSessionEnded(stopApp)
  
})
