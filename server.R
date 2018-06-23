
shinyServer(function(input, output, session) {
  
  values <- reactiveValues(execution_log = "", update_files = FALSE, 
                           cur_ticker = NULL, updated_database = FALSE)
  
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
