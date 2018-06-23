
update_company <- function(old, new) {
  
  cur_data <- dbGetQuery(DB_CONNECTION, glue("SELECT * FROM empresa where nome = '{old}';"))
  
  cur_cnpj <- cur_data$cnpj
  
  dbSendQuery(DB_CONNECTION, glue("UPDATE empresa SET nome = '{new}' WHERE cnpj = {cur_cnpj};"))
  
}