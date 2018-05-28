
list_stocks <- function() {
  stocks <- dbGetQuery(DB_CONNECTION, 
               paste(
                 "SELECT i.codigo as ticker, i.id as id, a.tipo as type, a.cnpj_empresa as cnpj", 
                 "FROM acao as a, instrumento as i", 
                 "WHERE a.id_instrumento = i.id"))
  
  if (nrow(stocks) == 0) {
    return(data.frame(ticker = character(0), id = numeric(0), tipo = character(0), cnpj = numeric(0), 
                      nome = character(0)))
  }
  
  companies <- dbGetQuery(DB_CONNECTION, 
                          "SELECT * FROM empresa")
  
  if (nrow(companies) == 0) {
    companies <- data.frame(cnpj = numeric(0), setores = character(0), atividade_principal = character(0), site = character(0), nome = character(0))
  }
  
  merge(stocks, companies, all.x = T) %>% select(-setores, -atividade_principal, -site)
}


list_futures <- function(maturity_filter = NULL) {
  
  original_query <- paste('SELECT i.codigo as code, i.id as id, f.id_objeto as id_obj, f.vencimento as maturity', 
                           'FROM futuro as f, instrumento as i', 
                          'WHERE f.id_instrumento = i.id')
  if(!is.null(maturity_filter)) original_query <- paste0(original_query, " AND f.vencimento >= '", maturity_filter, "'")
  
  futs <- dbGetQuery(DB_CONNECTION, original_query)
  
  if (nrow(futs) == 0) {
    return(data.frame(code = character(0), id = numeric(0), objeto = character(0), maturity = character(0)))
  }
  
  mercs <- dbGetQuery(DB_CONNECTION, paste('SELECT id_instrumento as id_obj, codigo as objeto', 
                                            'FROM mercadoria, instrumento', 
                                            'WHERE id = id_instrumento'))
  indices <- dbGetQuery(DB_CONNECTION, paste('SELECT id_instrumento as id_obj, codigo as objeto', 
                                              'FROM indice, instrumento', 
                                              'WHERE id = id_instrumento'))
  
  mrc_fut <- futs %>% merge(mercs)
  idc_fut <- futs %>% merge(indices)
  
  rbind(mrc_fut, idc_fut) %>% select(-id_obj)
  
}

list_options <- function(maturity_filter = NULL) {
  
  original_query <- paste('SELECT i.codigo as code, i.id as id, f.id_underlying as id_underlying, f.vencimento as maturity,',
                           'f.strike as strike, f.tipo as type, f.estilo as model',
                           'FROM opcao as f, instrumento as i', 
                          'WHERE f.id_instrumento = i.id')
  if(!is.null(maturity_filter)) original_query <- paste0(original_query, " AND f.vencimento >= '", maturity_filter, "'")
  
  options <- dbGetQuery(DB_CONNECTION, original_query)
  if (nrow(options) == 0) {
    return(data.frame(code = character(0), id = numeric(0), maturity = character(0), strike = numeric(0), type = character(0), 
                      model = numeric(0), cod_underlying = character(0)))
  }
  
  futs <- list_futures(maturity_filter) %>% select(id_underlying = id, cod_underlying = code)
  stocks <- list_stocks() %>% select(id_underlying = id, cod_underlying = ticker)
  
  fut_opt <- options %>% merge(futs)
  sto_opt <- options %>% merge(stocks)
  
  rbind(fut_opt, sto_opt) %>% select(-id_underlying)
  
}

list_mercs <- function() {
  dbGetQuery(DB_CONNECTION, 
                       paste(
                         "SELECT i.codigo as code, i.id as id, a.descricao as desc, a.tipo as tipo", 
                         "FROM mercadoria as a, instrumento as i", 
                         "WHERE a.id_instrumento = i.id"))
}


list_index <- function() {
  dbGetQuery(DB_CONNECTION, 
             paste(
               "SELECT i.codigo as code, i.id as id, a.descricao as desc", 
               "FROM indice as a, instrumento as i", 
               "WHERE a.id_instrumento = i.id"))
}

list_companies <- function() {
  dbGetQuery(DB_CONNECTION, 
             paste(
               "SELECT e.cnpj as cnpj, e.nome as name, e.setores as sectors, e.atividade_principal as actv, e.site as site, i.codigo as ticker",
               "FROM empresa as e, instrumento as i, acao as a", 
               "WHERE e.cnpj = a.cnpj_empresa", 
               "AND a.id_instrumento = i.id"
             ))
}