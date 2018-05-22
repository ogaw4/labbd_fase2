
get_stocks <- function(ticker, from = NULL, to = NULL) {
  
  dateq <- ''
  if (!is.null(from) && !is.null(to)) {
    dateq <- glue(', "data_ref": {{ "$lte": "{to}", "$gte": "{from}" }}')
  } else if (!(is.null(from) && is.null(to))) {
    dateq <- glue(ifelse(is.null(from), ', "data_ref": {{ "$lte":"{to}"}}', ', "data_ref": {{ "$gte":"{from}"}}'))
  }
  
  query <- '{{"codigoAcao":"{ticker}"{dateq}}}'
  
  results <- MONGO_STOCKS$find(glue(query))
  
  if (nrow(results) == 0) {
    return(data.frame(idAcao = numeric(0), codigoAcao = character(0), data_ref = character(0), 
                      precoAbertura = numeric(0), precoMinimo = numeric(0), precoMedio = numeric(0), 
                      precoFechamento = numeric(0), precoMaximo = numeric(0), oscilacao = numeric(0), 
                      numeroNegocios = numeric(0), quantidadeNegociada = numeric(0), volumeNegociado = numeric(0)))
  }
  
  results
  
}


get_index <- function(ticker, from = NULL, to = NULL) {
  
  dateq <- ''
  if (!is.null(from) && !is.null(to)) {
    dateq <- glue(', "data_ref": {{ "$lte": "{to}", "$gte": "{from}" }}')
  } else if (!(is.null(from) && is.null(to))) {
    dateq <- glue(ifelse(is.null(from), ', "data_ref": {{ "$lte":"{to}"}}', ', "data_ref": {{ "$gte":"{from}"}}'))
  }
  
  query <- '{{"codigoIndice":"{ticker}"{dateq}}}'
  
  results <- MONGO_INDEXES$find(glue(query))
  
  if (nrow(results) == 0) {
    return(data.frame(idIndice = numeric(0), codigoIndice = character(0), data_ref = character(0), 
                      precoAbertura = numeric(0), precoMinimo = numeric(0), precoMedio = numeric(0), 
                      precoFechamento = numeric(0), precoMaximo = numeric(0), oscilacao = numeric(0)))
  }
  
  results
  
}



get_options <- function(ticker, from = NULL, to = NULL) {
  
  dateq <- ''
  if (!is.null(from) && !is.null(to)) {
    dateq <- glue(', "data_ref": {{ "$lte": "{to}", "$gte": "{from}" }}')
  } else if (!(is.null(from) && is.null(to))) {
    dateq <- glue(ifelse(is.null(from), ', "data_ref": {{ "$lte":"{to}"}}', ', "data_ref": {{ "$gte":"{from}"}}'))
  }
  
  query <- '{{"codigoOpcao":"{ticker}"{dateq}}}'
  
  results <- MONGO_OPTIONS$find(glue(query))
  
  if (nrow(results) == 0) {
    return(data.frame(idOpcao = numeric(0), codigoOpcao = character(0), data_ref = character(0), 
                      precoAbertura = numeric(0), precoMinimo = numeric(0), precoMedio = numeric(0), 
                      precoFechamento = numeric(0), precoMaximo = numeric(0), oscilacao = numeric(0), 
                      numeroNegocios = numeric(0), quantidadeNegociada = numeric(0), volumeNegociado = numeric(0), 
                      diasCorridosRest = numeric(0), diasUteisRest = numeric(0), volReferencia = numeric(0), 
                      deltaReferencia = numeric(0), premioReferencia = numeric(0)))
  }
  
  results
  
}


get_futures <- function(ticker, from = NULL, to = NULL) {
  
  dateq <- ''
  if (!is.null(from) && !is.null(to)) {
    dateq <- glue(', "data_ref": {{ "$lte": "{to}", "$gte": "{from}" }}')
  } else if (!(is.null(from) && is.null(to))) {
    dateq <- glue(ifelse(is.null(from), ', "data_ref": {{ "$lte":"{to}"}}', ', "data_ref": {{ "$gte":"{from}"}}'))
  }
  
  query <- '{{"codigoFuturo":"{ticker}"{dateq}}}'
  
  results <- MONGO_FUTURES$find(glue(query))
  
  if (nrow(results) == 0) {
    return(data.frame(idFuturo = numeric(0), codigoFuturo = character(0), data_ref = character(0), 
                      precoAbertura = numeric(0), precoMinimo = numeric(0), precoMedio = numeric(0), 
                      precoFechamento = numeric(0), precoMaximo = numeric(0), oscilacao = numeric(0), 
                      numeroNegocios = numeric(0), quantidadeNegociada = numeric(0), volumeNegociado = numeric(0), 
                      diasCorridosRest = numeric(0), diasUteisRest = numeric(0), volReferencia = numeric(0), 
                      deltaReferencia = numeric(0), precoAjuste = numeric(0)))
  }
  
  results
  
}

