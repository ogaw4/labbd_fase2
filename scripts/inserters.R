
bvbg086_insert_handler <- function(fnames, data_ref, dest_dir) {
  
  fname <- paste(dest_dir, "/", fnames[1], sep="")
  
  if(all(file.exists(fname))) {
    
    negDoc <- xmlInternalTreeParse(fname)
    negs <- getNodeSet(negDoc, "//d:PricRpt", c(d="urn:bvmf.217.01.xsd"))
    
    #####################################################
    # Insert stocks 
    #####################################################
    
    stocks <- list_stocks()
    ticker_list <- stocks$ticker
    id_list <- stocks$id
    
    negs_df <- lapply(negs, function(node) {
      ticker <- xmlValue(node[['SctyId']][['TckrSymb']])
      trd_dt <- xmlValue(node[['TradDt']][['Dt']])
      idx <- match(ticker, ticker_list)
      if(!is.na(idx) && as.character(data_ref) == as.character(trd_dt)) {
        attrib <- node[['FinInstrmAttrbts']]
        id_acao <- id_list[idx]
        PREABE <- as.numeric(xmlValue(attrib[['FrstPric']]))
        PREMIN <- as.numeric(xmlValue(attrib[['MinPric']]))
        PREMED <- as.numeric(xmlValue(attrib[['TradAvrgPric']]))
        PREULT <- as.numeric(xmlValue(attrib[['LastPric']]))
        PREMAX <- as.numeric(xmlValue(attrib[['MaxPric']]))
        OSCILA <- as.numeric(xmlValue(attrib[['OscnPctg']]))
        neg1 <- as.numeric(xmlValue(attrib[['RglrTxsQty']]))
        neg1 <- if(is.na(neg1)) 0 else neg1
        neg2 <- as.numeric(xmlValue(attrib[['NonRglrTxsQty']]))
        neg2 <- if(is.na(neg2)) 0 else neg2
        TOTNEG <- neg1 + neg2
        QUATOT <- as.numeric(xmlValue(attrib[['FinInstrmQty']]))
        VOLTOT <- as.numeric(xmlValue(attrib[['NtlFinVol']]))
        data.frame(idAcao=id_acao, codigoAcao = ticker,
                   data_ref=as.Date(data_ref), precoAbertura=PREABE,
                   precoMinimo=PREMIN, precoMedio=PREMED, precoFechamento=PREULT,
                   precoMaximo=PREMAX, oscilacao=OSCILA, numeroNegocios=TOTNEG, 
                   quantidadeNegociada=QUATOT, volumeNegociado=VOLTOT, 
                   stringsAsFactors = FALSE)
      } else NA
    })
    negs_df <- do.call(rbind, negs_df)
    if(!is.null(names(negs_df))) {
      negs_df <- negs_df[!is.na(negs_df$idAcao), ]
      negs_df[is.na(negs_df)] <- 0
      if (nrow(get_stocks(negs_df[1, ]$codigoAcao, from = data_ref, to = data_ref)) > 0) stop("Dados de negociação já foram inseridos para essa data.")
      ins_res <- MONGO_STOCKS$insert(negs_df)
    } else {
      stop("Erro ao inserir histórico de ações")
      return(NULL)
    }
    
    
    #####################################################
    # Insert futures and deriv. opts
    #####################################################
    
    options <- list_options(data_ref)
    futures <- list_futures(data_ref)
    contr <- rbind(options %>% select(id, code), futures %>% select(id, code))
    ticker_list <- contr$code
    id_list <- contr$id
    
    negs_df <- lapply(negs, function(node) {
      ticker <- xmlValue(node[['SctyId']][['TckrSymb']])
      trd_dt <- xmlValue(node[['TradDt']][['Dt']])
      idx <- match(ticker, ticker_list)
      if(!is.na(idx) && as.character(data_ref) == as.character(trd_dt)) {
        attrib <- node[['FinInstrmAttrbts']]
        id_contr <- id_list[idx]
        valor_ponto_contrato <- 0
        volume_reais <- as.numeric(xmlValue(attrib[['NtlFinVol']]))
        volume_dolar <- as.numeric(xmlValue(attrib[['IntlFinVol']]))
        contratos_em_aberto <- as.numeric(xmlValue(attrib[['OpnIntrst']]))
        neg1 <- as.numeric(xmlValue(attrib[['RglrTxsQty']]))
        neg1 <- if(is.na(neg1)) 0 else neg1
        neg2 <- as.numeric(xmlValue(attrib[['NonRglrTxsQty']]))
        neg2 <- if(is.na(neg2)) 0 else neg2
        n_negocios <- neg1 + neg2
        contratos_negociados <- as.numeric(xmlValue(attrib[['FinInstrmQty']]))
        preco_abertura <- as.numeric(xmlValue(attrib[['FrstPric']]))
        preco_minimo <- as.numeric(xmlValue(attrib[['MinPric']]))
        preco_maximo <- as.numeric(xmlValue(attrib[['MaxPric']]))
        preco_medio <- as.numeric(xmlValue(attrib[['TradAvrgPric']]))
        valor_fechamento <- as.numeric(xmlValue(attrib[['LastPric']]))
        qt <- as.numeric(xmlValue(attrib[['AdjstdQt']]))
        qt_tax <- as.numeric(xmlValue(attrib[['AdjstdQtTax']]))
        OSCILA <- as.numeric(xmlValue(attrib[['OscnPctg']]))
        valor_ajuste <- if (is.na(qt)) qt_tax else qt
        
        data.frame(id_contr=id_contr, cod_gts = ticker, data_ref=data_ref, valor_ponto_contrato=valor_ponto_contrato,
                   volume_reais=volume_reais, volume_dolar=volume_dolar, contratos_em_aberto=contratos_em_aberto,
                   n_negocios=n_negocios, contratos_negociados=contratos_negociados, preco_abertura=preco_abertura,
                   preco_minimo=preco_minimo, preco_maximo=preco_maximo, preco_medio=preco_medio,
                   oscila = OSCILA,
                   valor_fechamento=valor_fechamento, valor_ajuste=valor_ajuste, stringsAsFactors = FALSE)
      } else NA
    })
    negs_df <- do.call(rbind, negs_df)
    if(!is.null(names(negs_df))) {
      
      negs_df <- negs_df[!is.na(negs_df$id_contr), ]
      negs_df[is.na(negs_df)] <- 0
      negs_df$data_ref <- as.character(negs_df$data_ref)
      
      
      fname <- paste(dest_dir, "/", fnames[2], sep="")
      if (file.exists(fname)) {
        
        fields_DO <- c(data_ref = 8, cod_merc = 3, tip_merc = 1,
                       serie = 4, vencimento = 8, cod_gts = 20,
                       tip_opc = 1, modelo_opc = 1, ajuste = 1,
                       cod_moeda = 2, strike = 15, vol = 19,
                       delta_sign = 1, delta = 19)
        names_DO <- c('data_ref', 'cod_merc', 'tip_merc', 'serie', 'vencimento',
                      'cod_gts', 'tip_opc', 'modelo_opc', 'ajuste', 'cod_moeda', 'strike',
                      'vol', 'delta_sign', 'delta')
        
        decimais_vol <- 7
        decimais_strike <- 3
        decimais_delta <- 7
        contents_do <- read.fwf(fname, fields_DO)
        names(contents_do) <- names_DO
        contents_do$vol <- as.numeric(contents_do$vol)
        contents_do$strike <- as.numeric(contents_do$strike)
        contents_do$delta <- as.numeric(contents_do$delta)
        contents_do <- contents_do[!is.na(contents_do$vol), ]
        contents_do <- contents_do[!is.na(contents_do$strike), ]
        contents_do <- contents_do[!is.na(contents_do$delta), ]
        contents_do$vol <- contents_do$vol/(10^decimais_vol)
        contents_do$strike <- contents_do$strike/(10^decimais_strike)
        contents_do$delta <- (contents_do$delta/(10^decimais_delta))*ifelse(contents_do$delta_sign == "+", 1, -1)
        contents_do$data_ref <- rep(as.character(data_ref), nrow(contents_do))
        contents_do$vencimento <- as.Date(as.character(contents_do$vencimento), format='%Y%m%d')
        contents_do$cod_gts <- trimws(contents_do$cod_gts)
        contents_do <- subset(contents_do, select=-c(delta_sign, ajuste, cod_moeda))
      } else {
        stop("Arquivo DO (delta de opções) não encontrado")
        return(NULL)
      }
      
      fname <- paste(dest_dir, "/", fnames[3], sep="")
      if (file.exists(fname)) {
        
        fields_RE <- c(id_trans = 6, compl_trans = 3, tipo_reg = 2, data_ref = 8,
                       cod_merc = 3, tip_merc = 1, serie = 4, tip_opc = 1,
                       modelo_opc = 1, vencimento = 8, strike = 15, preco_ref = 15,
                       n_dec = 1)
        
        names_RE <- c('id_trans', 'compl_trans', 'tipo_reg', 'data_ref', 'cod_merc',
                      'tip_merc', 'serie', 'tip_opc', 'modelo_opc', 'vencimento',
                      'strike', 'preco_ref', 'n_dec')
        
        contents_re <- read.fwf(fname, fields_RE)
        names(contents_re) <- names_RE
        contents_re <- contents_re[!is.na(contents_re$preco_ref), ]
        contents_re <- contents_re[!is.na(contents_re$strike), ]
        contents_re$preco_ref <- as.numeric(contents_re$preco_ref)
        contents_re$strike <- as.numeric(contents_re$strike)
        contents_re$preco_ref <- contents_re$preco_ref/(10^contents_re$n_dec)
        contents_re$strike <- contents_re$strike/(10^contents_re$n_dec)
        contents_re$data_ref <- as.Date(as.character(contents_re$data_ref), format='%Y%m%d')
        contents_re$vencimento <- as.Date(as.character(contents_re$vencimento), format='%Y%m%d')
        
        contents_re <- subset(contents_re, select=-c(id_trans, compl_trans, tipo_reg, n_dec))
        
      } else {
        stop("Arquivo RE (prêmio de opções) não encontrado")
        return(NULL)
      }
      
      contents <- merge(contents_re, contents_do)
      contents <- subset(contents, select=c(cod_gts, preco_ref, vol, delta)) %>% 
        filter(cod_gts %in% ticker_list)
      
      futs <- merge(negs_df %>% filter(id_contr %in% futures$id), futures %>% select(id_contr = id, maturity)) %>%
        mutate(dc = as.numeric(as.Date(maturity) - as.Date(data_ref)), 
               du = bizdays(data_ref, maturity))
      opts <- merge(negs_df %>% filter(id_contr %in% options$id), options %>% select(id_contr = id, maturity)) %>%
        merge(contents) %>% 
        mutate(dc = as.numeric(as.Date(maturity) - as.Date(data_ref)), 
               du = bizdays(data_ref, maturity))
      
      
      if (nrow(get_futures(futs[1, ]$cod_gts, from = data_ref, to = data_ref)) > 0) stop("Dados de negociação já foram inseridos para essa data.")
      
      
      ins_res <- MONGO_FUTURES$insert(futs %>% select(data_ref, idFuturo = id_contr, 
                                               codigoFuturo = cod_gts, precoAbertura = preco_abertura, 
                                               precoMinimo = preco_minimo, precoMedio = preco_medio, 
                                               precoMaximo = preco_maximo, precoFechamento = valor_fechamento,
                                               precoAjuste = valor_ajuste, oscilacao = oscila, 
                                               numeroNegocios = n_negocios, quantidadeNegociada = contratos_negociados, 
                                               volumeNegociado = volume_reais, diasCorridosRest = dc, diasUteisRest = du))
      ins_res <- MONGO_OPTIONS$insert(opts %>% select(data_ref, idOpcao = id_contr, 
                                               codigoOpcao = cod_gts, precoAbertura = preco_abertura, 
                                               precoMinimo = preco_minimo, precoMedio = preco_medio, 
                                               precoMaximo = preco_maximo, precoFechamento = valor_fechamento,
                                               oscilacao = oscila, 
                                               numeroNegocios = n_negocios, quantidadeNegociada = contratos_negociados, 
                                               volumeNegociado = volume_reais, diasCorridosRest = dc, diasUteisRest = du, 
                                               volReferencia = vol, deltaReferencia = delta, premioReferencia = preco_ref))
    } else {
      stop("Erro ao inserir histórico de futuros e opções sobre derivativos.")
      return(NULL)
    }
    
    
  } else {
    stop("Arquivo BVBG.086.01 não foi encontrado no diretório")
    return(NULL)
  }
}

bvbg028_insert_handler <- function(fname, data_ref, dest_dir) {
  fname <- paste(dest_dir, "/", fname, sep="")
  
  if(file.exists(fname)) {
    # read file
    cadDoc <- xmlInternalTreeParse(fname)
    
    doc_date <- substr(xmlValue(getNodeSet(cadDoc, "//d:CreDtAndTm", c(d="urn:bvmf.052.01.xsd"))[[1]]), 1, 10)
    if (doc_date != data_ref) {
      free(cadDoc)
      gc()
      stop("Data do arquivo de cadastro BVBG.028 não bate com a data de referência!")
    }
    
    contr_nodes <- getNodeSet(cadDoc, "//d:FinInstrmAttrCmon/parent::*", c(d="urn:bvmf.100.02.xsd"))
    
    options <- list_options(data_ref)
    futures <- list_futures(data_ref)
    stocks <- list_stocks()
    instr <- dbGetQuery(DB_CONNECTION, 'select * from instrumento') %>% select(id, cod_gts = codigo)
    curr_contracts <- rbind(options %>% select(id, cod_gts = code), futures %>% select(id, cod_gts = code), 
                            stocks %>% select(id, cod_gts = ticker))
    
    contr_df <- lapply(contr_nodes, function(node) {
      cod_merc <- xmlValue(node[['FinInstrmAttrCmon']][['Asst']])
      tip_merc <- as.numeric(xmlValue(node[['FinInstrmAttrCmon']][['Mkt']]))
      info <- if (tip_merc %in% c(1, 2, 5)) node[['InstrmInf']][['FutrCtrctsInf']] else node[['InstrmInf']][['OptnOnSpotAndFutrsInf']]
      if (tip_merc == 10) {
        info <- node[['InstrmInf']][['EqtyInf']]
        if (is.null(info)) { NA } else {
          cod_gts <- xmlValue(info[['TckrSymb']])
          if (cod_gts %in% curr_contracts$cod_gts || cod_gts %in% instr$cod_gts) { NA } else {
            tip_opc <- xmlValue(info[['SpcfctnCd']])
            tip_opc <- ifelse(grepl('ON', tip_opc) | grepl('PN', tip_opc), tip_opc, NA)
            internal_id <- xmlValue(node[['FinInstrmId']][['OthrId']][['Id']])
            data.frame(id_underlying=NA, cod_merc=NA, tip_serie=NA,
                       data_ref=data_ref, internal_id = internal_id,
                       tip_merc=NA, indic_tip_opc=NA, tip_opc=tip_opc, vencimento=NA,
                       strike=NA, cod_gts=cod_gts, cod_isin=NA,
                       indic_opc_aj=NA, cod_moeda=NA, descricao=NA, stringsAsFactors = FALSE)
          }
        }
        
      } else {
        if (is.null(info) && tip_merc == 4) info <- node[['InstrmInf']][['DrvsOptnExrcInf']]
        if(is.null(info)) { NA } else {
          cod_gts <- xmlValue(info[['TckrSymb']])
          if (cod_gts %in% curr_contracts$cod_gts || cod_gts %in% instr$cod_gts) { NA } else {
            id_underlying <- xmlValue(info[['UndrlygInstrmId']][['OthrId']][['Id']])
            cod_merc <- substr(cod_gts, 1, 3)
            tip_serie <- substr(cod_gts, 4, 6)
            indic_tip_opc <- xmlValue(info[['OptnTp']]) # PUTT -> V CALL -> C
            tip_opc <- xmlValue(info[['ExrcStyle']]) # EURO -> E AMER -> A
            vencimento <- xmlValue(info[['XprtnDt']])
            strike <- as.numeric(xmlValue(info[['ExrcPric']]))
            if(is.null(strike) || is.na(strike)) strike <- 0
            cod_isin <- xmlValue(info[['ISIN']])
            indic_opc_aj <- 'N'
            cod_moeda <- xmlValue(info[['TradgCcy']]) # BRL -> 2 USD -> 1
            descricao <- xmlValue(node[['FinInstrmAttrCmon']][['Desc']])
            internal_id <- xmlValue(node[['FinInstrmId']][['OthrId']][['Id']])
            
            data.frame(id_underlying=id_underlying, cod_merc=cod_merc, tip_serie=tip_serie,
                       data_ref=data_ref, internal_id = internal_id,
                       tip_merc=tip_merc, indic_tip_opc=indic_tip_opc, tip_opc=tip_opc, vencimento=vencimento,
                       strike=strike, cod_gts=cod_gts, cod_isin=cod_isin,
                       indic_opc_aj=indic_opc_aj, cod_moeda=cod_moeda, descricao=descricao, stringsAsFactors = FALSE)
          }
        }
      }
    })
    contr_df <- do.call(rbind, contr_df)
    if(!is.null(names(contr_df))) {
      
      sdf <- contr_df %>% filter(is.na(vencimento), is.na(tip_merc), grepl('ON', tip_opc) | grepl('PN', tip_opc))
      contr_df <- contr_df %>% filter(!is.na(data_ref), !(tip_merc == 4 & is.na(vencimento))) %>% rbind(sdf)
      contr_df$indic_tip_opc[contr_df$indic_tip_opc == "CALL"] <- 'call'
      contr_df$indic_tip_opc[contr_df$indic_tip_opc == "PUTT"] <- 'put'
      contr_df$tip_opc[contr_df$tip_opc == "AMER"] <- "americana"
      contr_df$tip_opc[contr_df$tip_opc == "EURO"] <- "europeia"
      
      if (nrow(contr_df) == 0) return(TRUE)
      dbWriteTable(DB_CONNECTION, 'instrumento', contr_df %>% select(codigo = cod_gts), append = T, row.names = F)
      
      instr <- dbGetQuery(DB_CONNECTION, 'select * from instrumento') %>% select(id, cod_gts = codigo)
      
      futs <- merge(contr_df %>% filter(is.na(tip_opc) & !is.na(vencimento)), instr)
      futs <- futs[!duplicated(futs[, 'id']), ]
      opts <- merge(contr_df %>% filter(!is.na(tip_opc) & !is.na(vencimento)), instr)
      opts <- opts[!duplicated(opts[, 'id']), ]
      stocks <- merge(contr_df %>% filter(is.na(vencimento), is.na(tip_merc), grepl('ON', tip_opc) | grepl('PN', tip_opc)), instr)
      stocks <- stocks[!duplicated(stocks[, 'id']), ]
      
      dbWriteTable(DB_CONNECTION, 'acao', stocks %>% mutate(cnpj_empresa = NA, tip_opc = substr(tip_opc, 1, 2)) %>% 
                     select(id_instrumento = id, cnpj_empresa, tipo = tip_opc), append = T, row.names = F)
      
      mercs <- list_mercs() %>% select(code, id)
      index <- list_index() %>% select(code, id)
      objs <- rbind(mercs, index) %>% rename(cod_obj = code, id_objeto = id)
      
      futs <- futs %>% mutate(cod_obj = substr(cod_gts, 1, 3)) %>% merge(objs)
      
      dbWriteTable(DB_CONNECTION, 'futuro', futs %>% select(id_instrumento = id, id_objeto, vencimento), append = T, row.names = F)
      
      futures <- list_futures(data_ref)
      
      contracts_id_df <- lapply(contr_nodes, function(node) {
        tip_merc <- as.numeric(xmlValue(node[['FinInstrmAttrCmon']][['Mkt']]))
        info <- if (tip_merc %in% c(1, 2, 5)) node[['InstrmInf']][['FutrCtrctsInf']] else node[['InstrmInf']][['OptnOnSpotAndFutrsInf']]
        if (is.null(info) && tip_merc == 4) info <- node[['InstrmInf']][['DrvsOptnExrcInf']]
        
        if (is.null(info)) {
          NA
        } else {
          ticker <- xmlValue(info[['TckrSymb']])
          int_id <- as.numeric(xmlValue(node[['FinInstrmId']][['OthrId']][['Id']]))
          contr_id <- (futures[futures$code == ticker, ]$id)[1]
          data.frame(ticker=ticker, id_underlying=int_id, contr_id = contr_id, stringsAsFactors = FALSE)
        }
      })
      contracts_id_df <- do.call(rbind, contracts_id_df)
      contracts_id_df <- contracts_id_df[!is.na(contracts_id_df$ticker), ]
      
      opts <- opts %>% merge(contracts_id_df %>% select(id_underlying, contr_id), all.x = T) %>% mutate(id_underlying = contr_id) %>% 
        mutate(id_underlying = ifelse(is.na(id_underlying), id, id_underlying))
      dbWriteTable(DB_CONNECTION, 'opcao', opts %>% select(id_instrumento = id, id_underlying, vencimento, strike,
                                                           tipo = indic_tip_opc, estilo = tip_opc), append = T, row.names = F)
      
      
      rm(cadDoc)
      rm(contr_nodes)
      
    }
    
  } else {
    stop("Arquivo BVBG.028.02 não foi encontrado no diretório")
    return(NULL)
  }
}


bvbg087_insert_handler <- function(fname, data_ref, dest_dir) {
  fname <- paste(dest_dir, "/", fname, sep="")
  
  if(file.exists(fname)) {
    negDoc <- xmlInternalTreeParse(fname)
    negs <- getNodeSet(negDoc, "//d:IndxInf", c(d="urn:bvmf.218.01.xsd"))
    
    #####################################################
    # Insert index hist
    #####################################################
    
    idxs <- list_index()
    ticker_list <- idxs$code
    id_list <- idxs$id
    
    idx_df <- lapply(negs, function(node) {
      snode <- node[['SctyInf']]
      ticker <- xmlValue(snode[['SctyId']][['TckrSymb']])
      idx <- match(ticker, ticker_list)
      if(!is.na(idx)) {
        id_indice <- id_list[idx]
        PREABE <- as.numeric(xmlValue(snode[['OpngPric']]))
        PREMIN <- as.numeric(xmlValue(snode[['MinPric']]))
        PREMED <- as.numeric(xmlValue(snode[['TradAvrgPric']]))
        PREULT <- as.numeric(xmlValue(snode[['ClsgPric']]))
        PREMAX <- as.numeric(xmlValue(snode[['MaxPric']]))
        OSCILA <- as.numeric(xmlValue(snode[['OscnVal']]))*100
        data.frame(id_indice=id_indice, data_ref=data_ref, ticker = ticker, PREABE=PREABE, PREMIN=PREMIN, PREMED=PREMED, PREULT=PREULT,
                   PREMAX=PREMAX, OSCILA=OSCILA, stringsAsFactors = FALSE)
      } else NA
    })
    idx_df <- do.call(rbind, idx_df)
    if(!is.null(names(idx_df))) {
      
      idx_df <- idx_df[!is.na(idx_df$id_indice), ]
      idx_df[is.na(idx_df)] <- 0
      if (nrow(get_index(idx_df[1, ]$ticker, from = data_ref, to = data_ref)) > 0) { 
        stop("Dados de negociação já foram inseridos para essa data.")
        rm(negs)
        rm(negDoc)
      }
      
      ins_res <- MONGO_INDEXES$insert(idx_df %>% select(data_ref, idIndice = id_indice, 
                                                 codigoIndice = ticker, precoAbertura = PREABE, 
                                                 precoMinimo = PREMIN, precoMedio = PREMED, 
                                                 precoMaximo = PREMAX, precoFechamento = PREULT,
                                                 oscilacao = OSCILA))
      rm(negs)
      rm(negDoc)
    } else {
      stop("Erro ao inserir histórico de índices")
      return(NULL)
    }
    
    
  } else {
    stop("Arquivo BVBG.087.01 não foi encontrado no diretório")
    return(NULL)
  }
  
}


bvbgopc_insert_handler <- function(fnames, data_ref, dest_dir) {
  
  fname <- paste(dest_dir, "/", fnames[1], sep="")
  
  if(file.exists(fname)) {
    cadDoc <- xmlInternalTreeParse(fname)
    cad <- getNodeSet(cadDoc, "//d:OptnOnEqtsInf", c(d="urn:bvmf.100.02.xsd"))
    
    stocks <- list_stocks()
    ticker_list <- stocks$ticker
    id_list <- stocks$id
    
    # Prepare stocks internal id list
    sto <- getNodeSet(cadDoc, "//d:EqtyInf", c(d="urn:bvmf.100.02.xsd"))
    stocks_df <- lapply(sto, function(node) {
      ticker <- xmlValue(node[['TckrSymb']])
      par <- xmlParent(xmlParent(node))
      int_id <- as.numeric(xmlValue(par[['FinInstrmId']][['OthrId']][['Id']]))
      data.frame(ticker=ticker, int_id=int_id, stringsAsFactors = FALSE)
    })
    stocks_df <- do.call(rbind, stocks_df)
    sto_df_ids <- stocks_df$int_id
    sto_df_tcks <- stocks_df$ticker
    
    #####################################################
    # Insert stock options info
    #####################################################
    opts_df <- lapply(cad, function(node) {
      underlying_id <- as.numeric(xmlValue(node[['UndrlygInstrmId']][['OthrId']][['Id']]))
      # find underlying
      idxtckr <- match(underlying_id, sto_df_ids)
      underlying_ticker <- sto_df_tcks[idxtckr]
      
      idx <- match(underlying_ticker, ticker_list)
      if(!is.na(idx)) {
        id_acao <- id_list[idx]
        cod_opcao <- xmlValue(node[['TckrSymb']])
        data_vcto <- xmlValue(node[['XprtnDt']])
        tipo_opc <- xmlValue(node[['OptnTp']])
        tipo_instr <- xmlValue(node[['OptnStyle']])
        strike <- as.numeric(xmlValue(node[['ExrcPric']]))
        instrm <- xmlParent(xmlParent(node))
        internal_id <- as.numeric(xmlValue(instrm[['FinInstrmId']][['OthrId']][['Id']]))
        data.frame(id_acao=id_acao, underlying = underlying_ticker, cod_opcao=cod_opcao, data_vcto=data_vcto, tipo_opc=tipo_opc, tipo_instr=tipo_instr,
                   strike=strike, internal_id=internal_id, data_ref=data_ref, stringsAsFactors = FALSE)
      } else NA
      
    })
    
    opts_df <- do.call(rbind, opts_df)
    
    if(!is.null(names(opts_df))) {
      
      opts_df <- opts_df[!is.na(opts_df$data_ref), ] %>% filter(data_vcto >= data_ref) %>%
        mutate(cod_opcao = paste0(cod_opcao, '_', substr(cod_opcao, 5, 5), substr(as.character(data_vcto), 3, 4)))
      opts_df$tipo_opc[opts_df$tipo_opc == "CALL"] <- "call"
      opts_df$tipo_opc[opts_df$tipo_opc == "PUTT"] <- "put"
      opts_df$tipo_instr[opts_df$tipo_instr == "AMER"] <- "americana"
      opts_df$tipo_instr[opts_df$tipo_instr == "EURO"] <- "europeia"
      instr <- dbGetQuery(DB_CONNECTION, 'select * from instrumento') %>% select(id, cod_opcao = codigo)
      
      curr_opts <- list_options(data_ref) %>% select(-strike)
      names(curr_opts) <- c('cod_opcao', 'id_opcao', 'data_vcto', 'tipo_opc', 'tipo_instr', 'underlying')
      keys <- c('underlying', 'cod_opcao', 'data_vcto', 'tipo_opc', 'tipo_instr')
      new_opts <- merge(opts_df, curr_opts %>% select(cod_opcao, id_opcao), all.x=TRUE)
      new_opts <- new_opts[which(is.na(new_opts$id_opcao)) , c('id_acao', keys, 'strike') ] %>% filter(!(cod_opcao %in% instr$cod_opcao))
      
      if (nrow(new_opts) > 0) {
        dbWriteTable(DB_CONNECTION, 'instrumento', new_opts %>% select(codigo = cod_opcao), append = T, row.names = F)
        instr <- dbGetQuery(DB_CONNECTION, 'select * from instrumento') %>% select(id, cod_opcao = codigo)
        opts <- merge(new_opts, instr)
        opts <- opts[!duplicated(opts[, 'id']), ]
        dbWriteTable(DB_CONNECTION, 'opcao', opts %>% select(id_instrumento = id, id_underlying = id_acao, 
                                                             vencimento = data_vcto, strike = strike, 
                                                             tipo = tipo_opc, estilo = tipo_instr), append = T, row.names = F)
        curr_opts <- list_options(data_ref) %>% select(-strike)
        names(curr_opts) <- c('cod_opcao', 'id_opcao', 'data_vcto', 'tipo_opc', 'tipo_instr', 'underlying')
      }
      
      authorized_opts <- merge(opts_df, curr_opts %>% select(cod_opcao, id_opcao))
      authorized_opts <- authorized_opts[!is.na(authorized_opts$id_opcao), ]
      
    } else {
      stop("Erro ao cadastrar opções de ações.")
      return(NULL)
    }
    
    
  } else {
    stop("Arquivo BVBG028 não encontrado")
    return(NULL)
  }
  
  
  
  fname <- paste(dest_dir, "/", fnames[2], sep="")
  
  if(file.exists(fname)) {
    # read file
    negDoc <- xmlInternalTreeParse(fname)
    negs <- getNodeSet(negDoc, "//d:PricRpt", c(d="urn:bvmf.217.01.xsd"))
    
    #####################################################
    # Insert stock options
    #####################################################
    
    internal_id_list <- authorized_opts$internal_id
    id_list <- authorized_opts$id_opcao
    cod_list <- authorized_opts$cod_opcao
    strikes <- authorized_opts$strike
    
    negs_df <- lapply(negs, function(node) {
      option_id <- as.numeric(xmlValue(node[['FinInstrmId']][['OthrId']][['Id']]))
      trd_dt <- xmlValue(node[['TradDt']][['Dt']])
      idx <- match(option_id, internal_id_list)
      if(!is.na(idx) && as.character(data_ref) == as.character(trd_dt)) {
        id_opcao <- id_list[idx]
        cod_opcao <- cod_list[idx]
        attrib <- node[['FinInstrmAttrbts']]
        PREABE <- as.numeric(xmlValue(attrib[['FrstPric']]))
        PREMIN <- as.numeric(xmlValue(attrib[['MinPric']]))
        PREMED <- as.numeric(xmlValue(attrib[['TradAvrgPric']]))
        PREULT <- as.numeric(xmlValue(attrib[['LastPric']]))
        PREMAX <- as.numeric(xmlValue(attrib[['MaxPric']]))
        OSCILA <- as.numeric(xmlValue(attrib[['OscnPctg']]))
        neg1 <- as.numeric(xmlValue(attrib[['RglrTxsQty']]))
        neg1 <- if(is.na(neg1)) 0 else neg1
        neg2 <- as.numeric(xmlValue(attrib[['NonRglrTxsQty']]))
        neg2 <- if(is.na(neg2)) 0 else neg2
        TOTNEG <- neg1 + neg2
        QUATOT <- as.numeric(xmlValue(attrib[['FinInstrmQty']]))
        VOLTOT <- as.numeric(xmlValue(attrib[['NtlFinVol']]))
        data.frame(id_opcao=id_opcao, cod_opcao = cod_opcao, data_ref=data_ref, PREABE=PREABE, PREMIN=PREMIN, PREMED=PREMED, PREULT=PREULT,
                   PREMAX=PREMAX, OSCILA=OSCILA, TOTNEG=TOTNEG, QUATOT=QUATOT,
                   VOLTOT=VOLTOT, strike=strikes[idx], stringsAsFactors = FALSE)
      } else NA
    })
    negs_df <- do.call(rbind, negs_df)
    if(!is.null(names(negs_df))) {
      
      negs_df <- negs_df[!is.na(negs_df$id_opcao), ]
      negs_df[is.na(negs_df)] <- 0
      negs_df$data_ref <- as.character(negs_df$data_ref)
      
      fname <- paste(dest_dir, "/", fnames[3], sep="")
      if (file.exists(fname)) {
        contents <- read.csv(fname, header=FALSE, sep=";", skip=1, stringsAsFactors = FALSE)
        names(contents) <- c('cod_opcao', 'tipo_opc', 'tipo_instr', 'data_vcto',
                             'strike', 'preco_ref', 'vol')
        
        contents$data_vcto <- as.Date(as.character(contents$data_vcto), format='%Y%m%d')
        contents$tipo_opc <- ifelse(contents$tipo_opc == "C", "call", "put")
        contents$tipo_instr <- ifelse(contents$tipo_instr == "A", "americana", "europeia")
        contents <- contents %>%
          mutate(cod_opcao = paste0(cod_opcao, '_', substr(cod_opcao, 5, 5), substr(as.character(data_vcto), 3, 4)))
        
      } else {
        stop("Arquivo PE (prêmio de opções de ações) não encontrado")
        return(NULL)
      }
      negs_df <- negs_df %>% merge(contents) %>% mutate(dc = as.numeric(as.Date(data_vcto) - as.Date(data_ref)),
                                                        du = bizdays(data_ref, data_vcto), delta = 0)
      
      
      if (nrow(get_options(negs_df[1, ]$cod_opcao, from = data_ref, to = data_ref)) > 0) stop("Dados de negociação já foram inseridos para essa data.")
      
      ins_res <- MONGO_OPTIONS$insert(negs_df %>% select(data_ref, idOpcao = id_opcao, 
                                                  codigoOpcao = cod_opcao, precoAbertura = PREABE, 
                                                  precoMinimo = PREMIN, precoMedio = PREMED, 
                                                  precoMaximo = PREMAX, precoFechamento = PREULT,
                                                  oscilacao = OSCILA, 
                                                  numeroNegocios = TOTNEG, quantidadeNegociada = QUATOT, 
                                                  volumeNegociado = VOLTOT, diasCorridosRest = dc, diasUteisRest = du, 
                                                  volReferencia = vol, deltaReferencia = delta, premioReferencia = preco_ref))
      
    } else {
      stop("Erro ao inserir histórico de opções de ações.")
      return(NULL)
    }
    
  } else {
    stop("Arquivo BVBG086 não encontrado")
    return(NULL)
  }
  
}

fix_number <- function(n) {
  
  n1 <- str_replace_all(str_replace_all(n, "\\.|>|<", ""), ",", ".")
  
  sig <- 1 
  
  if (any(unlist(str_detect(n1, "\\(|\\)")))) sig <- -1 
  
  sig * as.numeric(str_replace_all(n1, "\\(|\\)", ""))
}

company_insert <- function() {
  
  company_dir <- file.path(dest_dir, 'company') 
  
  json_files <- list.files(company_dir, full.names = T) 
  stocks_list <- list_stocks() 
  
  if (length(json_files) == 0) stop("Dados de empresas não encontrados") 
  
  dbBegin(DB_CONNECTION)
  companies <- lapply(json_files, function(fname) {
    
    comp_data <- tryCatch( { read_json(fname) }, error = function(e) NULL)
    if (is.null(comp_data) || nchar(comp_data$cnpj) <= 5) {NULL} else {
      name <- comp_data$nome 
      site <- comp_data$site
      cnpj <- as.numeric(str_remove_all(comp_data$cnpj, "\\.|/|-"))
      setores <- "Indisponivel"
      atividade_principal <- "Indisponiivel"
      tickers <- unlist(comp_data$ticker)
      
      base_query <- paste("INSERT INTO empresa (cnpj, nome, setores, atividade_principal, site)",
                    "VALUES ({cnpj}, '{name}', '{setores}', '{atividade_principal}', '{site}') ON CONFLICT (cnpj) DO UPDATE", 
                    "SET nome = EXCLUDED.nome, setores = EXCLUDED.setores, atividade_principal = EXCLUDED.atividade_principal,",
                    "site = EXCLUDED.site;")
      
      res <- dbSendQuery(DB_CONNECTION, glue(base_query))
      
      
      if (!is.null(tickers))  {
        for (t in tickers) {
          if (t %in% stocks_list$ticker) {
            stock_id <- first((stocks_list %>% filter(ticker == t))$id)
            res <- dbSendQuery(DB_CONNECTION, glue('UPDATE acao SET cnpj_empresa = {cnpj} WHERE id_instrumento = {stock_id}'))
          }
        }
      }
      
      
      balanco <- comp_data$balanco_patrimonial
      composicao <- comp_data$composicao_capital_social
      
      if (length(balanco$periodo > 0)) {
        res <- dbSendQuery(DB_CONNECTION, glue('INSERT INTO relatorio (cnpj_empresa) VALUES ({cnpj});'))
        rid <- first(dbGetQuery(DB_CONNECTION, glue('SELECT * FROM relatorio WHERE cnpj_empresa = {cnpj} ORDER BY id DESC LIMIT 1;'))$id)
        
        dt <- as.Date(str_replace_all(balanco$periodo[2], ">|<", ""), format = "%d/%m/%Y")
        
        
        res <- dbSendQuery(DB_CONNECTION, glue("INSERT INTO relatorio_financeiro (id_relatorio, data) VALUES ({rid}, '{dt}');"))
        
        perm <- fix_number(balanco$ativo_permanente[2])
        tot <- fix_number(balanco$ativo_total[2])
        liq <- fix_number(balanco$patromonio_liquido[2])
        on <- fix_number(composicao$ordinarias[1])
        pn <- fix_number(composicao$preferenciais[1])
        
        if (length(perm) == 0 || is.na(perm)) perm <- 0
        if (length(tot) == 0 || is.na(tot)) tot <- 0
        if (length(liq) == 0 || is.na(liq)) liq <- 0
        if (length(on) == 0 || is.na(on)) on <- 0
        if (length(pn) == 0 || is.na(pn)) pn <- 0
        
        
        
        res <- dbSendQuery(DB_CONNECTION, glue("INSERT INTO balanco_patrimonial (id_relatorio, ativo_total, ativo_imobilizado, patrimonio_atribuido, patrimonio_liquido) VALUES ({rid}, {tot}, {perm}, 0, {liq});"))
        
        res <- dbSendQuery(DB_CONNECTION, glue("INSERT INTO capital_social (id_relatorio, ordinario, preferencial) VALUES ({rid}, {on}, {pn});"))
        
        
      }
    
    }
    
    
  })
  
  dbCommit(DB_CONNECTION)
  
}

