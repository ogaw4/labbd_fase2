gen_main_plot <- function(instr, src, dr, plot_type) {
  
  switch(src, 
         index = {gen_index_plot(instr, dr, plot_type)},
         stock = {gen_stock_plot(instr, dr, plot_type)}, 
         option = {gen_option_plot(instr, dr, plot_type)}, 
         future = {gen_future_plot(instr, dr, plot_type)})
}

gen_balance_plot <- function(cnpj, name) {
  reports <- get_company_report(cnpj) 
  dt <- reports$report_date
  balance <- reports$balance
  
  .df <- data.frame(`Valor` = c(balance$ativo_total, balance$ativo_imobilizado, balance$patrimonio_liquido), 
                    `Tipo` = c("Ativo total", "Ativo Imobilizado", "Patrimônio Líquido"))
  
  colours <- ifelse(.df$Valor < 0, 'rgba(222, 45, 38, 0.9)', 'rgba(51, 204, 51, 0.9)')
  
  p <- plot_ly(.df, y = ~`Valor`, x = ~Tipo, type = 'bar', marker = list(color = colours)) %>% 
    plotly::layout(title = paste0("Balanço Patrimonial - ", name, " - ", as.character(dt)))
  
  p$elementId <- NULL
  p
  
}

gen_capital_plot <- function(cnpj, name) {
  reports <- get_company_report(cnpj) 
  
  dt <- reports$report_date
  capital <- reports$capital
  
  .df <- data.frame(Quantidade = c(capital$ordinario, capital$preferencial), Tipo = c("Ordinário", "Preferencial"))
  
  p <- plot_ly(.df, labels=~Tipo, values=~Quantidade, type = 'pie', 
               marker = list(colors = c('rgb(255, 204, 102)', 'rgb(51, 153, 255)')),
               textposition = 'inside', 
               textinfo = 'label+percent') %>% 
    plotly::layout(title = paste0("Composição do Capital Acionário - ", name, " - ", as.character(dt)))
  
  p$elementId <- NULL
  p
  
}


gen_index_plot <- function(instr, dr, plot_type) {
  
  
  dd <- get_index(instr, from = dr[1], to = dr[2]) %>% 
    mutate(Data = as.Date(data_ref)) %>% 
    select(Data, precoAbertura, precoMinimo, precoMedio, precoMaximo, 
           precoFechamento)
  title <- paste0("Preços do índice - ", instr)
  
  if (nrow(dd) == 0) return(NULL)
  
  switch(plot_type, 
         line = {
           dd <- melt(dd, id.vars = 'Data', variable.name = 'Preço', value.name = 'Valor')
           dd[dd == 0] <- NA
           
           p <- suppressMessages(ggplotly(ggplot(dd, aes(x = Data, y = Valor, color = `Preço`)) +
                                            geom_point() + geom_line() + ggtitle(title)))
           
           p$elementId <- NULL
           p
         }, 
         candle = {
           gen_idx_candle_plot(dd, title)
         })
  
}

gen_stock_plot <- function(instr, dr, plot_type) {
  
  dd <- get_stocks(instr, from = dr[1], to = dr[2]) %>% 
    mutate(Data = as.Date(data_ref))  %>% 
    select(Data, precoAbertura, precoMinimo, precoMedio, precoMaximo, 
           precoFechamento, volumeNegociado)
  title <- paste0("Dados de negociação - ", instr)
  if (nrow(dd) == 0) return(NULL)
  
  switch(plot_type, 
         line = {
           dd <- melt(dd, id.vars = 'Data', variable.name = 'Preço', value.name = 'Valor')
           dd[dd == 0] <- NA
           
           p <- suppressMessages(ggplotly(ggplot(dd %>% filter(`Preço` != 'volumeNegociado'), aes(x = Data, y = Valor, color = `Preço`)) +
                                            geom_point() + geom_line() + ggtitle(title)))
           
           p$elementId <- NULL
           p
         }, 
         candle = {
           gen_candle_plot(dd, title, 'stock')
         })
  
}



gen_option_plot <- function(instr, dr, plot_type) {
  
  dd <- get_options(instr, from = dr[1], to = dr[2]) %>% 
    mutate(Data = as.Date(data_ref)) %>% 
    select(Data, precoAbertura, precoMinimo, precoMedio, precoMaximo, 
           precoFechamento, volumeNegociado, Referencia = premioReferencia)
  title <- paste0("Dados de negociação - ", instr)
  if (nrow(dd) == 0) return(NULL)
  
  switch(plot_type, 
         line = {
           dd <- melt(dd, id.vars = 'Data', variable.name = 'Preço', value.name = 'Valor')
           dd[dd == 0] <- NA
           p <- suppressMessages(ggplotly(ggplot(dd %>% filter(`Preço` != 'volumeNegociado'), aes(x = Data, y = Valor, color = `Preço`)) +
                                            geom_point() + geom_line() + ggtitle(title)))
           
           p$elementId <- NULL
           p
         }, 
         candle = {
           gen_candle_plot(dd, title, 'option')
         })
  
}


gen_future_plot <- function(instr, dr, plot_type) {
  
  dd <- get_futures(instr, from = dr[1], to = dr[2]) %>% 
    mutate(Data = as.Date(data_ref)) %>% 
    select(Data, precoAbertura, precoMinimo, precoMedio, precoMaximo, 
           precoFechamento, volumeNegociado, Referencia = precoAjuste)
  title <- paste0("Dados de negociação - ", instr)
  if (nrow(dd) == 0) return(NULL)
  
  switch(plot_type, 
         line = {
           dd <- melt(dd, id.vars = 'Data', variable.name = 'Preço', value.name = 'Valor')
           dd[dd == 0] <- NA
           p <- suppressMessages(ggplotly(ggplot(dd %>% filter(`Preço` != 'volumeNegociado'), aes(x = Data, y = Valor, color = `Preço`)) +
                                            geom_point() + geom_line() + ggtitle(title)))
           
           p$elementId <- NULL
           p
         }, 
         candle = {
           gen_candle_plot(dd, title, 'future')
         })
  
}


gen_candle_plot <- function(dd, title, instr_type) {
  
  do_volumes <- FALSE
  
  if (!all((dd$volumeNegociado == 0))) {
    dd[dd == 0] <- NA
    min_vol <- min(dd$volumeNegociado, na.rm = T)
    max_vol <- max(dd$volumeNegociado, na.rm = T)
    min_price <- ifelse(min(dd$precoMinimo, na.rm = T) == 0, mean(dd$precoMinimo, na.rm = T), min(dd$precoMinimo, na.rm = T))
    lower_bound <- min_price - mean(dd$precoMaximo, na.rm = T)/10
    vol_normalizer <- (min_price - lower_bound)/(max_vol - min_vol)
    do_volumes <- TRUE
  }
  
  
  dd$Data <- as.POSIXct(as.character(dd$Data))
  dd$change <- ifelse(dd$precoFechamento > dd$precoAbertura, 
                      "up", 
                      ifelse(dd$precoFechamento < dd$precoAbertura, "down", "flat"))
  dd$width <- 86400 # 1 dia
  clrs <- c(`down` = rgb(192, 0, 0, alpha = 255, maxColorValue = 255), 
            `up` = rgb(0, 192, 0, alpha = 255, maxColorValue = 255), 
            `flat` = rgb(0, 0, 192, alpha = 255, maxColorValue = 255))
  
  plt <- ggplot(dd, aes(x = Data, group = 1,
                        text =  sprintf("Mínimo: %s<br>Máximo: %s<br>Abertura: %s<br>Fechamento: %s<br>Volume: R$ %s<br>Data: %s",
                                        precoMinimo, precoMaximo, precoAbertura, precoFechamento,
                                        format(volumeNegociado, big.mark = ".", decimal.mark = ","), format(Data, "%Y-%m-%d"))), 
                alpha = 1) +
    geom_linerange(aes(ymin = precoMinimo, ymax = precoMaximo, colour = change, alpha = 1)) +
    labs(title = title) +
    geom_rect(aes(xmin = Data - width/2 * 0.9, xmax = Data + width/2 * 0.9, 
                  ymin = pmin(precoAbertura, precoFechamento), ymax = pmax(precoAbertura, precoFechamento),
                  fill = change), 
              alpha = 1) + 
    guides(fill = F, colour = F) + 
    scale_color_manual(values = clrs) +
    scale_fill_manual(values = clrs) + 
    ylab('Valor')
  
  if (do_volumes) plt <- plt + geom_rect(aes(xmin = Data - width/2 * 0.9, xmax = Data + width/2 * 0.9, 
                                ymin = lower_bound, ymax = lower_bound + (volumeNegociado - min_vol) * vol_normalizer,
                                fill = change), 
                            alpha = 0.5) 
  
  if (any(dd$change == "flat")) {
    plt <- plt +
      geom_segment(data = dd[dd$change == "flat",], 
                   aes(x = Data - width/2 * 0.9, y = precoFechamento, yend = precoFechamento,
                       xend = Data + width/2 * 0.9, colour = change),
                   alpha = 1)
  } 
  
  if ('Referencia' %in% names(dd)) {
    ref_text <- ifelse(instr_type == 'option', 'Referência', 'Ajuste')
    plt <- suppressWarnings(plt + geom_line(aes(x = Data, y = Referencia), alpha = 0.5, size = 0.5) +
      geom_point(aes(x = Data, y = Referencia, 
                                 text = sprintf('%s: %s', 
                                                ref_text, 
                                                Referencia)), alpha = 0.5, size = 0.5))
  }
  
  p <- suppressMessages(ggplotly(plt, tooltip = 'text') %>% layout(showlegend = F))
  p$elementId <- NULL
  p
}



gen_idx_candle_plot <- function(dd, title) {
  dd[dd == 0] <- NA
  
  dd$Data <- as.POSIXct(as.character(dd$Data))
  dd$change <- ifelse(dd$precoFechamento > dd$precoAbertura, 
                      "up", 
                      ifelse(dd$precoFechamento < dd$precoAbertura, "down", "flat"))
  dd$width <- 86400 # 1 dia
  clrs <- c(`down` = rgb(192, 0, 0, alpha = 255, maxColorValue = 255), 
            `up` = rgb(0, 192, 0, alpha = 255, maxColorValue = 255), 
            `flat` = rgb(0, 0, 192, alpha = 255, maxColorValue = 255))
  
  plt <- ggplot(dd, aes(x = Data, group = 1,
                        text =  sprintf("Mínimo: %s<br>Máximo: %s<br>Abertura: %s<br>Fechamento: %s<br>Data: %s",
                                        precoMinimo, precoMaximo, precoAbertura, precoFechamento, format(Data, "%Y-%m-%d"))), 
                alpha = 1) +
    geom_linerange(aes(ymin = precoMinimo, ymax = precoMaximo, colour = change, alpha = 1)) +
    labs(title = title) +
    geom_rect(aes(xmin = Data - width/2 * 0.9, xmax = Data + width/2 * 0.9, 
                  ymin = pmin(precoAbertura, precoFechamento), ymax = pmax(precoAbertura, precoFechamento),
                  fill = change), 
              alpha = 1) +
    guides(fill = F, colour = F) + 
    scale_color_manual(values = clrs) +
    scale_fill_manual(values = clrs) + 
    ylab('Valor')
  
  if (any(dd$change == "flat")) {
    plt <- plt +
      geom_segment(data = dd[dd$change == "flat",], 
                   aes(x = Data - width/2 * 0.9, y = precoFechamento, yend = precoFechamento,
                       xend = Data + width/2 * 0.9, colour = change),
                   alpha = 1)
  } 
  
  p <- suppressMessages(ggplotly(plt, tooltip = 'text') %>% layout(showlegend = F))
  p$elementId <- NULL
  p
}