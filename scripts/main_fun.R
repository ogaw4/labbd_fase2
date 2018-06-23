
download_data <- function(dest_dir, ref_date) {
    # Premios/Valores de ref. de opçoes
  b3_download_handler(c("PE", "DO", "RE"), c("PE.txt", "DO.txt", "RE.txt"), ref_date, dest_dir)
  
  # Dados diários de negociação, dados de índices
  bvbg_download_handler("IR", "BVBG.087.xml", ref_date, dest_dir)
  bvbg_download_handler("PR", "BVBG.086.xml", ref_date, dest_dir)
  
  # Dados de cadastro
  bvbg_download_handler("IN", "BVBG.028.xml", ref_date, dest_dir)
  
}


insert_data <- function(d_dir, ref_date) {
  
  # Cadastro de contratos
  bvbg028_insert_handler("BVBG.028.xml", ref_date, d_dir)
  # Historico de acoes/contratos
  bvbg086_insert_handler(c("BVBG.086.xml", "DO.txt", "RE.txt"), ref_date, d_dir)
  # Historico de indices
  bvbg087_insert_handler("BVBG.087.xml", ref_date, d_dir)
  # Cadastro/Historico de opcoes sobre acoes
  bvbgopc_insert_handler(c("BVBG.028.xml", "BVBG.086.xml", "PE.txt"), ref_date, d_dir)
  
}


format_cnpj <- function(cnpj) {
  cnpj <- as.character(cnpj) 
  paste0(substr(cnpj, 1, 2), ".", 
         substr(cnpj, 3, 5), ".", 
         substr(cnpj, 6, 9), "/", 
         substr(cnpj, 10, 13), "-", 
         substr(cnpj, 14, 15))
}
