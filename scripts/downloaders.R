
b3_download_handler <- function(urlfname, fname, data_ref, dest_dir) {
  mapply(function(url_fname, fname) {
    b3_file_downloader(url_fname, fname, dest_dir, data_ref)
  }, urlfname, fname)
}

b3_file_downloader <- function(url_fname, fname, dest_dir, ref_date) {
  data <- as.Date(ref_date)
  if (fname == "DO.txt") { # arquivo DO é gerado como d+1 para d0
    data <- data + days(1)
    data <- adjust.next(data)
    if (substr(as.character(data), 6, nchar(as.character(data))) %in% c("01-25", "07-09", "11-20")) { # ajustar feriados municipais
      data <- data + days(1)
      data <- adjust.next(data)
    }
  }
  data <- format(x=data, format="%y%m%d")
  url <- 'http://www.bmfbovespa.com.br/pesquisapregao/download?filelist='
  url <- paste(url, url_fname, data, '.ex_', sep="")
  dest_file <- paste(dest_dir, '/', fname, '.zip', sep="")
  final_file <- paste(dest_dir, '/', fname, sep="")
  if (file.exists(final_file)) return(TRUE)
  download.file(url, dest_file, "auto", mode="wb")
  if(file.exists(dest_file)) {
    sec_zip <- unzip(zipfile=dest_file, exdir=dest_dir)
    txt_files <- unzip(zipfile=sec_zip, exdir=dest_dir)
    file.remove(dest_file)
    file.remove(sec_zip)
    file.copy(txt_files[length(txt_files)], final_file)
    file.remove(txt_files)
    final_file
  } else {
    stop(paste("Erro no download do arquivo ", obj$filename))
    return(NULL)
  }
}


bvbg_download_handler <- function(urlfname, fname, data_ref, dest_dir) {
  data <- as.Date(data_ref)
  data <- format(x=data, format="%y%m%d")
  url <- 'http://www.bmfbovespa.com.br/pesquisapregao/download?filelist='
  url <- paste(url, urlfname, data, '.zip', sep="")
  dest_file <- paste(dest_dir, '/', fname, '.zip', sep="")
  final_file <- paste(dest_dir, '/', fname, sep="")
  if (file.exists(final_file)) return(TRUE)
  download.file(url, dest_file, "auto")
  if(file.exists(dest_file)) {
    sec_zip <- unzip(zipfile=dest_file, exdir=dest_dir)
    xml_files <- unzip(zipfile=sec_zip, exdir=dest_dir)
    file.remove(dest_file)
    file.remove(sec_zip)
    file.copy(xml_files[length(xml_files)], final_file)
    file.remove(xml_files)
    final_file
  } else {
    stop(paste("Erro no download do arquivo ", fname))
    return(NULL)
  }
}
