
if (!require(pacman)) install.packages("pacman")

pacman::p_load(bizdays, utils, httr, lubridate, XML, dplyr, shiny, shinydashboard, ggplot2, 
               plotly, RPostgreSQL, yaml, mongolite, reshape2, glue)

dir <- "scripts/"

scripts <- list.files(dir)

load_scripts <- lapply(scripts,
                       function(x) source(paste0(dir,x), encoding = 'UTF-8'))

bizdays.options$set(default.calendar = "Brazil/ANBIMA")

dest_dir <- "raw_data/"
if (!dir.exists(dest_dir)) dir.create(dest_dir)

DB_USERNAME <- 'postgres'
DB_PWORD <- '#####'
DB_HOST <- 'localhost'
DB_NAME <- '#####'
DB_PORT <- 5432

SCHEMA_NAME <- 'mac439_bmfbovespa'

drv <- dbDriver('PostgreSQL')
DB_CONNECTION <- dbConnect(drv, user = DB_USERNAME, password = DB_PWORD,
                  host = DB_HOST, port = DB_PORT, dbname = DB_NAME)
sp <- dbGetQuery(DB_CONNECTION, paste0('set search_path to ', SCHEMA_NAME, ';'))

MONGO_DB_NAME <- 'mac439_bmfbovespa'
MONGO_DB_URL <- 'mongodb://localhost:27017'

MONGO_STOCKS <- mongo(collection = 'Negociacao_Acao', db = MONGO_DB_NAME,
                      url = MONGO_DB_URL )
MONGO_FUTURES <- mongo(collection = 'Negociacao_Futuro', db = MONGO_DB_NAME,
                       url = MONGO_DB_URL )
MONGO_OPTIONS <- mongo(collection = 'Negociacao_Opcao', db = MONGO_DB_NAME,
                       url = MONGO_DB_URL )
MONGO_INDEXES <- mongo(collection = 'Preco_Indice', db = MONGO_DB_NAME,
                       url = MONGO_DB_URL )