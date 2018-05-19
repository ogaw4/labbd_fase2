# Dados da B3 - Lab BD 2018 Fase 2

## Instruções de uso

Para execução e uso do aplicativo são necessários:

- R versão 3.x
- Servidor PostgreSQL
- Servidor MongoDB

Antes de utilizar o aplicativo o schema deve ser criado com o script em criacao_sql.sql.

As configurações de conexão aos bancos de dados estão no arquivo global.R. Para execução do aplicativo localmente recomenda-se 
o uso do RStudio, mas é possível executá-lo por linha de comando com:

> R -e "shiny::runApp('~/shinyapp')"

estando na pasta do projeto. Para isso será necessário instalar o pacote 'shiny'. Os demais pacotes necessários são instalados automaticamente 
na execução do aplicativo. 

Tendo aberto o aplicativo é possível atualizar os dados no banco na aba de Inserção de Dados, e visualizar gráficos com dados de negociação 
na aba Gráficos.

## Versão online

Como a inserção de dados é demorada, também está disponível uma versão do aplicativo em https://ogawa.shinyapps.io/fase2_shinyapps/, com 
dados pré-carregados. Como não é simples a conexão com bancos de dados na plataforma utilizada, essa versão serve meramente para ver 
a interface com alguns dados carregados. 