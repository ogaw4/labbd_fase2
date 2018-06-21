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

Tendo aberto o aplicativo é possível atualizar os dados no banco na aba de Inserção de Dados, para um dado período podem ser inseridos dados de negociação e cadastro de ações, opções, futuros e índices, e pode-se atualizar os dados das empresas. Todas as informações são obtidas das seções de consulta pública do site da B3 (antiga BM&FBovespa), sendo os dados de empresa sempre os mais atualizados existentes. 

Com dados inseridos, pode ser feita uma busca na aba principal que faz um match parcial do texto inserido com tickers de ações, opções, futuros, índices e dados de empresas como nomes, CNPJ e áreas de atuação. Escolhendo um dos resultados da busca o usuário é redirecionado para a página com gráficos de dados históricos de negociação para um período ajustável, e tabelas com informações cadastrais relevantes. 

## Versão online

Como a inserção de dados é demorada para longos períodos devido ao tamanho dos arquivos XML publicados pela Bolsa, também está disponível uma versão do aplicativo em https://ogawa.shinyapps.io/fase2_shinyapps/, com 
dados pré-carregados. Como não é simples a conexão com bancos de dados na plataforma utilizada, essa versão serve meramente para ver 
a interface com alguns dados carregados. 

## Memory leak

O pacote XML tem um memory leak conhecido se instalado pelo CRAN (instalação automática usual) em sistemas Windows. Recomenda-se o uso de 
outros sistemas operacionais ou instalação da biblioteca XML a partir do código fonte. 
