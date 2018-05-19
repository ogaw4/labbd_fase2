create schema mac439_bmfbovespa;
set search_path to mac439_bmfbovespa;

CREATE TABLE empresa (
    cnpj numeric PRIMARY KEY,
    nome text NOT NULL,
    setores text,
    atividade_principal text,
    site text
);

CREATE TABLE instrumento
(
	id serial PRIMARY KEY,
	codigo text
);

CREATE TABLE acao
(
	id_instrumento int PRIMARY KEY REFERENCES instrumento(id) ON DELETE CASCADE,
	cnpj_empresa int REFERENCES empresa(cnpj) ON DELETE SET NULL,
	tipo char(2) NOT NULL CHECK (tipo = 'PN' OR tipo = 'ON')
);

CREATE TABLE opcao 
(
	id_instrumento int PRIMARY KEY REFERENCES instrumento(id) ON DELETE CASCADE,
	id_underlying int NOT NULL REFERENCES instrumento(id) ON DELETE CASCADE, 
	vencimento date NOT NULL, 
	strike numeric NOT NULL,
	tipo varchar(4) NOT NULL CHECK (tipo = 'call' OR tipo = 'put'),
	estilo varchar(9) NOT NULL CHECK (estilo = 'americana' OR estilo = 'europeia')
);

-- A nomenclatura usada na bolsa para 
-- o ativo ao qual o futuro se refere 
-- é objeto de negociação, por isso objeto. 
CREATE TABLE futuro (
    id_instrumento int PRIMARY KEY REFERENCES instrumento(id) ON DELETE CASCADE,
    id_objeto int REFERENCES instrumento(id) ON DELETE CASCADE,
    vencimento date NOT NULL
);

CREATE TABLE indice (
    id_instrumento int PRIMARY KEY REFERENCES instrumento(id) ON DELETE CASCADE,
    descricao text
);

CREATE TABLE mercadoria (
    id_instrumento int PRIMARY KEY REFERENCES instrumento(id) ON DELETE CASCADE,
    descricao text,
    tipo text
);

-- Evitar perder dados de relatórios 
-- acidentalmente ao remover uma empresa.
CREATE TABLE relatorio (
    id serial PRIMARY KEY,
    cnpj_empresa int REFERENCES empresa(cnpj) ON DELETE NO ACTION ON UPDATE CASCADE
);

CREATE TABLE relatorio_consolidado (
    id_relatorio int REFERENCES relatorio(id) ON DELETE CASCADE,
    periodo_ref text
);

CREATE TABLE relatorio_financeiro (
    id_relatorio int REFERENCES relatorio(id) ON DELETE CASCADE,
    data date
);

CREATE TABLE balanco_patrimonial (
    id_relatorio int REFERENCES relatorio(id) ON DELETE CASCADE,
	ativo_total numeric,
	ativo_imobilizado numeric,
	patrimonio_atribuido numeric,
	patrimonio_liquido numeric	
);

CREATE TABLE demonstracao_de_resultado (
    id_relatorio int REFERENCES relatorio(id) ON DELETE CASCADE,
	receita numeric,
	resultado_bruto numeric,
	resultado_financeiro numeric,
	resultado_liquido numeric,
	resultado_equivalente numeric,
	lucro_periodo numeric,
	lucro_atribuido numeric
);

CREATE TABLE demonstracao_do_fluxo_de_caixa (
    id_relatorio int REFERENCES relatorio(id) ON DELETE CASCADE,
	ativo_operacional numeric,
	ativo_investimento numeric,
	ativo_financeiro numeric,
	aumento_caixa numeric,
	variacao_caixa numeric
);

CREATE TABLE acoes_emitidas (
    id_relatorio int REFERENCES relatorio(id) ON DELETE CASCADE,
	total_geral numeric,
	ordinarias numeric,
	pessoas_fisicas numeric,
	pessoas_juridicas numeric
);


CREATE TABLE capital_social (
    id_relatorio int REFERENCES relatorio(id) ON DELETE CASCADE,
	ordinario numeric,
	preferencial numeric
);


-- Mercadorias e índices mudam tão raramente 
-- que não há sentido em fazer uma inserção 
-- automatizada. 

INSERT INTO instrumento(codigo) VALUES ('ACF');
INSERT INTO mercadoria(id_instrumento, descricao, tipo) SELECT id as id_instrumento, 'Açúcar Cristal' as descricao, 'Commodities' as tipo FROM instrumento WHERE codigo = 'ACF';

INSERT INTO instrumento(codigo) VALUES ('BGI');
INSERT INTO mercadoria(id_instrumento, descricao, tipo) SELECT id as id_instrumento, 'Boi Gordo' as descricao, 'Commodities' as tipo FROM instrumento WHERE codigo = 'BGI';

INSERT INTO instrumento(codigo) VALUES ('ICF');
INSERT INTO mercadoria(id_instrumento, descricao, tipo) SELECT id as id_instrumento, 'Café Arábica 4/5' as descricao, 'Commodities' as tipo FROM instrumento WHERE codigo = 'ICF';

INSERT INTO instrumento(codigo) VALUES ('KFE');
INSERT INTO mercadoria(id_instrumento, descricao, tipo) SELECT id as id_instrumento, 'Café Arábica 6/7' as descricao, 'Commodities' as tipo FROM instrumento WHERE codigo = 'KFE';

INSERT INTO instrumento(codigo) VALUES ('ETN');
INSERT INTO mercadoria(id_instrumento, descricao, tipo) SELECT id as id_instrumento, 'Etanol Anidro' as descricao, 'Commodities' as tipo FROM instrumento WHERE codigo = 'ETN';

INSERT INTO instrumento(codigo) VALUES ('ETH');
INSERT INTO mercadoria(id_instrumento, descricao, tipo) SELECT id as id_instrumento, 'Etanol Hidratado' as descricao, 'Commodities' as tipo FROM instrumento WHERE codigo = 'ETH';

INSERT INTO instrumento(codigo) VALUES ('CCM');
INSERT INTO mercadoria(id_instrumento, descricao, tipo) SELECT id as id_instrumento, 'Milho' as descricao, 'Commodities' as tipo FROM instrumento WHERE codigo = 'CCM';

INSERT INTO instrumento(codigo) VALUES ('OZ1');
INSERT INTO mercadoria(id_instrumento, descricao, tipo) SELECT id as id_instrumento, 'Ouro' as descricao, 'Commodities' as tipo FROM instrumento WHERE codigo = 'OZ1';

INSERT INTO instrumento(codigo) VALUES ('WTI');
INSERT INTO mercadoria(id_instrumento, descricao, tipo) SELECT id as id_instrumento, 'Petróleo CME' as descricao, 'Commodities' as tipo FROM instrumento WHERE codigo = 'WTI';

INSERT INTO instrumento(codigo) VALUES ('SFI');
INSERT INTO mercadoria(id_instrumento, descricao, tipo) SELECT id as id_instrumento, 'Soja' as descricao, 'Commodities' as tipo FROM instrumento WHERE codigo = 'SFI';

INSERT INTO instrumento(codigo) VALUES ('DDI');
INSERT INTO mercadoria(id_instrumento, descricao, tipo) SELECT id as id_instrumento, 'Cupom DI' as descricao, 'Juros e inflação' as tipo FROM instrumento WHERE codigo = 'DDI';

INSERT INTO instrumento(codigo) VALUES ('DCO');
INSERT INTO mercadoria(id_instrumento, descricao, tipo) SELECT id as id_instrumento, 'Cupom Selic' as descricao, 'Juros e inflação' as tipo FROM instrumento WHERE codigo = 'DCO';

INSERT INTO instrumento(codigo) VALUES ('DAP');
INSERT INTO mercadoria(id_instrumento, descricao, tipo) SELECT id as id_instrumento, 'Cupom IPCA' as descricao, 'Juros e inflação' as tipo FROM instrumento WHERE codigo = 'DAP';

INSERT INTO instrumento(codigo) VALUES ('DI1');
INSERT INTO mercadoria(id_instrumento, descricao, tipo) SELECT id as id_instrumento, 'Taxa DI' as descricao, 'Juros e inflação' as tipo FROM instrumento WHERE codigo = 'DI1';

INSERT INTO instrumento(codigo) VALUES ('OC1');
INSERT INTO mercadoria(id_instrumento, descricao, tipo) SELECT id as id_instrumento, 'Taxa Selic' as descricao, 'Juros e inflação' as tipo FROM instrumento WHERE codigo = 'OC1';

INSERT INTO instrumento(codigo) VALUES ('IAP');
INSERT INTO mercadoria(id_instrumento, descricao, tipo) SELECT id as id_instrumento, 'IPCA' as descricao, 'Juros e inflação' as tipo FROM instrumento WHERE codigo = 'IAP';

INSERT INTO instrumento(codigo) VALUES ('AUD');
INSERT INTO mercadoria(id_instrumento, descricao, tipo) SELECT id as id_instrumento, 'Dolar Australiano' as descricao, 'Moedas' as tipo FROM instrumento WHERE codigo = 'AUD';

INSERT INTO instrumento(codigo) VALUES ('CAD');
INSERT INTO mercadoria(id_instrumento, descricao, tipo) SELECT id as id_instrumento, 'Dolar Canadense' as descricao, 'Moedas' as tipo FROM instrumento WHERE codigo = 'CAD';

INSERT INTO instrumento(codigo) VALUES ('NZD');
INSERT INTO mercadoria(id_instrumento, descricao, tipo) SELECT id as id_instrumento, 'Dolar Nova Zelandia' as descricao, 'Moedas' as tipo FROM instrumento WHERE codigo = 'NZD';

INSERT INTO instrumento(codigo) VALUES ('DOL');
INSERT INTO mercadoria(id_instrumento, descricao, tipo) SELECT id as id_instrumento, 'Dolar Estados Unidos' as descricao, 'Moedas' as tipo FROM instrumento WHERE codigo = 'DOL';

INSERT INTO instrumento(codigo) VALUES ('EUR');
INSERT INTO mercadoria(id_instrumento, descricao, tipo) SELECT id as id_instrumento, 'Euro' as descricao, 'Moedas' as tipo FROM instrumento WHERE codigo = 'EUR';

INSERT INTO instrumento(codigo) VALUES ('CHF');
INSERT INTO mercadoria(id_instrumento, descricao, tipo) SELECT id as id_instrumento, 'Franco Suíço' as descricao, 'Moedas' as tipo FROM instrumento WHERE codigo = 'CHF';

INSERT INTO instrumento(codigo) VALUES ('JPY');
INSERT INTO mercadoria(id_instrumento, descricao, tipo) SELECT id as id_instrumento, 'Yen Japonês' as descricao, 'Moedas' as tipo FROM instrumento WHERE codigo = 'JPY';

INSERT INTO instrumento(codigo) VALUES ('CNY');
INSERT INTO mercadoria(id_instrumento, descricao, tipo) SELECT id as id_instrumento, 'Yuan Chinês' as descricao, 'Moedas' as tipo FROM instrumento WHERE codigo = 'CNY';

INSERT INTO instrumento(codigo) VALUES ('GBP');
INSERT INTO mercadoria(id_instrumento, descricao, tipo) SELECT id as id_instrumento, 'Libra Esterlina' as descricao, 'Moedas' as tipo FROM instrumento WHERE codigo = 'GBP';

INSERT INTO instrumento(codigo) VALUES ('TRY');
INSERT INTO mercadoria(id_instrumento, descricao, tipo) SELECT id as id_instrumento, 'Lira Turca' as descricao, 'Moedas' as tipo FROM instrumento WHERE codigo = 'TRY';

INSERT INTO instrumento(codigo) VALUES ('CLP');
INSERT INTO mercadoria(id_instrumento, descricao, tipo) SELECT id as id_instrumento, 'Peso Chileno' as descricao, 'Moedas' as tipo FROM instrumento WHERE codigo = 'CLP';

INSERT INTO instrumento(codigo) VALUES ('MXN');
INSERT INTO mercadoria(id_instrumento, descricao, tipo) SELECT id as id_instrumento, 'Peso Mexicano' as descricao, 'Moedas' as tipo FROM instrumento WHERE codigo = 'MXN';

INSERT INTO instrumento(codigo) VALUES ('ZAR');
INSERT INTO mercadoria(id_instrumento, descricao, tipo) SELECT id as id_instrumento, 'Rande África do Sul' as descricao, 'Moedas' as tipo FROM instrumento WHERE codigo = 'ZAR';

INSERT INTO instrumento(codigo) VALUES ('IND');
INSERT INTO indice(id_instrumento, descricao) SELECT id as id_instrumento, 'Índice Bovespa - deriv.' as descricao FROM instrumento WHERE codigo = 'IND';

INSERT INTO instrumento(codigo) VALUES ('IBOV');
INSERT INTO indice(id_instrumento, descricao) SELECT id as id_instrumento, 'Índice Bovespa - spot' as descricao FROM instrumento WHERE codigo = 'IBOV';

INSERT INTO instrumento(codigo) VALUES ('BRI');
INSERT INTO indice(id_instrumento, descricao) SELECT id as id_instrumento, 'Índice Brasil 50 - deriv.' as descricao FROM instrumento WHERE codigo = 'BRI';

INSERT INTO instrumento(codigo) VALUES ('IBXL');
INSERT INTO indice(id_instrumento, descricao) SELECT id as id_instrumento, 'Índice Brasil 50 - spot' as descricao FROM instrumento WHERE codigo = 'IBXL';

INSERT INTO instrumento(codigo) VALUES ('ISP');
INSERT INTO indice(id_instrumento, descricao) SELECT id as id_instrumento, 'Índice S&P - deriv.' as descricao FROM instrumento WHERE codigo = 'ISP';





