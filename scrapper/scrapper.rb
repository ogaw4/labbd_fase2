require 'net/http'
require 'json'

class Scrapper
	def initialize(login="", senha="")
		@login = login
		@senha = senha
		@webservice = "http://webfeeder.cedrofinances.com.br"
	end

	def connect_to_bovespa()
		post("SignIn?login=#{@login}&password=#{senha}")
	end

	def get(command)
		Net::HTTP.get(URI("#{@webservice}/#{command}"))
	end

	def post(command)
		uri = URI("#{@webservice}/#{command}")
		request = Net::HTTP::Post.new(uri)
		Net::HTTP.start(uri.hostname, uri.port)
	end

	def get_quotes(ticker)
		#has to handle invalid data, need to test that
		eval(get("services/quotes/quote/#{ticker}"))
	end

	def get_company_data()
		proxy = ScrapeCompanys.new()
		cvms = proxy.get_cvms()
		output = []

		cvms.each do |cvm|
			output << proxy.get_company_data(cvm)
		end

		output.to_json
	end
end

class ScrapeCompanys
	def initialize()
		@base_query_address = "http://bvmf.bmfbovespa.com.br/pt-br/mercados/acoes/empresas/ExecutaAcaoConsultaInfoEmp.asp?CodCVM="
		@code_source = "http://cvmweb.cvm.gov.br/SWB/Sistemas/SCW/CPublica/CiaAb/FormBuscaCiaAbOrdAlf.aspx?LetraInicial="
	end

	def extract_simple_text(source)

		if (source[0].nil?)
			return ""
		end
		simplified = source[0].reverse

		begin
			simplified = simplified[(simplified.index(">dt/<") + 5)..(simplified.index(">dt<") - 1)]
		rescue => e
			simplified = simplified[(simplified.index(">dt/<") + 5)..(simplified.index("\>\""))]
		end

		simplified.reverse
	end

	def extract_site(source)
		if (source[0].nil?)
			return ""
		end
		simplified = source[0].reverse
		simplified = simplified[(simplified.index(">a/<") + 4)..(simplified.index(">\"") - 1)]
		simplified.reverse
	end

	def extract_data_composicao_capital_social(source)		
		if (source[0].nil?)
			return ""
		end
		simplified = source[0]
		simplified[3..simplified.size]
	end

	def extract_many_th(source)
		if (source[0].nil?)
			return []
		end
		divided = source[0].split("\r\n")
		simplified = divided[1..divided.size]
		output = []

		simplified.each do |s|
			s.reverse!

			if s.index(">ht/<") != nil then
				begin
					output << s[(s.index(">ht/<") + 4)..(s.index(">\"") - 1)].reverse
				rescue => e
					output << s[(s.index(">ht/<") + 4)..(s.index(">h") - 1)]
				end
			end
		end

		output	
	end

	def extract_many_td(source)
		if (source[0].nil?)
			return []
		end
		divided = source[0].split("\r\n")
		simplified = divided[1..divided.size]
		output = []

		simplified.each do |s|
			s.reverse!
			output << s[(s.index(">dt/<") + 4)..(s.index(">\"") - 1)].reverse
		end

		output	
	end

	def get_company_data(cvm)
		output = {}
		source = Net::HTTP.get(URI("#{@base_query_address}#{cvm}"))
		if source.nil?
			nil
		else
			if (source.scan(/Nome de Preg.+\r\n.+\<\/td\>/)[0]).nil?
				return nil
			end
			output[:nome] = extract_simple_text(source.scan(/Nome de Preg.+\r\n.+\<\/td\>/))
			output[:cnpj] = extract_simple_text(source.scan(/CNPJ.+\r\n.+\<\/td\>/))
			output[:site] = extract_site(source.scan(/Site:\<\/td\>\r\n.+\<\/a\>/))

			output[:balanco_patrimonial] = {}
			output[:balanco_patrimonial][:periodo] = extract_many_th(source.scan(/Balan.+\r\n.+\r\n.+\r\n/))
			output[:balanco_patrimonial][:ativo_permanente] = extract_many_td(source.scan(/Ativo Permanente.+\r\n.+\r\n.+\r\n/))
			output[:balanco_patrimonial][:ativo_total] = extract_many_td(source.scan(/Ativo Total.+\r\n.+\r\n.+\r\n/))
			output[:balanco_patrimonial][:patromonio_liquido] = extract_many_td(source.scan(/Patrim.+nio L.+quido.+\r\n.+\r\n.+\r\n/))
			
			output[:demonstracao_do_resultado] = {}
			output[:demonstracao_do_resultado][:periodo] = extract_many_th(source.scan(/Demonstra.+o do Resultado.+\r\n.+\r\n.+\r\n.+\r\n/))
			output[:demonstracao_do_resultado][:receitas_da_intermediacao_financeira] = extract_many_td(source.scan(/Receitas da Intermedia.+ Financeira.+\r\n.+\r\n.+\r\n/))
			output[:demonstracao_do_resultado][:resultado_bruto_de_intermediacao_financeira] = extract_many_td(source.scan(/Resultado Bruto da Intermedia.+ Financeira.+\r\n.+\r\n.+\r\n/))
			output[:demonstracao_do_resultado][:resultado_operacional] = extract_many_td(source.scan(/Resultado Operacional.+\r\n.+\r\n.+\r\n/))
			output[:demonstracao_do_resultado][:lucro_liquido] = extract_many_td(source.scan(/Lucro .+ L.+quido.+\r\n.+\r\n.+\r\n/))

			output[:demonstracao_do_fluxo_de_caixa] = {
			periodo: extract_many_th(source.scan(/Demonstra.+ do Fluxo de Caixa.+\r\n.+\r\n.+\r\n/)),
			atividades_operacionais: extract_many_td(source.scan(/Atividades Operacionais.+\r\n.+\r\n.+\r\n/)),
			atividades_de_investimento: extract_many_td(source.scan(/Atividades de Investimento.+\r\n.+\r\n.+\r\n/)),
			atividades_de_financiamento: extract_many_td(source.scan(/Atividades de Financiamento.+\r\n.+\r\n.+\r\n/)),
			variacao_cambial_sobre_caixa_e_equivalentes: extract_many_td(source.scan(/Varia.+o Cambial sobre Caixa e Equivalentes.+\r\n.+\r\n.+\r\n/)),
			aumento_de_caixa_e_equivalentes: extract_many_td(source.scan(/Aumento .+ de Caixa e Equivalentes.+\r\n.+\r\n.+\r\n/))}
		
			output[:posicao_acionaria] = {
				headings: extract_many_th(source.scan(/\<th\>Nome.+\r\n.+\r\n.+\r\n.+\r\n/)),
				outros: extract_many_td(source.scan(/\<td\>Outros.+\n.+\n.+\n.+\n/)),
				total: extract_many_td(source.scan(/\<td\>Total.+\n.+\<td.+\n.+\<td.+\n.+\<td.+\n/))}

			output[:composicao_capital_social] = {
				data: extract_data_composicao_capital_social(source.scan(/ - [0-9]+\/[0-9]+\/[0-9]+/)),
				ordinarias: extract_simple_text(source.scan(/Ordin.+rias.+\n.+\n/)),
				preferenciais: extract_simple_text(source.scan(/Preferenciais.+\n.+\n/)),
				total: extract_simple_text(source.scan(/Total.+\n.+\n.+\<\/tr/))}

			output
		end
	end

	def get_cvms()
		cvms = []
		guidelines = ['A', 'B', 'C', 'D', 'E', 'F', 'G', 
									'H', 'I', 'J', 'K', 'L', 'M', 'N', 
									'O', 'P', 'Q', 'R', 'S', 'T', 'U', 
									'V', 'W', 'Y', 'X', 'Z', '0', '1', 
									'2', '3', '4', '5', '6', '7', '8', '9']

		guidelines.each do |k|
			source = Net::HTTP.get(URI("#{@code_source}#{k}")).scan(/\>[0-9]+\<\/a\>/)

			source.each do |str|
				sub = str[1..(str.length - 5)]
				if sub.length > 1 then
					cvms << sub.to_i
				end
			end
	
			source = nil
		end

		puts cvms.size
		cvms
	end
end




scrap = ScrapeCompanys.new()

cvms = scrap.get_cvms()

cvms.each do |cvm|
	data = scrap.get_company_data(cvm)	
	if not data.nil?
		nm = data[:nome].gsub("/", "")
		File.open("./raw_data/company/" + nm + "_data.json", "w") do |f|
	  		f.write(data.to_json)
		end
	end

end

f = File.open("./done.txt", "w")
f.write("done")
f.close()
