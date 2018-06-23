require 'net/http'
require 'json'


source = Net::HTTP.get(URI("http://bvmf.bmfbovespa.com.br/pt-br/mercados/acoes/empresas/ExecutaAcaoConsultaInfoEmp.asp?CodCVM=9512"))
puts(" hello ")
puts(source)
puts(source.scan(/:VerificaCotacao/))