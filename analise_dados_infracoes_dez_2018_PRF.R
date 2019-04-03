#Fonte dados PRF:
# https://www.prf.gov.br/portal/dados-abertos
#Categoria: Infracoes
#Ano: 2018
#Mes: Dezembro
#Link direto: http://www.prf.gov.br/arquivos/index.php/s/RRibshM06CTBnGw/download

#identifica o diretorio de trabalho (workspace)
getwd()

setwd("/home/leo/Downloads")

#Download do arquivo original
download.file("http://www.prf.gov.br/arquivos/index.php/s/RRibshM06CTBnGw/download",destfile="arquivo.zip",method="libcurl")

#Extrair arquivo
unzip("arquivo.zip")

#Criar o dataframe que guardar? os dados do arquivo
infracoes_dez <- read.csv("2018/dez.csv", header=TRUE, sep=",", encoding="latin1")

### Chama as fun??es que examinam o dataframe
#dim(infracoes)
#str(infracoes)
#summary(infracoes)
#colnames(infracoes)

### Mostra o cabecalho do dataframe com um numero limitado de linhas
#head(infracoes, n = 10)

analise_tabela_qtd_por_uf <- function(){
  View(sort(table(infracoes_dez$uf_infracao), decreasing = TRUE))
}

analise_qtd_regiao_sul_e_outras <- function(){
  titulos <- c("Outras regiões", "Região Sul")
  pie(table(infracoes_dez$uf_infracao %in% c('PR', 'SC', 'RS')), labels = titulos)
}

analise_qtd_regiao_sul <- function(tipoGrafico){
  ocorrencias_sc <-table(infracoes_dez$uf_infracao == 'SC')["TRUE"]
  ocorrencias_pr <-table(infracoes_dez$uf_infracao == 'PR')["TRUE"]
  ocorrencias_rs <-table(infracoes_dez$uf_infracao == 'RS')["TRUE"]
  titulos <- c('PR', 'SC', 'RS')
  ocorrencias_regiao_sul <- c(ocorrencias_pr, ocorrencias_sc, ocorrencias_rs)
  if(tipoGrafico == "pizza"){
    pie(ocorrencias_regiao_sul, labels = titulos, main = "Gráfico Pizza - Nº Ocorrências")
  }
  if(tipoGrafico == "barra"){
    barplot(ocorrencias_regiao_sul, names.arg = titulos, main = "Gráfico Barra - Nº Ocorrências")
  }
}

analise_qtd_rodovias_regiao <- function(tipoGrafico){
  ocorrencias_101 <-table(infracoes_dez$num_br_infracao == "101" & infracoes_dez$uf_infracao == "SC")["TRUE"]
  ocorrencias_282 <-table(infracoes_dez$num_br_infracao == "282" & infracoes_dez$uf_infracao == "SC")["TRUE"]
  ocorrencias_470 <-table(infracoes_dez$num_br_infracao == "470" & infracoes_dez$uf_infracao == "SC")["TRUE"]
  titulos <- c('101', '282', '470')
  ocorrencias_rodovias <- c(ocorrencias_101, ocorrencias_282, ocorrencias_470)
  if(tipoGrafico == "pizza"){
    pie(ocorrencias_rodovias, labels = titulos, main = "Gráfico Pizza - Nº Ocorrências")
  }
  if(tipoGrafico == "barra"){
    barplot(ocorrencias_rodovias, names.arg = titulos, main = "Gráfico Barra - Nº Ocorrências Sul")
  }
}

analise_top_five_infracoes_dez <- function(numero_top){
  pie(summary(infracoes_dez$descricao_abreviada, maxsum = numero_top))
}

#Chama a funcao que compara via tabela as ocorrencias entre os estados
analise_tabela_qtd_por_uf()

#Chama a funcao que compara a regiao Sul com as demais
analise_qtd_regiao_sul_e_outras()

#Chama a funcao que compara as ocorrencia entre os tres estados da regiao sul
analise_qtd_regiao_sul("pizza")

#Chama a funcao que compara as ocorrencias entre as principais rodovias do estado
analise_qtd_rodovias_regiao("pizza")

#Chama a funcao analise TOP 5 infracoes
analise_top_five_infracoes_dez(5)

#Escrevendo arquivo de saida da analise de infracoes por UF
write.table(sort(table(infracoes_dez$uf_infracao), decreasing = TRUE), "analise.txt")


