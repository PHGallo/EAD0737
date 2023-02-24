# Aula 8 - Composição de Carteiras

### Retorno

# Exemplo 1
# O preço de uma ação cai de $34.5 para $29.8
p1 = 34.50
p2 = 29.80

r = (p2/p1)-1 # retorno
r*100 # retorno %

# Exemplo 2
# O preço de uma ação aumenta de $34.5 para $37.2
p1 = 34.50
p2 = 37.20

r = (p2/p1)-1 # retorno
r*100 # retorno %

# ------------------------------------------------------------------------------
### Retorno Esperado

# Exemplo 1
rets = c(0.2,0.16,0.14,0.17) # retornos
w = c(0.15,0.20,0.40,0.25) # pesos

sum(rets*w)*100 # retorno carteira %

# ------------------------------------------------------------------------------
### Teoria do Portfólio

# Exemplo 2

retA = 0.10 # retorno A
retB = 0.16 # retorno B
sigmaA = 0.05 # DP de A
sigmaB = 0.08 # DP de B
rho = 0.35 # Correlacao A e B

wa = seq(0,1,by = 0.01) # pesos para A
wb = 1 - wa # pesos para B
noCarteiras = length(wa) # no. de carteiras
retPort = 0 # inicializado retornos das carteiras
riscoPort = 0 # inicializado DPs das carteiras

# Calcular retorno e risco para cada carteira

for(i in 1:noCarteiras){
  retPort[i] = wa[i]*retA + wb[i]*retB
  riscoPort[i] = sqrt((wa[i]^2)*(sigmaA^2) + (wb[i]^2)*(sigmaB^2)+2*wa[i]*wb[i]*rho*sigmaA*sigmaB)
}
plot(riscoPort,retPort,col="red",type = "o")
lines(riscoPort,retPort,col="green",type = "o")
#lines(riscoPort,retPort,col="blue",type = "o")
#lines(riscoPort,retPort,col="black",type = "o")

carteiras = as.data.frame(cbind(wa,wb,retPort,riscoPort))

# Fronteira Eficiente

carteirasEF = as.data.frame(cbind(wa[1:64],wb[1:64],retPort[1:64],riscoPort[1:64]))
plot(riscoPort[1:64],retPort[1:64],col="red")

# ------------------------------------------------

# Exemplo 3

# Dados
library(readxl)
DadosExemplo3 <- read_excel("DadosExemplo3.xlsx")

# A - CVC e B - DrogaRaia

retA = mean(DadosExemplo3$RetCVC[2:12])
retB = mean(DadosExemplo3$RetRaia[2:12])
sigmaA = sd(DadosExemplo3$RetCVC[2:12])
sigmaB = sd(DadosExemplo3$RetRaia[2:12])
rho = cor(DadosExemplo3$RetCVC[2:12],DadosExemplo3$RetRaia[2:12])

wa = seq(0,1,0.01)
wb = 1 - wa
noCarteiras = length(wa)
retPort = 0
riscoPort = 0
for(i in 1:noCarteiras){
  retPort[i] = wa[i]*retA + wb[i]*retB
  riscoPort[i] = sqrt((wa[i]^2)*(sigmaA^2) + (wb[i]^2)*(sigmaB^2)+2*wa[i]*wb[i]*rho*sigmaA*sigmaB)
}
plot(riscoPort,retPort,col="green")
carteiras = as.data.frame(cbind(wa,wb,retPort,riscoPort))

# Fronteira Eficiente

carteirasEF = as.data.frame(cbind(wa[1:25],wb[1:25],retPort[1:25],riscoPort[1:25]))
plot(riscoPort[1:25],retPort[1:25],col="green")

# ------------------------------------------------

# Carregar os pacotes necessarios

library(readxl)
library(PortfolioAnalytics)
library(PerformanceAnalytics)
library(ROI)
library(ROI.plugin.quadprog)
library(ROI.plugin.glpk)
library(ROI.plugin.symphony)
library(doParallel)
registerDoParallel(cores=4)

# ------------------------------------------------

# Carregar os dados dos retornos das acoes:

Acoes = read_excel("Acoes.xlsx",sheet = "Retornos")

# ------------------------------------------------

# Avaliar as correlacoes dos retornos:

noAcoes = ncol(Acoes) - 1 # numero de acoes (primeira coluna datas)

matrizCorrelacao = round(cor(as.matrix(Acoes[,2:(noAcoes+1)])),3)*100

# Dividir amostra antes e depois da formacao de carteiras...

# Antes, periodo de 3/1/12 a 29/12/2016:

Retornos = Acoes[1:1236,] # divisao com base nos dados
RetornosFora = Acoes[1237:1935,]

summary(Retornos) # estatisticas descritivas retornos

# ------------------------------------------------

# Transformar dados em tipo series temporais (pacote exige):

Retornos = xts(Retornos[,2:(noAcoes+1)], as.Date(Retornos$Data, format = "%d/%m/%Y"))
RetornosFora = xts(RetornosFora[,2:(noAcoes+1)], as.Date(RetornosFora$Data, format = "%d/%m/%Y"))

# ------------------------------------------------

# Especificacoes da carteira:

fund.names = colnames(Retornos) # nome dos ativos

carteira = portfolio.spec(assets = fund.names) # criando a carteira

# Restricao 1 - carteira totalmente investida:

carteira = add.constraint(portfolio = carteira, type = "full_investment")

# Restricao 2 - apenas posicoes compradas:

carteira = add.constraint(portfolio = carteira, type = "long_only")

# Restricao 3 - para os pesos:

# carteira = add.constraint(portfolio = carteira, type = "box", min = 0, max = 0.15)

# ------------------------------------------------

# Fronteira Eficiente:

FE = meanvar.efficient.frontier(portfolio = carteira, Retornos, n.portfolios = 40)
plot(100*FE[,2],100*FE[,1],xlab="Risco (%)",ylab = "Retorno (%)",col="blue",main = "Fronteira Eficiente")

# ------------------------------------------------

# Processo de otimizacao... 

# Definindo o objetivo do investidor:

# 1. Carteira de variancia minima (CVM) - eficiente e com menor risco...

carteira = add.objective(portfolio = carteira, type = "risk", name = "StdDev")

# 2. Carteira de retorno pre definido - eficiente e com menor risco para o retorno desejado...

# carteira <- add.constraint(portfolio = carteira, type = "return",return_target = 0.0008)

# Otimizando a carteira...

MinhaCarteira = optimize.portfolio(R = Retornos,portfolio = carteira,optimize_method = "ROI",trace = TRUE)

MinhaCarteira # informacoes da carteira

# Calcular o retorno medio (%) da carteira:

mean(Return.portfolio(Retornos,weights = extractWeights(MinhaCarteira)))*100

# Verifique a carteira na fronteira eficiente!!!

100*round(MinhaCarteira[["weights"]],4) # pesos em cada ativo (%)

# Alocacoes:

plot(MinhaCarteira)

# Vamos verificar o desempenho dela fora da amostra 2017 a 2019...

# Calcular os retornos nesse periodo:

RetornoMC = Return.portfolio(RetornosFora,weights = extractWeights(MinhaCarteira))

# Retorno medio (%) fora da amostra:

mean(RetornoMC)*100

# Desvio-padrao (%) fora da amostra:

sd(RetornoMC)*100

# Vizualizacao:

plot(RetornoMC)

# Visualizar retornos acumulados (soma geometrica dos retornos dia a dia): 

chart.CumReturns(RetornoMC)

