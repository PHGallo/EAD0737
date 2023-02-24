# Atividade

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
library(tidyverse)
library(dygraphs)

# Companhia Energética de Minas Gerais (CMIG4.SA)
cmig4 <- quantmod::getSymbols("CMIG4.SA", 
                             src = "yahoo", 
                             auto.assign = FALSE, 
                             from = "2017-01-01",
                             to = "2022-11-05",
                             return.class = 'xts')

knitr::kable(head(cmig4), align = "c")

# Banco Santander (Brasil) S.A. (SANB11.SA)
santander <- quantmod::getSymbols("SANB11.SA", 
                             src = "yahoo", 
                             auto.assign = FALSE, 
                             from = "2017-01-01",
                             to = "2022-11-05",
                             return.class = 'xts')

knitr::kable(head(santander), align = "c")

# Lojas Renner S.A. (LREN3.SA)
renner <- quantmod::getSymbols("LREN3.SA", 
                             src = "yahoo", 
                             auto.assign = FALSE, 
                             from = "2017-01-01",
                             to = "2022-11-05",
                             return.class = 'xts')

knitr::kable(head(renner), align = "c")

# Telefônica Brasil S.A. (VIVT3.SA)
vivo = quantmod::getSymbols("VIVT3.SA", 
                            src = "yahoo", 
                            auto.assign = FALSE, 
                            from = "2017-01-01",
                            to = "2022-11-05",
                            return.class = 'xts')

knitr::kable(head(vivo), align = "c")

# WEG S.A. (WEGE3.SA)
wage = quantmod::getSymbols("WEGE3.SA", 
                            src = "yahoo", 
                            auto.assign = FALSE, 
                            from = "2017-01-01",
                            to = "2022-11-05",
                            return.class = 'xts')

knitr::kable(head(wage), align = "c")

# Calcular o retorno diário usando o log(p_t) - log(p_t-1)
retorno_cmig4 <- PerformanceAnalytics::Return.calculate(cmig4$CMIG4.SA.Close, method = "log")
retorno_santander <- PerformanceAnalytics::Return.calculate(santander$SANB11.SA.Close, method = "log")
retorno_renner <- PerformanceAnalytics::Return.calculate(renner$LREN3.SA.Close, method = "log")
retorno_vivo <- PerformanceAnalytics::Return.calculate(vivo$VIVT3.SA.Close, method = "log")
retorno_wage <- PerformanceAnalytics::Return.calculate(wage$WEGE3.SA.Close, method = "log")

# Alterar o nome da coluna do objeto
colnames(retorno_cmig4) <- "CMIG4.SA"
colnames(retorno_santander) <- "SANB11.SA"
colnames(retorno_renner) <- "LREN3.SA"
colnames(retorno_vivo) <- "VIVT3.SA"
colnames(retorno_wage) <- "WEGE3.SA"

# Carregar os dados dos retornos das acoes:
Acoes = data.frame(retorno_cmig4, retorno_santander$SANB11.SA, retorno_renner$LREN3.SA, retorno_vivo$VIVT3.SA, retorno_wage$WEGE3.SA)
Acoes = na.omit(Acoes)

# ------------------------------------------------

# Avaliar as correlacoes dos retornos:

(noAcoes = ncol(Acoes)) # numero de acoes (primeira coluna datas)

(matrizCorrelacao = round(cor(as.matrix(Acoes[,1:(noAcoes)])),3)*100)

# Dividir amostra antes e depois da formacao de carteiras...

Retornos = Acoes[1:1366,]        # 03/01/17 a 30/06/2022
RetornosFora = Acoes[1367:1454,] # 01/07/2022 a 04/11/04

summary(Retornos) # estatisticas descritivas retornos

# ------------------------------------------------

# Especificacoes da carteira:

(fund.names = colnames(Retornos)) # nome dos ativos

(carteira = portfolio.spec(assets = fund.names)) # criando a carteira

# Restricao 1 - carteira totalmente investida:

carteira = add.constraint(portfolio = carteira, type = "full_investment")

# Restricao 2 - apenas posicoes compradas:

carteira = add.constraint(portfolio = carteira, type = "long_only")

# Restricao 3 - para os pesos:

# carteira = add.constraint(portfolio = carteira, type = "box", min = 0, max = 0.2)

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

carteira <- add.constraint(portfolio = carteira, type = "return",return_target = 0.0008)

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
