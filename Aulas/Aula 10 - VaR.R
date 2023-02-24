
# Aula 10 - VaR

# ------------------------------------------------

# Carregar os pacotes necessarios:

library(readxl)

# ------------------------------------------------

# Carregar os dados BTC:

BTCMensal = read_excel("DadosAula10.xlsx",sheet = "BTCMensal")
BTCDiario = read_excel("DadosAula10.xlsx",sheet = "BTCDiario")

# ------------------------------------------------

# Calcular os retornos:

noObsMensal = nrow(BTCMensal)
noObsDiario = nrow(BTCDiario)

RetornosMensal = matrix(0,nrow = noObsMensal-1,ncol = 1)
for(i in 1:(noObsMensal-1)){
  RetornosMensal[i,1] = (as.numeric(BTCMensal[i+1,2])/as.numeric(BTCMensal[i,2]))-1
}

RetornosDiario = matrix(0,nrow = noObsDiario-1,ncol = 1)
for(i in 1:(noObsDiario-1)){
  RetornosDiario[i,1] = (as.numeric(BTCDiario[i+1,2])/as.numeric(BTCDiario[i,2]))-1
}

# ------------------------------------------------

# Distribuicao retornos:

hist(RetornosMensal,nclass = 10,xlab = "Retornos Mensais",ylab = "Frequencia",main = "Histograma dos Retornos Mensais")

hist(RetornosDiario,nclass = 20,xlab = "Retornos Diarios",ylab = "Frequencia",main = "Histograma dos Retornos Diarios")

# ------------------------------------------------

# VaR retornos mensais (simulacao historica):

VaRBTCMensal = quantile(RetornosMensal,probs = c(0.01, 0.05, 0.10))
round(VaRBTCMensal * 100, 2)

# VaR 2 meses (simulacao historica):

round(VaRBTCMensal * sqrt(2) * 100, 2)

# VaR carteira com 10 BTC (simulacao historica):

round(103700 * VaRBTCMensal, 2)

# ------------------------------------------------

# VaR diario (simulacao historica) - janela 6 meses:

# 6 meses = 6 * 22 d.u. = 132 d.u.

janela = 132 # janela movel

nivel = 0.05 # VaR de 5%

noVaR = (noObsDiario - 1) - janela # quantas vezes o VaR sera calculado

VaR_diario_HS = matrix(0, nrow = noVaR, ncol = 1)
for(i in 1:(noVaR)){
  VaR_diario_HS[i,1] = quantile(RetornosDiario[i:(i+janela-1), 1], probs = nivel)
}

plot(RetornosDiario[(janela+1):(noObsDiario-1), 1],type = "l", ylab = "Retornos", xlab = "Tempo", main = "VaR BTC HS - Janela 6 Meses")
lines(VaR_diario_HS, type = "l", col = "red")

# ------------------------------------------------

# VaR retornos mensais (desvio-padrao):

VaRBTCMensal = sd(RetornosMensal) * qnorm(c(0.10,0.05,0.01),0,1)
round(VaRBTCMensal * 100, 2)

# VaR 2 meses (DP):

round(VaRBTCMensal * sqrt(2) * 100, 2)

# VaR carteira com 10 BTC (DP):

round(103700 * VaRBTCMensal, 2)

# ------------------------------------------------

# VaR diario (desvio-padrao) - janela 6 meses:

# 6 meses = 6 * 22 d.u. = 132 d.u.

janela = 132 # janela movel

nivel = 0.05 # VaR de 5%
z = qnorm(nivel, 0, 1)

noVaR = (noObsDiario - 1) - janela # quantas vezes o VaR sera calculado

VaR_diario_DP = matrix(0, nrow = noVaR, ncol = 1)
for(i in 1:(noVaR)){
  VaR_diario_DP[i,1] = sd(RetornosDiario[i:(i+janela-1),1])*z
}

plot(RetornosDiario[(janela+1):(noObsDiario-1),1], type = "l", ylab = "Retornos", xlab = "Tempo", main = "VaR BTC HS - Janela 6 Meses")
lines(VaR_diario_HS, type = "l", col = "red")
lines(VaR_diario_DP, type = "l", col = "blue")

# ------------------------------------------------

# Estimacao volatilidade EWMA...

# Mensal

lambda = 0.94
sigma_M = 0
for(i in 2:(noObsMensal-1)){
  sigma_M[i] = sqrt(lambda*(sigma_M[i-1]^2)+(1-lambda)*(RetornosMensal[i-1,1]^2))
}
plot(sigma_M,type = "l",main = "Volatilidade EWMA",col = "red")

# Diario

lambda = 0.94
sigma_D = 0
for(i in 2:(noObsDiario-1)){
  sigma_D[i] = sqrt(lambda*(sigma_D[i-1]^2)+(1-lambda)*(RetornosDiario[i-1,1]^2))
}
plot(sigma_D,type = "l",main = "Volatilidade EWMA",col = "blue")


# ------------------------------------------------

# VaR retornos mensais (EWMA):

VaRBTCMensal = sigma_M[55]*qnorm(c(0.10,0.05,0.01),0,1)
round(VaRBTCMensal*100,2)

# VaR 2 meses (EWMA):

round(VaRBTCMensal*sqrt(2)*100,2)

# VaR carteira com 10 BTC (EWMA):

round(103700*VaRBTCMensal,2)

# ------------------------------------------------

# VaR diario (EWMA) - janela 6 meses:

# 6 meses = 6 * 22 d.u. = 132 d.u.

janela = 132 # janela movel

nivel = 0.05 # VaR de 5%
z = qnorm(nivel,0,1)

noVaR = (noObsDiario-1) - janela # quantas vezes o VaR sera calculado

VaR_diario_EWMA = matrix(0,nrow = noVaR,ncol = 1)
for(i in 1:(noVaR)){
  VaR_diario_EWMA[i,1] = sigma_D[(janela+i)]*z
}

plot(RetornosDiario[(janela+1):(noObsDiario-1),1],type = "l",ylab = "Retornos",xlab = "Tempo",main = "VaR BTC HS - Janela 6 Meses")
lines(VaR_diario_HS,type = "l",col = "red")
lines(VaR_diario_DP,type = "l",col = "blue")
lines(VaR_diario_EWMA,type = "l",col = "green")

# ------------------------------------------------

# Calcular taxa de violacao do VaR:

violacoes = matrix(0,nrow = noVaR,ncol = 3)
for(i in 1:noVaR){
  # HS:
  if(RetornosDiario[(janela+i),1] < VaR_diario_HS[i,1]){
    violacoes[i,1] = 1
  }else{
    violacoes[i,1] = 0
  }
  # DP:
  if(RetornosDiario[(janela+i),1] < VaR_diario_DP[i,1]){
    violacoes[i,2] = 1
  }else{
    violacoes[i,2] = 0
  }
  # EWMA:
  if(RetornosDiario[(janela+i),1] < VaR_diario_EWMA[i,1]){
    violacoes[i,3] = 1
  }else{
    violacoes[i,3] = 0
  }
}

# Calcular taxa de violacao:
taxaViolacao = matrix(0,nrow = 1,ncol = 3)
for(i in 1:3){
  taxaViolacao[1,i] = sum(violacoes[,i])/length(violacoes[,i])
  
}

# Mostrar taxas de violacao (HS,DP e EWMA):
round(taxaViolacao*100,2)

# ------------------------------------------------

