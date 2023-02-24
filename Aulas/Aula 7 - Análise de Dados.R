# Aula 7 - Análise de Dados

### Dados: preços ações Carrefour e Arezzo
library(readxl)
DadosExemplos2 = read_excel("DadosExemplos2.xlsx")
summary(DadosExemplos2)
attach(DadosExemplos2)

# Retornos
DadosExemplos2[,4:5] = 0
colnames(DadosExemplos2)[4] = "RetCarr"
colnames(DadosExemplos2)[5] = "RetArez"

DadosExemplos2$RetCarr = c(0, diff(log(CRFB3)))
DadosExemplos2$RetArez = c(0, diff(log(ARZZ3)))

# Plot 
plot(DadosExemplos2$Data, DadosExemplos2$CRFB3, type = "l", col = "red", ylim = c(15, 70), ylab = "Preço", xlab = "Tempo")
lines(DadosExemplos2$Data, DadosExemplos2$ARZZ3, col = "blue")

plot(DadosExemplos2$Data, DadosExemplos2$RetCarr, type = "l", col = "red", ylab = "Retornos", xlab = "Tempo")
plot(DadosExemplos2$Data, DadosExemplos2$RetArez, type = "l", col = "blue", ylab = "Retornos", xlab = "Tempo")

# Estatísticas Descritivas

# Variância amostral
var(DadosExemplos2$RetCarr)
var(DadosExemplos2$RetArez)

# Covariancia amostral
cov(DadosExemplos2$RetCarr, DadosExemplos2$RetArez)

# Correlação amostral 
cor(DadosExemplos2$RetCarr, DadosExemplos2$RetArez)

# Desvio padrão amostral
sd(DadosExemplos2$RetCarr)
sd(DadosExemplos2$RetArez)

# Assimetria e curtose
library(moments)

skewness(DadosExemplos2$RetCarr)
kurtosis(DadosExemplos2$RetCarr)

skewness(DadosExemplos2$RetArez)
kurtosis(DadosExemplos2$RetArez)

# Histograma
hist(DadosExemplos2$RetCarr)
hist(DadosExemplos2$RetArez)

# Boxplot
boxplot(DadosExemplos2$RetCarr)
boxplot(DadosExemplos2$RetArez)

# -------------------------------------------------------------------------
### Números Aleatórios e Simulação

# Criar função de densidade

x = seq(-10, 25, 0.1)
fdp = dnorm(x, mean = 3.5, sd = sqrt(14))
plot(x, fdp, type = "l")
pnorm(4, mean = 4.5, sd = 2) # P(X < 4), X ~ N(4.5, 4)
rnorm(500, mean = 3.5, sd = 3) # números aleatórios

# Funções estatísticas - Distribuição Normal

dnorm(x) # densidade Normal para o valor x
plot(dnorm, -3, 3, main = "Normal")

p = pnorm(3) # P(X <= 3)
plot(pnorm, -3, 3, main = "Normal Cumulativa")

q = qnorm(p) # P(X <= q) = p

rnorm(100) # gerador de números aleatórios

# -------------------------------------------------------------------------
### Problema 1

# i 

x = seq(210, 260, 0.1)

# Função Densidade de Probabilidade
fdp = dnorm(x, mean = 235, sd = 7)
plot(x, fdp, type = "l", main = "Normal")

# Função de distribuição Cumulativa
fdc = pnorm(x, mean = 235, sd = 7)
plot(x, fdc, type = "l", main = "Normal")

# ii - P(X > 242.5), X ~ N(235, 7)
1 - pnorm(242.5, mean = 235, sd = 7) 

# iii - P(221.12 < X < 239.89), X ~ N(235, 7)
pnorm(239.89, mean = 235, sd = 7) - pnorm(221.12, mean = 235, sd = 7)

# -------------------------------------------------------------------------
### Problema 2

# Considerando que 199 dias
# Gerando uma amostra com distribuição normal
# Média = 0 e Desvio-padrão = 1
n = 199 # Número de dias
U = rnorm(n)
hist(U, main = "Histograma da distribuição gerada U ~ N(0,1)",
     ylab = "Frequência observada",
     xlab = "Intervalo")

# Considerando o ponto de partida como y = 20
# O Random Walk pode ser simulado do seguinte modo:
y = c(20, 20 + cumsum(U)) # Soma Acumulativa
plot(y, main = "Simulação do Randon Walk",
     type = "l",xlab="t",ylab="y(t)",
     lwd=2)

# Sumário do Random Walk simulado
summary(y)
sd(y) # Desvio-Padrão

hist(y, main = "Histograma do Randon Walk gerado",
     ylab = "Frequência Observada",
     xlab = "Intervalo")
