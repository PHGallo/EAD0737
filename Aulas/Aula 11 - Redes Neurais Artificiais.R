
# Aula 11 - Redes Neurais Artificiais

# ------------------------------------------------

# Carregar os pacotes necessarios

library(readxl)
library(neuralnet)

# ------------------------------------------------

cat("\f") # Limpar o console

rm(list = ls()) # Limpar todas as variaveis

# ------------------------------------------------

# Carregar os dados IPCA (variacao % mensal Jan/00 a Set/20):

IPCA = read_excel("DadosAula11.xlsx")

# Plotar os dados:

plot(IPCA$Data,IPCA$IPCA,type = "l",xlab = "",ylab = "Variacao (%)",main = "IPCA Mensal",col = "blue")


# ------------------------------------------------

# Passo 1: normalizacao dos dados

# Construir função para normalizar (min-max):

normalize = function(x){
  return((x-min(x))/(max(x)-min(x)))
}

# Normalizar os dados:

IPCA_norm = normalize(IPCA$IPCA)

plot(IPCA$Data,IPCA_norm,type = "l",xlab = "",ylab = "Variacao (%)",main = "IPCA Mensal Normalizado",col = "blue")


# ------------------------------------------------

# Passo 2: definir modelo de previsao...

# Quantos valores passados para prever o IPCA do proximo mes?

no = length(IPCA_norm)

base = as.data.frame(cbind(IPCA_norm[1:(no-3)],
                           IPCA_norm[2:(no-2)],
                           IPCA_norm[3:(no-1)],
                           IPCA_norm[4:no]))

# ------------------------------------------------

# Passo 3: dividir amostras em treinamento e validacao:

n = 20 # numero de obs deixadas para previsão

IPCAin = base[1:(nrow(base)-n),]
IPCAout = base[(nrow(base)-n+1):(nrow(base)),]

# ------------------------------------------------

# Passo 4: definir estrutura e treinar RNA:

modeloRede = neuralnet(
  V4 ~ V1 + V2 + V3, # modelo de previsao considerado;
  data = IPCAin, # base de dados treinamento;
  act.fct = "tanh", # tangente hiperbolica;
  hidden = c(5,4,3,2,1), # camadas e neuronios em cada uma.
  threshold = 0.01, 
  stepmax = 1e+05,
  lifesign.step = 1000, 
  algorithm = "rprop+"
)

# Visualizar rede neural:

plot(modeloRede)

# Guardar a saida do modelo no treinamento:

saida_Treino = as.matrix(modeloRede[["net.result"]][[1]])

# Visualizar ajuste da rede no treinamento:

plot(IPCA$Data[1:(nrow(base)-n)],IPCAin[,3],xlab = "",ylab = "Variacao (%)",type="l",main = "IPCA Real e Previsto - Amostra Treino")
lines(IPCA$Data[1:(nrow(base)-n)],saida_Treino,col="red")

# ------------------------------------------------

# Passo 5: realizar previsões na amostra teste.

previsao = predict(modeloRede,IPCAout[,1:4])

# Visualizacao:

plot(IPCA$Data[(nrow(base)-n+1):(nrow(base))],IPCAout[,3],xlab = "",ylab = "Variacao (%)",type="l",main = "IPCA Real e Previsto - Amostra Teste")
lines(IPCA$Data[(nrow(base)-n+1):(nrow(base))],previsao,col = "red")

(erro_dentro2 <- mean((saida_Treino - IPCAin$V4)^2))
(erro_dentro2 <- mean((previsao - IPCAout$V4)^2))

# ------------------------------------------------
