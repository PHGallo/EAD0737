# CAPM - Atividade

# ------------------------------------------------

# Carregar os pacotes necessarios:

library(readxl)

# ------------------------------------------------

# Carregar os dados dos retornos das acoes

Dados <- read_excel("Santander_semcovid.xlsx")

# ------------------------------------------------

# Calcular os retornos

(noEmpresas = ncol(Dados) - 2) # exceto data e CDI (ja eh retorno)
(noObservacoes = nrow(Dados))

Retornos = matrix(0,nrow = noObservacoes-1, ncol = noEmpresas)
for(j in 1:noEmpresas){
  for(i in 1:(noObservacoes-1)){
    Retornos[i,j] = (as.numeric(Dados[i+1,j+1])/as.numeric(Dados[i,j+1])) - 1
  }
}

colnames(Retornos) = c("Santander", "IBOV")

# ------------------------------------------------

# Calcular o CDI diario

DI_diario = 0
for(i in 2:noObservacoes){
  DI_diario[i-1] = ((1+(as.numeric(Dados$CDI[i])/100))^(1/252))-1
}

# ------------------------------------------------

# Criar a matriz de dados dos premios pelo risco (retorno - DI_diario)

Premios = matrix(0,nrow = noObservacoes-1,ncol = noEmpresas)
for(j in 1:noEmpresas){
  for(i in 1:nrow(Retornos)){
    Premios[i,j] = Retornos[i,j] - DI_diario[i]
  }
}

Premios = as.data.frame(Premios)
colnames(Premios) = c("PSantander","PIBOV")

# ------------------------------------------------

# Estimar a regressao do CAPM para JBS com intercepto (alpha)

CAPM_SANTANDER_1 = lm(PSantander ~ PIBOV, data = Premios)
summary(CAPM_SANTANDER_1)

# Estimar a regressao do CAPM para JBS sem intercepto (alpha)

CAPM_SANTANDER_2 = lm(PSantander ~ 0 + PIBOV, data = Premios)
summary(CAPM_SANTANDER_2)

# Analise dos residuos do modelo

plot(CAPM_SANTANDER_2$residuals)
hist(CAPM_SANTANDER_2$residuals, nclass = 50)

# Se R_F = 1,41% a.a., R_M = 26,89% a.a., qual custo de capital do Santander?

Ke = 0.0141 + as.numeric(CAPM_SANTANDER_2[["coefficients"]])*(0.2689 - 0.0141) + 0.0282 
paste("O custo de capital proprio da JBS e:", round(Ke*100,2), "% a.a.")

# Se R_F = 13,75% a.a., R_M = 31,58% a.a., qual custo de capital do Santander?

Ke = 0.1375 + as.numeric(CAPM_SANTANDER_2[["coefficients"]])*(0.3158 - 0.1375)
paste("O custo de capital proprio da JBS e:", round(Ke*100,2), "% a.a.")
