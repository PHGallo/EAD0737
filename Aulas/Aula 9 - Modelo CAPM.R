# Aula 9 - CAPM

# ------------------------------------------------

# Carregar os pacotes necessarios

library(readxl)

# ------------------------------------------------

# Carregar os dados dos retornos das acoes

Dados = read_excel("DadosAula9.xlsx")

# ------------------------------------------------

# Calcular os retornos

noEmpresas = ncol(Dados) - 2 # exceto data e CDI (ja eh retorno)
noObservacoes = nrow(Dados)

Retornos = matrix(0,nrow = noObservacoes-1,ncol = noEmpresas)
for(j in 1:noEmpresas){
  for(i in 1:(noObservacoes-1)){
    Retornos[i,j] = (as.numeric(Dados[i+1,j+1])/as.numeric(Dados[i,j+1])) - 1
  }
}

colnames(Retornos) = c("JBS","Sabesp","Petrobras","Renner","IBOV")

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
colnames(Premios) = c("PJBS","PSabesp","PPetrobras","PRenner","PIBOV")

# ------------------------------------------------

# Estimar a regressao do CAPM para JBS com intercepto (alpha)

CAPM_JBS_1 = lm(PJBS ~ PIBOV, data = Premios)
summary(CAPM_JBS_1)

# Estimar a regressao do CAPM para JBS sem intercepto (alpha)

CAPM_JBS_2 = lm(PJBS ~ 0 + PIBOV, data = Premios)
summary(CAPM_JBS_2)

# Analise dos residuos do modelo

plot(CAPM_JBS_2$residuals)
hist(CAPM_JBS_2$residuals, nclass = 50)

# Se R_F = 4% a.a., R_M = 15% a.a., qual custo de capital da JBS?

Ke = 0.04 + as.numeric(CAPM_JBS_2[["coefficients"]])*(0.15 - 0.04)
paste("O custo de capital proprio da JBS e:", round(Ke*100,2), "% a.a.")

# ------------------------------------------------

# Estimar a regressao do CAPM para Sabesp com intercepto (alpha):

CAPM_Sabesp_1 = lm(PSabesp ~ PIBOV,data = Premios)
summary(CAPM_Sabesp_1)

# Estimar a regressao do CAPM para Sabesp sem intercepto (alpha):

CAPM_Sabesp_2 = lm(PSabesp ~ 0 + PIBOV,data = Premios)
summary(CAPM_Sabesp_2)

# ------------------------------------------------

# Estimar a regressao do CAPM para Petrobras com intercepto (alpha):

CAPM_Petrobras_1 = lm(PPetrobras ~ PIBOV,data = Premios)
summary(CAPM_Petrobras_1)

# Estimar a regressao do CAPM para Petrobras sem intercepto (alpha):

CAPM_Petrobras_2 = lm(PPetrobras ~ 0 + PIBOV,data = Premios)
summary(CAPM_Petrobras_2)

# ------------------------------------------------

# Estimar a regressao do CAPM para Renner com intercepto (alpha):

CAPM_Renner_1 = lm(PRenner ~ PIBOV,data = Premios)
summary(CAPM_Renner_1)

# Estimar a regressao do CAPM para Renner sem intercepto (alpha):

CAPM_Renner_2 = lm(PRenner ~ 0 + PIBOV,data = Premios)
summary(CAPM_Renner_2)

# ------------------------------------------------
