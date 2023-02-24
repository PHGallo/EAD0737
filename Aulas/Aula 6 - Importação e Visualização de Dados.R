# Aula 6 -  Importação e Visualizaçãoo de Dados

### Importação

# Quantidade de informações
profissao = c("mecânico", "vendedor", "motorista", "advogado", "engenheira")
salarios = c(2460.87, 1452.10, 2356.29, 4563.11, 10928.00)
idade = c(41,22,38,50,46)

Colaboradores = data.frame(profissao, salarios, idade); Colaboradores

# Substituição de qualquer valor
Colaboradores[2, 3] = 36

# Inclusão de coluna
tempo = c(3, 2, 6, 8, 7)
Colaboradores = cbind(Colaboradores, tempo); Colaboradores

# Inclusão de linha
linha = data.frame(profissao = "marceneiro", salarios = 3271.19, idade = 30, tempo = 7)
Colaboradores = rbind(Colaboradores, linha); Colaboradores

# Salvar os dados no R - save()
save(Colaboradores, file = "~/DadosRemuneracao.RData")

# Exportação em outros formados
write.table(Colaboradores, "Dados.txt")
write.table(Colaboradores, "Dados.csv")

### ----------------------------------------------------------------------------
# Visualização

# Bibliotecas
library(graphics)
library(lattice)
library(ggplot2)
library(readxl)

# Importar dados
Magalu = read_excel("DadosExemplos.xlsx", sheet = "MGLU3")
Phillips = read_excel("DadosExemplos.xlsx", sheet = "Phillips")
Carteira = read_excel("DadosExemplos.xlsx", sheet = "Carteira")
Titanic = read_excel("DadosExemplos.xlsx", sheet = "Titanic")
Acoes = read_excel("DadosExemplos.xlsx", sheet = "Acoes")

# Plotar dados (linha ou scatter)
plot(Magalu$Data, Magalu$MGLU3, type = "l", main = "Preços", xlab = "Tempo", ylab = "Preço")
plot(Phillips$Desemprego, Phillips$Inflacao, type = "p", main = "Curva de Phillips", xlab = "Desemprego", ylab = "Inflação", col = "blue")

# Gráfico de barras ou pizza
barplot(Carteira$Participacao, names.arg = Carteira$Ativo, main = "Composição da Carteira",
        ylab = "Pesos", horiz = FALSE, border = "red", col = "blue", density = 10)

pie(Carteira$Participacao, labels = Carteira$Ativo, main = "Composição da Carteira")

# Histograma
hist(Titanic$Idade, main = "Histograma", xlab = "Idade", ylab = "Frequência",
     density = 13, nclass = 15, col = "purple", border = "yellow")

# Frame de Figuras
par(mfrow = c(2, 2))

for (i in 2:5) {
  plot(Acoes$Data, unlist(Acoes[,i]), type = "l", main = names(Acoes)[i])
}

par(mfrow = c(1, 1))
