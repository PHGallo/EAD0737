# Aula 5 - Programação Orientada II

### Operadores de Loops/Recursão

# repeat

popA = 80000   # população A
popB = 200000  # população B
crescA = 0.03  # crescimento da população A
crescB = 0.015 # crescimento da população B
n = 0          # variável que guarda o número de anos

repeat {
  if(popA >= popB){
    break
  } else {
    n = n + 1                # acreseneta 1 ano
    popA = popA * (1+crescA) # população A cresce 
    popB = popB * (1+crescB) # população B cresce 
  }
}

paste("O número de anos é:", n)

# while

popA = 80000   # população A
popB = 200000  # população B
crescA = 0.03  # crescimento da população A
crescB = 0.015 # crescimento da população B
n = 0          # variável que guarda o número de anos

while (popA < popB) {
  n = n + 1                # acreseneta 1 ano
  popA = popA * (1+crescA) # população A cresce 
  popB = popB * (1+crescB) # população B cresce 
}

### Problema 1 - Sorteio da Mega Sena
meujogo = sort(c(10, 34, 44, 21, 45, 58)) # meu jogo em ordem crescente
nS = 1                                    # número de sorteios necessários para ser vencedor
sorteio = sort(sample.int(60, size = 6, replace = FALSE))

while (all(sorteio == meujogo) == FALSE) {
  nS = nS + 1 
  sorteio = sort(sample.int(60, size = 6, replace = FALSE))
  if (nS > 500) {break}
}

# for

# Vetor com o tempo de estacionamento de cada cliente em um dia
x = c(1.5, 4.8, 6.9, 12, 12, 0.5, 23, 14, 10.8, 9.7, 6.5, 5, 4.3)

# Número de clientes
nClientes = length(X)

# Vetor com os valores pagos por cliente
Gastos = rep(0, nClientes)

# Calcular o gasto de cada cliente
for (i in 1:nClientes) {
  if (x[i] <= 3) {
    Gastos[i] = 2
  } else {
    Gastos[i] = 2 + 0.5 * (round((x[i] - 3), 0))
  }
  if (Gastos[i] > 10) {
    Gastos[i] = 10
  }
}

sum(Gastos)