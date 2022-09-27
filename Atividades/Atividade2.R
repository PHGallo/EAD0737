# Sucessão de Fibonacci

# Crie a sequência de Fibonacci com os 500 elementos iniciais
Fibonacci = c(1, 1)
for (i in 3:500) {
  Fibonacci[i] <- Fibonacci[i - 2] + Fibonacci[i - 1]
}

# Vetor contendo os 10 primeiros elementos que restam 1 na divisão por 2
vetor = numeric(10)
cont = 1
i = 1

while (cont < 11) {
  if (Fibonacci[i] %% 2 == 1) {
    vetor[cont] = Fibonacci[i]
    cont = cont + 1
  }
  i = i + 1
}

# Soma dos 10 elementos
sum(vetor)
