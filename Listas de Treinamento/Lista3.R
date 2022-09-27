# Lista de Treinamento 3

### Questão 1

for (i in 7:133) {
  if (i %% 2 != 0) {print(i)}
}

### Questão 2

is.prime <- function(n) n == 2L || all(n %% 2L:max(2,floor(sqrt(n))) != 0)

Filter(is.prime, 0:100)

### Questão 3

for (i in 5:400) {
  if (i %% 5 == 0) {print(i)}
}

### Questão 4

cont = 1
n = 1

repeat {
  if(n >= 10^10){
    print(cont)
    break
  } else {
    cont = cont + 1
    n = n * cont
  }
}

### Questão 5

exponencial = function() {
  n = as.numeric(readline("Digite n: "))
  x = as.numeric(readline("Digite x: "))
  e = 0
  for (i in 0:n) {
    e = e + x^i/factorial(i)
  }
  paste("Fórmula:", e, "Função:", exp(x), "Diff:", abs(e - exp(x)))
}

### Questão 6

vendas = matrix(c(2, 7, 10, 4, 9, 1, 5, 9, 10, 12, 11, 8, 0, 13, 22, 24, 10, 32, 0, 0, 5, 9, 20, 14, 8, 18, 5, 5, 9, 16, 15, 14, 11, 12, 32), nrow = 7, ncol = 5, byrow = TRUE)

custo = c(1.55, 2.27, 5.47, 3.80, 3.15)
# total de itens vendidos na semana
sum(vendas)

# total de itens vendidos por dia da semana
total_venda = data.frame(dias = c("seg", "ter", "qua", "qui", "sex", "sab","dom"), vendas = rowSums(vendas)); total_venda

# dia da semana de maior venda
total_venda$dias[which.max(total_venda$vendas)]

# lucro total da semana
lucro = 0
for (i in 1:nrow(vendas)) {
  lucro = lucro + sum(vendas[i,]*custo)
}
print(lucro)

# lucro por dia da semana
lucros = rep(0, 7)
for (i in 1:nrow(vendas)) {
  lucros[i] = sum(vendas[i,]*custo)
}
total_lucro = data.frame(dias = c("seg", "ter", "qua", "qui", "sex", "sab","dom"), lucro = lucros); total_lucro

# dia da semana de maior lucro
total_lucro$dias[which.max(total_lucro$lucro)]

# total de de itens vendidos por produtos
total_produto = data.frame(produto = c(1, 2, 3, 4, 5), vendas = colSums(vendas)); total_produto

# produto mais vendido
total_produto$produto[which.max(total_produto$vendas)]

# lucro por produto
lucro_produto = rep(0, 5)
for (i in 1:ncol(vendas)) {
  lucro_produto[i] = sum(vendas[,i]*custo[i])
}
total_lucro_produto = data.frame(produto = c(1, 2, 3, 4, 5), lucro = lucro_produto); total_lucro_produto

# produto de maior lucro
total_lucro_produto$produto[which.max(total_lucro_produto$lucro)]

### Questão 7
fib = function() {
  n = as.numeric(readline("Quantos termos? "))
  fibonacci = c(1, 1, rep(0, n - 2))
  for (i in 3:n) {
    fibonacci[i] = fibonacci[i-1] + fibonacci[i-2]
  }
  print(fibonacci)
}

### Questão 8
meia_vida = function() {
  massa = as.numeric(readline("Digite a massa inicial: "))
  massai = massa
  t = 0
  
  while (massa > 0.5) {
    massa = massa/2 
    t = t + 50
  }
  
  paste("Massa inicial:", massai, "Massa final:", massa, "Tempo:", t)
}

### Questão 9
quadrado = function() {
  n = as.numeric(readline("Digite n: "))
  valor = 1
  for (i in 0:abs(n) - 1) {
    valor = valor + (2 * i + 1)
  }
  print(valor)
}

### Questão 10

f = function(x) {
  x^2
}

simpson = function() {
  n = as.numeric(readline("Digite n: "))
  a = as.numeric(readline("Digite a: "))
  b = as.numeric(readline("Digite b: "))
  
  h = (b - a) / n
  valor = 0
  
  for (i in 0:n) {
    x = a + i * h
    if (i == 0 || i == n) {
      valor = valor + (h/3) * f(x)
    } else if (i %% 2 == 0) {
      valor = valor + (2*h/3) * f(x)
    } else {
      valor = valor + (4*h/3) * f(x)
    }
  }
  print(valor)
  print(integrate(f, lower = a, upper = b))
}
