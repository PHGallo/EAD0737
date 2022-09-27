# Lista de Treinamento 2

### Questão 1

mercearia = data.frame(
  codigo = c(234, 111, 562, 452, 829, 198, 335, 723, 661),
  descricao = c("detergente", "macarrão", "açúcar", "molho de tomate", "desinfetante", "sabão em pó", "farinha", "arroz", "esponja"),
  tipo = c("limpeza", rep("alimento", 3), rep("limpeza", 2), rep("alimento", 2), "limpeza"),
  preco = c(2.3, 3.4, 3.19, 0.99, 3.3, 6.49, 2.99, 11.82, 4.4),
  quantidade = c(100, 78, 40, 33, 19, 28, 85, 60, 50)
); mercearia

### Questão 2

n = 0

for (i in 1:959) {
  if (i %% 3 == 0) {n = n + 1}
}

paste("Existem", n, "números de 1 a 959 divisíveis por 3")

### Qustão 3

X = rep(0, 64)

for (i in 1:64) {
  X[i] = log(i, base = 15)
}

matriz = matrix(X, nrow = 8, ncol = 8, byrow = T)

### Questão 4

somaN = function () {
  n = as.numeric(readline("Digite n: "))
  X = seq(1, n)
  paste("Usando a função 'sum()', a soma de 1 até", n, "é:", sum(X))
}

somaN2 = function() {
  n = as.numeric(readline("Digite n: "))
  X = seq(1, n)
  paste("Usando a fórmula (n*(n+1))/2, a soma de 1 até", n, "é:", (n*(n+1))/2)
}

### Questão 5

variancia = function (X) {
  v = 0
  for (i in 1:length(X)) {
    v = v + (X[i] - mean(X))^2
  }
  print(1/(length(X)-1)*v)
  print(var(X))
}

variancia(rnorm(100))

### Questão 6

v1 = c(11, 9, 8, 7, 11, 6, 7, 9)
v2 = c(1, 2, 1, 2, 1, 2, 1, 2)

mediaPonderada = function(v1, v2) {
  mediapond = sum(v1 * v2)/ sum(v2)
  print(mediapond)
  print(weighted.mean(v1, v2))
}

### Questão 7

conversao = function () {
  celsius = as.numeric(readline("Digite a temperatura, em graus Celsius: "))
  paste("A temperatura, em graus Farenheit, é", 9/5 * celsius + 32)
}

### Questão 8

fatorial = function () {
  n = as.numeric(readline("Digite n: "))
  valor = 1
  for (i in 1:n) {
    valor = valor * i
  }
  print(valor)
  print(factorial(n))
}

### Questão 9

parouimpar = function() {
  n = as.numeric(readline("Digite n: "))
  if (n%%2 == 0) {
    paste(n, "é par")
  } else {
    paste(n, "é impar")
  }
}

### Questão 10

menorseq = function () {
  n = rep(0, 3)
  for (i in 1:3) {
    n[i] = as.numeric(readline("Digite n: "))
  }
  if (min(n) == n[2]) {
    print("O segundo número informado é menor entre todos")
  } else {
    print("O segundo número informado não é menor entre todos")
  }
}


### Questão 11

circunferencia = function (x, y, raio) {
  u.x = as.numeric(readline("Digite o valor de x: "))
  u.y = as.numeric(readline("Digite o valor de y: "))
  if ((u.x - x)^2 + (u.y - y)^2 < raio^2) {
    print("O ponto é interno à circunferência")
  } else {
      print(("O ponto não é interno à circunferência"))
  }
}

### Questão 12

aprovacao = function (n1, n2, n3) {
  notas = c(n1, n2, n3)
  if (min(notas) > 3 && mean(notas) >= 7) {
    print("Aprovado")
  } else {
    print("Reprovado")
  }
}

### Questão 13

operacoes = function (n1, n2) {
  linhas <- paste("Soma:", n1 + n2,
            "Produto:", n1 * n2,
            "Diferença:", n1 - n2,
             "Quociente (real):", n1 / n2,
             "Quociente (inteiro):", n1 %/% n2,
             "Resto da divisão:", n1 %% n2,
             "Exponenciação:", n1^n2,
             sep = "\n")
  
  cat(stringr::str_wrap(linhas))
}

### Questão 14

combinacao = function(n, p) {
  if (p < 0) {
    print("valor p negativo")
  } else {
    print(factorial(n)/(factorial(p)*factorial(n - p))) 
  }
}

### Questão 15

eq = function(a, b, c){
  if(a == 0) {
    print("A equação não é de segundo grau") 
  } else {
    delta = (b^2) - (4*a*c)
    if(delta < 0) {
      x1 = (-b - complex(1, 0, sqrt(abs(delta)))) / (2*a)
      x2 = (-b + complex(1, 0, sqrt(abs(delta)))) / (2*a)
      paste("A função tem raízes imaginarias e elas são iguais a", round(x1, 2), "e", round(x2, 2)) 
    }
    else if(delta > 0) {
      x1 = (-b - sqrt(delta)) / (2*a)
      x2 = (-b + sqrt(delta)) / (2*a)
      paste("A função tem raízes reais e elas são iguais a", round(x1, 2), "e", round(x2, 2))
    }
    else if(delta == 0) {
      x1 = (-b+sqrt(delta))/(2*a)
      paste("A função tem duas raízes iguais a", round(x1, 2))
    }
  }  
}

eq(3, -8, 3)
eq(1/4,-5,25)
eq(1, -3, 3)
