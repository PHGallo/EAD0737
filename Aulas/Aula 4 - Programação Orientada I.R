# Aula 4 - Programação Orientada I

### Operações Lógicas

# Igualdade
3 == 5
6 == 12/2

# Não igual
10 != 2 * 5
10 != 3 +8

# Menor
5 < 3
5 < 8

# Maior
5 > 3
5 > 8

# Menor o igual
5 <= 3
5 <= 8

# Maior ou igual
5 >= 3
5 >= 8

# E
2 >= 0 && 4 >= 8
10 != 20 && 20 != 20 && 30 != 20

# OU
2 >= 0 || 4 >= 8
10 != 20 || 20 != 20 || 30 != 20

### Operador Condicional SE
if (2 > 7) {"Verdade"}

if (x >= 0) {sqrt(x)} else {"Não possui raiz quadrada"}
if (z != 3) {z + 1} else {z - 1}
if (t == 0) {"Zero"}else {"Diferente de zero"}

### Função para cálculo da Raiz Quadradea
numero = as.numeric(readline("Digite um número: "))
if (numero >= 0){
  raiz = sqrt(numero)
  paste("A raiz de",numero, "é", round(raiz, 2))
} else {
  paste("O número não possui raiz quadrada")
}

### Declaração condicional em uma função
f = function(x, y) {
  if (x > y) {x + y} else {x - y}
}
f(1, 2)
f(0, 0)
f(-100, sqrt(7))

g = function(s, t) {
  if (s >= t) {soma = s + t
  subtracao = s - t
  paste("Soma é", soma, "e Subtração é", subtracao)
  } else {produto = s * t
  paste("Produto é", produto)}
}

### Problema 1 - Função Definida por Partes
f <- function(x){
  ifelse((x <= 0), x^2, ifelse((x <= 2), sin(x), ifelse((x > 2), 4 * x + 1, NA)))
}

f(-3)
f(pi/2)
f(3)

### ifelse
x = 10
y = c(8, 13, 12, 3, 17)

if (x < y) {x} else {y} # erro
ifelse(x < y, x, y)

### Problema 2 - Calculadora IMC
calculaIMC = function() {
  altura = as.numeric(readline("Digite sua altura, em metros:"))
  peso = as.numeric(readline(" Digite seu peso, em quilos:"))
  if (peso <= 0 || altura <=0) { stop("Não existe peso/altura negativo ou igual a zero") }  
  IMC = peso/altura^2
  if (IMC < 18.5) {print("Abaixo do peso ideal")}
  else if(IMC < 24.9) {print("Peso ideal")}
  else if(IMC < 29.9) {print("Sobrepseo")}
  else if(IMC < 34.9) {print("Obesidade grau 1")}
  else if(IMC < 39.9) {print("Obesidade grau 2")}
  else {print("Obedidade grau 3")}
}
calculaIMC()
