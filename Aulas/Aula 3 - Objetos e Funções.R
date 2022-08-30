# Aula 3 - Objetos e Funções

### Factor
sexo = c("Masculino", "Feminino", "Feminino", "Masculino"); sexo; class(sexo)
fatorsexo = as.factor(sexo); fatorsexo; class(fatorsexo) 

### Acessando elementos de fator

# Elemento da tercira linha
fatorsexo[3]

# Elementos das linhas 1 até 3
fatorsexo[1:3]

### Dataframe - tabela de informações - combina vetores, matrizes, fatores
idade = c(20, 43, 18, 35)

basededados = data.frame(idade, fatorsexo); basededados; class(basededados)

### Acessando elementos de dataframe

# Elementos da coluna idade
basededados$idade

# Elementos da coluna fatorsexo
basededados$fatorsexo

# Elemento da segunda linha da coluna idade
basededados$idade[2]

# Elemento da quarta linha da coluna fatorsexo
basededados$fatorsexo[4]

# Elementos das primeira e terceira linha da coluna fatorsexo
basededados$fatorsexo[c(1,3)]

### Listas 
lista = list(idade, sexo, fatorsexo, basededados); lista; class(lista)

### Acessando elementos de lista

# Elementos da primeira variável
lista[[1]]

# Elemento do terceiro componente da primeira variável
lista[[1]][3]

# Elementos dos segundo e terceiros componentes da segunda variável
lista[[2]][2:3]

## Elemento do dataframe dentro da lista 

# A idade da quarta linha
lista[[4]]$idade[4]
lista[[4]][4, 1]

# Todos os elementos da quarta linha
lista[[4]][4,]

### Funções - nomeDaFuncao(Arg1, ..., ArgN)

# help("matrix")
A = matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2, byrow = TRUE, dimnames = list(c("L.1", "L.2"), c("C.1", "C.2"))); A

# imprime na tela a mensagem Hello World
print("Hello World")

# imprime na tela uma mensagem que combina texto e numeros
paste("O indivíduo da quarta linha tem", basededados$idade[4], "anos de idade")

# recebe valores indicados pelo usuário
idade = readline("Digite a sua idade, em anos: ")
class(idade)
idade = as.numeric(idade)

idade = as.numeric(readline("Digite a sua idade, em anos: "))
class(idade)

# Criação de funções
Funcao = function(x, y) {
  ((x^2 - 4 * y + 2)/(y^2 + 1)) - 10
}
Funcao(x = 1, y = 2)
valor1 = Funcao(4, 5)

SomaProduto = function(v1, v2, v3) {
  soma = sum(v1, v2, v3)
  produto = prod(v1, v2, v3)
  return(c(soma, produto))
}
SomaProduto(1, 2, 4)

CalculadoraIMC = function() {
  peso = as.numeric(readline("Digite seu peso, em Kg: "))
  altura = as.numeric(readline("Digite sua altura, em m: "))
  IMC = round(peso / altura^2, 2)
  paste("Seu IMC é:", IMC)
}
CalculadoraIMC()
