# Aula 2: Vetores, Matrizes e Indexação

### Vetores
c(2, 3, 5, 2, 6, 7, 19, 20)
c(1:32)
v = c(4, 7, 9)
t = c(88, 19, 75, 20, 11, 0, v)

# vetores podem conter objetos de diferentes classes
nomes = c("meu", "vetor")
inteios = c(4L, 5L, 8L)
com = c(2-8i, 10+5i, 9)
mix = c(0.23, 7L, "azul")

### operações com vetores
c(1, 2, 3, 4) + c(4, 3, 2, 1)
c(2, 3, 0, 1) * c(4, 4, 4, 4)
c(1, 1, 1, 1) - c(10, 3, 5, -2)
c(2, 12, 22, 32)/c(2, 4, 4, 2)

# Vetores com dimensões diferentes
c(1, 2, 3, 4) + 7
c(3, 4, 7, 9) * c(2, 1, 9) # aviso
c(2, 3, 6, 100)/c(2, 0, 4) # aviso

### Operações vetoriais
x = c(1, 3, 7, 4)
y = c(6, 10, 0, 8)
z = c(3, 2, 1, 1, 0)

# mesma dimensão
x * y
x %*% y # produto escalar

# dimensões diferentes
x * z    # aviso
x %*% z  # erro

### Operações especiais
a = c(8, 16, 32)
b = c(4, 0, 6)

# resto da divisâo elementos de a por b
a%%b 

# inteiro da divisão elementos de a por b
a%/%b

### Avesar os elementos de um vetor
v = c(5, 120, 9, -9, 1, 0, 12, 188)
# Acessa elemento posição 4 no vetor v
v[4] 

# Acessa elementos posições 1 até 5 no vetor v
v[1:5]

# Determina dimensão do vetor
length(v)

# Acessa elementos divisíveis por 3 no vetor v
v[v %% 3 == 0]

# Resto da divisão dos elementos de v por 3
v %% 3

# Verifica quais elementos são múltiplos de 3
v %% 3 == 0

# Acessa elementos nas posições 1, 3, 8 e 10 no vetor v
v[c(1,3,8,10)]
v[c(3,8,10,1)] # não precisa ser dna ordem exata

### Matrizes
A = matrix(data = c(3, 4, 10, 1, 6, 2, 8, 0, 1, 0, 6, -1),nrow = 4,ncol = 3)

### Acessando elementos de mtrizes

# Elemento da primeira linha e terceira coluna
A[1, 3]
A[1, 3] = 4

# Todos elementos da segunda linha
A[2,]

# Elementos das linhas 1 até 3, colunas 2 até 3
A[1:3,2:3]

# Todos elementos da segunda coluna
A[,2]

# Elementos da linha 1 até linha 3, todas colunas
A[1:3,]

# Linhas 2 e 4, todas colunas
A[c(2,4),]

### Funções especiais

# Matriz diagonal
F = diag(c(2,3), 2, 2)

# Matriz identidade
Iden = diag(1, 4, 4)

# Dimensão da matriz
dim(F)

# Matriz Transposta
Ft = t(F)

# Determinante
det(F)

# Matriz inversa
solve(F)

### Sejam os seguintes vetores e matrizes
G = matrix(data = c(8, 2, -5, -4, 0, 6, 4, -2, -1, 6, 2, 9), nrow = 4)
T = matrix(data = c(4, 0, 3, 2, 12, 6), nrow = 3)
U = matrix(data = c(3, -2, 12, 7))
K = matrix(data = c(3, 7, -5, 12, 10, 0), nrow = 2)

# GT
G %*% T

# GG^-1
G %*% solve(G) # erro

# GU
G %*% U # erro

# Gt(K)
G %*% t(K)

# TK
T %*% K

# Gt(G)
G %*% t(G)

# t(K)t(T)
t(K) %*%  t(T)

### Valores "especiais"

# NA - representa os valores indisponíveis (missing) - "not available
v = c(1,2,3) ; length(v) = 4; v

# Inf e -Inf
2^1500
-1/0

# NaN - "not a number"
0/0
Inf - Inf

# NULL - objeto nulo no R, usado em funções para dizer que um determinado argumento não recebe valor algum
