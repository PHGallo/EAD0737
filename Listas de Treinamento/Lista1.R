# Lista de Treinamento 1

### Questão 1

# i
4 * (2/5 - 7)

# ii
12 - 5800 * (4 * 0.2 - 18 + 12 * 0.11)

# iii
99/12 - 6726 * 56/0.293 + 15

# iv
y = 42/17
y * 256 - (45 * 40.9 +2)/(763 * 0.4)

# v
x = 56 - 0.1 * 12/(-0.9)
(x^(2 * 0.5 + 4))/(32 - 4 * 0.6)

# vi
(12 * 8 - 0.5 * 7625)/(4 * 2 + 6 * 8 - 56)

# vii
56^(4 - 2 * 0.6) * 12^(-12 * 0.227 - 1) * (-0.762)/(9^(12 * 1 - 8))

# viii
234 - (12 - 4i)/(0.6 * 12 -9)

### Questão 2

# i
v = c(-3, 4, 0.5, 12, 45)

# ii
u = c(-0.1, 0.34, 93, 2, 1, 0, 4)

# iii
t = c(8, -0.9, 10, 3, -1)

# iv
p = c(3, 4, -3, -4, 0, 1)

# v
X = matrix(data = c(4, -0.9, 2, 5, -1, 4, 13, 2), nrow = 2)

# vi
Z = matrix(data = c(4, -1, 6, 10, 2, -3, 0.4, 11, 8, -7, -9, 12), nrow = 4)

# vii
W = matrix(data = c(-2, 3, 0.4, 0.9, 9, 8, 8, 32, -66, -7, 10, 12, 10, 12, 0.22, 0, 31, -2, 98, 4, -1, -8, -2, 0, -33), nrow = 5)

# viii
K = matrix(data = c(-2, -1, 14, 0.5, 6, 0, 10, -44), nrow = 4)

# ix
G = matrix(data = c(-3, 19, 0.1, 8, 13, 0.2, 0.3, -17, -0.3), nrow = 3)

### Questão 3

# i
t %*% v

# ii
u / u  # 0/0 = NaN

# iii
t %*% p - v  # dimensões diferentes

# iv
X %*% Z

# v
W %*% W

# vi
Z * Z ###

### Questão 4

# i
v * u # dimensões diferentes

# ii 
v %*% v

# iii
v %*% t(p) 

# iv
X %*% t(X)

# v
Z %*% G

# vi
solve(G) %*% G

# vii
solve(K) %*% G # matriz K não é quadrada

# viii
t(v) %*% W

# ix
W %*% solve(W) - diag(1,5,5)

# x
solve(X %*% K)

### Questão 5

# i -  calcule o determinante das matrizes W, K e G
det(W)
det(K) # matriz K não é quadrada
det(G)

# ii - crie uma variavel que guarde os elementos da segunda e terceira linhas de ´ Z para todas as colunas
mZ = Z[2:3,]

# iii - crie uma variavel que guarde o elemento da terceira linha e quarta coluna de W
mW = W[3,4]

# iv - crie uma variavel que guarde os elementos de W que sao divisiveis por 4
mW4 = W %% 4 == 0

# v - crie uma variavel que guarde o resto da divisao dos elementos de G por 2
mG2 = G %% 2

# vi - crie uma variavel que guarde o inteiro da divisao dos elementos de Z por 4
mZ4 = Z %/% 4

# Questão 6 - sistema linear

# i - construa a matriz A e o vetor b
A = matrix(data = c(4, 2, 2, 2, -2, -3, -3, 4, 6, 10, 7, -1, -4, -1, 1, 10), nrow = 4)
b = c(-10, 11, -2, 13)

# ii - verificar se o sistema é SPD
det(A) # o determinante de A é diferente de zero, logo tem solução e essa solução é única

# iii - determinar a solução do sistema em sua forma matricial
x = solve(A) %*% b

# iv - verificar se a solução satisfaz o sistema, ou seja, se Ax = b
A %*% x
