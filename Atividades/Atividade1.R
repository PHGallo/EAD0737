# Matriz Idempotente 

# Em álgebra, uma matriz idempotente é uma matriz que, ao ser multiplicada por si mesma, resulta em si mesma

# Matriz A
A = matrix(c(3,15,20,2,40,6,1,9,10,9,18,1),4) 

# Matriz projeção de A
P = A %*% solve(t(A) %*% A) %*% t(A); P

# Verificar se a matriz de projeção projeção é idempontente
P %*% P

# Soma dos elementos da sua diagonal principal da matriz de projeção
sum(diag(P))
