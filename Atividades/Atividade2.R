# Problema do Coelho de Fibonacci

par_coelhos = 15
par_ferteis = 12
meses = 0

while (par_coelhos < 5800) {
  meses = meses + 1
  aux = par_ferteis
  par_ferteis = par_coelhos
  par_coelhos = par_coelhos + aux
}

print(meses)

