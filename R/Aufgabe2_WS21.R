#nr2
#a
(factorial(60)/factorial(15)*factorial(15)*factorial(15)*factorial(15)) * (15/60)^15* (15/60)^15* (15/60)^15* (15/60)^15
#b
1-pbinom(15,60,15/60)
#nr3
#a
pnorm(220.5,213.6,4.85)
#b
qnorm(0.99)
#c
c <- tibble(
  n = 230:250,
  prob = pnorm(220,mean = n*0.89,sd = sqrt(n*0.89*0.11))
)
c
