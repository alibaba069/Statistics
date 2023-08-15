
#nr4
x <- c(247.4, 249.0, 248.5, 247.5, 250.6, 252.2, 253.4, 248.3,
       251.4, 246.9, 249.8, 250.6, 252.7, 250.6, 250.6, 252.5, 249.4, 250.6, 247.0, 249.4)
x
alpha <- 0.5
sample.mean <- mean(x)
sample.sd <- sd(x)
n <-length(x)
n
#a)
sigma <- 2
#c(sample.mean-qt(1-alpha/2.df = n-1)*sigma/n^0.5,sample.mean+qt(1-alpha/2.df = n-1)*sigma/n^0.5)
######
#nr2
c <- c(8, 9, 10, 13, 14, 16, 17, 20, 21)
mean(c)
sigma <- 2.8
qnorm(1-0.05/2)
xu <- 14.22-qnorm(1-0.05/2)*2.8/sqrt(9)
xo <- 14.22+qnorm(1-0.05/2)*2.8/sqrt(9)

qnorm(1-0.01/2)
xu2 <- 14.22-qnorm(1-0.01/2)*2.8/sqrt(9)
xo2 <- 14.22+qnorm(1-0.01/2)*2.8/sqrt(9)
#nr3
alpha3 <- 0.01
sd3 <- 10
u3 <- 60-qnorm(1-0.01/2)*10/sqrt(22)
o3 <- 60+qnorm(1-0.01/2)*10/sqrt(22)
u3;o3
#b
u3b <- 60-qt(1-alpha3/2,9)*10/sqrt(22)
o3b <- 60+qt(1-alpha3/2,9)*10/sqrt(22)
u3b;o3b
#nr4
xi <- c(247.4, 249.0, 248.5, 247.5, 250.6, 252.2, 253.4, 248.3, 251.4, 
        246.9, 249.8, 250.6, 252.7, 250.6, 250.6, 252.5, 249.4, 250.6, 247.0, 249.4)
n <- length(xi)  
mean.x <- mean(xi)
sd4 <- sd(xi)
alpha4 <- 0.05
#a
u4a <- mean.x-qnorm(1-alpha4/2)*sd4/sqrt(n)
o4a <- mean.x+qnorm(1-alpha4/2)*sd4/sqrt(n)
u4a;o4a
#b
u4b <- mean.x-qt(1-alpha4/2,n-1)*sd4/sqrt(n)
o4b <- mean.x+qt(1-alpha4/2,n-1)*sd4/sqrt(n)
u4b;o4b
#c
mu <- 250
Qn <- sum((xi - mu)^2)
u4c <- Qn/qchisq(1-alpha4/2,n)
o4c <- Qn/qchisq(alpha4/2,n)
u4c;o4c
#d
u4d <- (n-1)*sd4^2/qchisq(1-alpha4/2,n-1)
o4d <- (n-1)*sd4^2/qchisq(alpha4/2,n-1)
u4d;o4d
#nr5
mean5 <- 300
sd5 <- 60
n5 <- 51
alpha5 <- 0.05
o5 <- mean5 + qt(1-0.05,n-1)*sd5/sqrt(n5)
o5
u5 <- sqrt((n5-1)*sd5^2/qchisq(1-alpha5,n5-1))
u5
#nr6
pdach <- 0.7
n6 <- 250
alpha6 <- 0.05
o6a <- pdach + qnorm(1-alpha6/2)*sqrt(pdach*(1-pdach)/n6)
u6a <- pdach - qnorm(1-alpha6/2)*sqrt(pdach*(1-pdach)/n6)
o6a;u6a

kontrolle6 <- binom.test(x=0.7*250,n=250,conf.level = 1-alpha6)$conf.int

#nr7
n7 <- 100
pdach7 <- 0.4
alpha7 <- 0.05
o7 <- pdach7 + qnorm(1-alpha7)*sqrt(pdach7*(1-pdach7)/n7)
o7
kontrolle7 <- binom.test(0.4*100,100,alternative = "less", conf.level = 1-alpha7)
kontrolle7$conf.int
#nr10
alpha10 <- 0.05
mean10 <- 46.7
u10 <- mean10 - qt(1-alpha10,9)

#nr11
pnorm(2.3)

####
p <- 12/200
12/200 + qnorm(1-0.05)*sqrt(p*(1-p)/200)
alpha <- 1- (2*(1-pnorm(2.810913476)))

binom.test(12/200*200,200,alternative = "less", conf.level = 1-0.05)$conf.int
