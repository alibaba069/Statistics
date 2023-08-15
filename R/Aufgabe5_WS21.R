#nr 5
#a
x <- c(98.32,97.26,99.85,99.52,95.73,96.56,100.49,98.19,95.16,98.26,96.46,100.23,99.76,98.58,97.43)
alpha <- 0.05
nx <- length(x)
#b
# t-test bc since normal model, test about mu, sd is unknown
#c
test.stat_c <- t.test(x,alternative="two.sided",mu=100,paired=FALSE,var.equal=TRUE,
       conf.level=1-alpha)
test.stat_c$p.value < alpha
#reject => H0 < alpha 

#d
#H0: mu1 >= mu2, H1: mu1 < mu2
#e
#t-test for mu , sigma unknown
y <- c(100.14,100.05,96.51,98.7,98.22,101.06,103.55,100.16,100.6,102.85,103.15,100.66,102.52,102.09,100.84)
ny <- length(y)
test.stat_e <- t.test(x,y,alternative = "less", paired = F, var.equal = T, conf.level = 1-alpha)
test.stat_e$p.value < alpha
# reject H0 
#f
#f-test , sd1^2 = sd2^2
test.stat_f <- var.test(x,y,alternative = "two.sided", conf.level = 1-0.1)
test.stat_f$p.value < 0.1
# no rejection of H0 since p_value > alpha

##
12/200 + qnorm(0.95)*sqrt((12/200 * 88/200)/200)
binom.test(x=12,n=200,p=12/200, alternative = "less", conf.level = 0.95)$conf.int
(qnorm(0.95)/0.01)^2 *0.1*0.9

(2*qnorm(1-0.1/2)*5/1.25)^2
1-(2*(1-pnorm(0.65*sqrt(200)/5)))
2*qnorm(1-0.2/2)*5/sqrt(150)

##
