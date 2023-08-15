#nr 1
sdx <- 0.5; sdy <- 0.6
x <- c(5.46,5.34,4.34,4.82,4.40,5.12,5.69,5.53,4.77,5.82)
y <- c(5.45,5.31,4.11,4.69,4.18,5.05,5.72,5.54,4.62,5.89,5.60,
       5.19,3.31,4.43,5.30,4.09)
nx <- length(x); ny <- length(y)
gaus.test <- (mean(y)-mean(x))/sqrt(sdx^2/nx + sdy^2/ny)
alpha <- 0.05

# one sided: reject H0 mu.x >= mu.y,if test.stat < quantile
quantile <- qnorm(alpha,0,1)
pvalue <- pnorm(gaus.test,0,1)
gaus.test;quantile;pvalue # -1.027782; -1.644854; 0.1520261
gaus.test < quantile
#nr 2 
alpha <- 0.05
xb <- c(7.06,11.84,9.28,7.92,13.5,3.98,3.82,7.34,8.7,9.24,4.86,3.32,
       12.78,12,5.24,11.4,6.56,9.04,7.72,9.26,7.88,8.6,9.3,8.42,8.54)
yb <- c(8.68,6,6.3,10.24,10.88,5.36,7.82,4.7,9.02,9.78,6.9,
       5.8,13.56,10.32,13.3,11.38,7.94,10.74,13.68,14.92,7.42,10.36,
       10.54,5.22,13.74,12.98,10.34,10.02,17.8,13.04,5.2,9.4,11.18,
       12.68,12.36)
nx <- length(xb); ny <- length(yb)

##########################################################################
# case: equal variances
alpha <- 0.05
stat.test <- t.test(xb,yb,alternative="less",mu=0,paired=FALSE,var.equal=TRUE,
                    conf.level=1-alpha)
stat.test
# reject H0, since p-value = 0.0181

##########################################################################
# case: not equal variances
alpha <- 0.05
stat.testb <- t.test(xb,yb,alternative="less",mu=0,paired=FALSE,var.equal=FALSE,
       conf.level=1-alpha)
stat.testb
# reject H0, since p-value = 0.01596

?t.test()
# reject H0, since p-value = 0.0181

# nr 3
water <- c(16,15,11,20,19,14,13,15,14,16)
alcohol <- c(13,13,10,18,17,11,10,15,11,16)
n_water <- length(water)
n_alcohol <- length(alcohol)

alk_test <- t.test(water,alcohol,alternative = "two.sided",mu =0, paired = TRUE, 
                   var.equal = FALSE,conf.level = 1-alpha)
alk_test
#reject H0
# nr 4
A <- c(102.4 ,101.3 ,97.6 ,98.2 ,102.3 ,99.1 ,97.8 ,103.9 ,101.6 ,100.1)
B <- c(98.4 ,101.7 ,100.5 ,99.3 ,100.6 ,99.6 ,102.2 ,101.1 ,99.9 ,101.0)
var.test(A, B, alternative = "greater", conf.level=0.95)
# nr 5
group_1 <- c(7.2 ,4.1 ,5.5 ,4.5 ,5.7 ,3.8 ,4.6 ,6.0 ,5.2 ,5.4)
group_2 <- c(5.3 ,4.4 ,5.0 ,3.5 ,3.9 ,4.9 ,5.6 ,2.5 ,4.0 ,3.6)

var.test(group_1,group_2,alternative = "two.sided", conf.level = 1-0.01/2)
