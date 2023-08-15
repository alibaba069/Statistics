1-pnorm(2.122)
# nr 4
# parameter of interest: success rate p

# Suppose in a clinical trial, 200 people received the new
# vaccine. Of these, 172 became immune to the infection. Based on
# this, can we say that the new vaccine is indeed more effective
# than the current one? 
# What is the corresponding Null-Hypothesis?
# Null Hypothesis H0: p<=p0   against H1: p>p0
p0 <- 0.8
# Test at a 5% significance level and state your conclusion in the
# context of the problem. 
n <- 200
p <- 172/n
alpha <- 0.05
# normal approximation
test_statistic <- (p-p0)/sqrt(p0*(1-p0)/n)
# reject if test_statistic > qnorm(1-alpha)
test_statistic > qnorm(1-alpha) # 2.12132 > 1.644854
# reject H0, thus the new vaccine seems to be better than the old one
p_value_app <- 1-pnorm(test_statistic) # 0.01694743

#################### exact test
binom.test(172,p=0.8,n,alternative = "greater", conf.level = 1-alpha)
####################
p_value <- 1-pbinom(n*p-1,n,p0)
p_value # = 0.01792922 < 0.05, i.e. rejection

# In making the above conclusion, which type of error are you
# risking, type I or type II?
# Since we reject H0, we are risking making type I error.

# What is the probability of a type II error if the true success
# rate is 85%? 
p1 <- 0.82
# beta.approx = P(test_statistic <= qnorm(1-alpa)) <=>
# beta.approx = P(p <= qnorm(1-alpa)*(p0(1-p0)/n)^0.5+p0)
# p ~ N(p1,p1(1-p1)/n) approx.
#d 
pbinom(qbinom(1 - alpha, size = n, prob = p0), size = n, prob = 0.82)

# nr 5
i <- 0:30
sum(choose(30,i) * (1/2)^i * (1/2)^30-i)

sum(dbinom(i,30,1/2))
# nr 6
t.test_a <- (248 - 250)/5 * sqrt(81)
qt(1- alpha,81 - 1)
p_value_a <- pt(t.test_a,81-1)
t.test_a < p_value_a
#H0 is rejected
#b
tstat_b <- (80-1)*5^2/7^2
tstat_b # 40.81633
qchisq(alpha ,80-1) # 60.39148 
pvalueb <- pchisq(tstat_b, df=80-1) 
pvalueb # 8.081861e=05
#H0 is rejected
#nr 7
t_stat <- (10.1-10) /0.3*sqrt(100)
alpha <- 0.1
qnorm(1-alpha/2)
qnorm(alpha/2)
#b
pnorm(3.33)
