#nr 1
raw_data <- read_csv("testing_covid.csv")
view(raw_data)
#nr 2
head(raw_data)
names(raw_data)
# country: qualitative, nominal, diskrete
# country_code: qualitative, nominal, diskrete
# year_week: quantitative , interval, diskrete
# level: qualitative, nominal, diskrete
# region_name: qualitative, nominal, diskrete
# new_cases: quantitative, absolute, diskrete
# test_done:quantitative, absolute, diskrete
# population: quantitative,absolute, diskrete
# 
1 - pnorm((0.08-0.12)/sqrt((0.12*0.88)/150))
m <- 12
n <- 200
x <- m/n
alpha <- 0.05
u <- x+qnorm(1-alpha)*sqrt((x*(1-x))/200)
u
binom.test(x=12, n= 200, p = 12/200,alternative = "less", conf.level = 1-alpha)$conf.int


(qnorm(1-alpha)/0.01)^2 * x*(1-x)
ceiling((qnorm(1-alpha)/0.01)**2*x*(1-x))
1-pnorm(0.1/sqrt(0.1*0.9/200))
n<-  (1.96 / 0.01)^2 * 0.1 * (1 - 0.1)

((2*qnorm(1-0.05/2)*0.05)/0.05)^2
