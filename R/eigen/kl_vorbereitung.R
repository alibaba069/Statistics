#Sheet 2 
#probieren

x <- seq(5,13,by=2)
summary(rainfall)
table <- tibble(name= c("Ali","b","c"),
                nachname = c("Ishaq","d","e"),
                alter= c(20,2,3))
table
y <- c(seq(1:20))
matrix <- matrix(data=y, nrow=5,ncol=4,byrow=TRUE,dimnames=NULL)
matrix
################0      
#nr 1
student1 <- tibble(
  student = c("Adam","Bernd","Christian","Doris"),
  algebra = c(NA,5,3,4),
  analysis = c(2,NA,1,3),
  disma = c(3,NA,2,4)
)
student1

student2 <- tibble(
  name = rep(c("Adam","Bernd","Christian","Doris"),each=2),
  type = rep(c("heigth","weight"),4),
  measure = c(1.83,81,1.75,71,1.69,55,1.57,62)
)
student2

student3 <- tibble(
  name = c("Adam","Bernd","Christian","Doris"),
  ratio = c("81/1.83","71/1.75","55/1.69","62/1.57")
)
student3
#c
tidy_student1 <- student1 %>%
  gather("algebra","analysis","disma", key = exam, "value" = "grade")
tidy_student1

tidy_student2 <-  student2 %>%
  spread(key = type, value = measure)
tidy_student2

tidy_student3 <- student3 %>%
  separate(col = ratio, into = c("weight","height"), sep = "/")
tidy_student3
#nr 2
nr2 <- (5+3)%>% 
  sqrt() %>%
  log() %>%
  sin()
nr2
#nr 3
v <- c(seq(from = 0.5, to= 5, by= 0.5))
v

vec <- v^2 %>% 
   log() %>%
  sum() %>%
  round(2)
vec

#nr4
df <-  tibble(
  id = c(1:10),
  sex = sample(x =c("f","m"), size = 10, replace = TRUE),
  age = sample(x = c(20:35),size = 10, replace = TRUE),
  score1 = sample(x=c(0:25), size = 10,replace = TRUE)
)
df
#a
df %>% filter(sex == "m")
#b
df <- df %>% 
  add_row(id=11,sex="m",age=25,score1=4)
df
#c
df <- df %>%
  mutate(score2 = sample(x=c(0:25),size = 11,replace = TRUE),
         score3 = sample(x=c(0:25),size = 11,replace = TRUE)
         )
df
#d

df <- df %>%
  mutate(sum = score1+score2+score3)
df
#e
df <- df %>%
  mutate(grade = case_when(
    sum_of_all <= 37 ~ 5,
    sum_of_all >37 & sum_of_all <=45 ~4,
    45 < sum_of_all &sum_of_all <= 55 ~ 3,
    55 < sum_of_all &sum_of_all<= 65 ~ 2,
    sum_of_all >= 65 ~1
  ))
df
#f
df %>% 
  arrange(sex) %>% 
  select(id,sex,grade) %>%
  filter(grade < 4)
df
#g
df %>% 
  group_by(sex) %>%
  summarize(mean = mean(sum_of_all),
            median = median(sum_of_all),
            min = min(sum_of_all),
            max = max(sum_of_all)) 
df

#nr4
library(tidyverse)
library(nycflights13) 
?flights()
flights
#b
flights %>% filter(arr_delay > 120)
#c
flights %>% filter(arr_delay >120 & dep_delay ==0)
#d
flights %>%
  filter(carrier %in% c("AA","DL","UA")) %>%
  filter(arr_delay <= 0)
flights
#e
flights %>% filter(carrier %in% c("AA","DL","UA")) %>%
  filter(month == 5 ) %>%
  filter(arr_delay >300) %>% 
  select(carrier,flight) %>%
  arrange(carrier,flight) %>%
  unique()
#f
flights %>% 
  mutate(dep_time = (dep_time %/% 100)*60 + dep_time %% 100) %>%
  mutate(arr_time = (arr_time %/% 100)*60 + arr_time %% 100)
#h
 flights %>% 
   mutate(speed = distance / air_time *60) %>% 
   select(carrier,flight,speed) %>% 
   arrange(desc(speed))%>% 
   top_n(10,speed)
 ################################################################################
#sheet 3
res13 <- c(0.268 ,0.205 ,0.126 ,0.107 ,0.092 , 0.089 ,0.062 ,0.05)
res17 <- c(0.341 ,0.257 ,0.047 ,0.048 ,0.086 , 0.084 ,0.074 ,0.062)
difference <- res17 - res13
party <- c("CDU","SPD","AFD","FDP","Die Linke","Gruene","CSU","Others")

#pie diagram for 13
?paste
?barplot
pie(x= res17, paste(lables=party,res17), main = "Result 2017")
barplot (res17 , names.arg=party ,
         ylim=c (0 ,0.7) , xlab="Parties" , ylab="2017 Votes (%)",main = "Result 2017")
hist(res17,xlab = "x", main = "Histogram")
#nr2
cfd <- c(568,577,581,640,645,657,673,696,703,720,728,729,777,808,824,825,865,875,1007)
H <- ecdf(cfd)
H
plot(H)
#b
H(800)
1-H(725)
H(777) - H(642)
#nr10
distance <- c(12.5,29.9,14.8,18.7,7.6,16.2,16.5,27.4,12.1,17.5)
altitude <- c(342,1245,502,555,398,670,796,912,238,466)
mean(distance)
mean(altitude)
median(distance)
median(altitude)
#b
quantile(distance,c(0.))
########################## zugriff auf die verschiedene tabelle
tidy_student1 <- student1 %>%
  gather("algebra","analysis","disma",key = "exam", value = "grade")
tidy_student1

tidy_student2 <- student2 %>%
  spread(key = type, value = measure)
tidy_student2
tidy_student3 <- student3 %>%
  separate(col = ratio, into = c("weight","height"), sep = "/")
#
df <- tibble( studentID = c(seq(1:10)),
              sex = sample(x=c("m","f"),size = 10,replace = TRUE),
              age= sample(x=c(20:35), size = 10,replace = TRUE),
              score1 = sample(x=(0:25),size = 10, replace = TRUE)
)
df
df %>% filter(sex =="m")
df <- df %>%
  add_row(studentID=11,sex="m", age = 25, score1= 4)
df
#
df <- df %>%
  mutate(score2= sample(x= c(0:25),size = 11,replace = TRUE),
         score3 = sample(x=c(0:25),size = 11,replace = TRUE))
df
#
df <- df %>%
  mutate(sum = score1+score2+score3)  
df
mutate(sum = score1+score2+score3)
#
df <- df %>%
  mutate(grade = case_when(
    sum <= 37 ~ 5,
    sum > 37 & sum <= 45 ~ 4,
    sum > 45 & sum <= 55 ~ 3,
    sum > 55 & sum <= 65 ~ 2,
    sum >= 65 ~1
  ))
df
#
df %>%
  select(studentID,sex,grade) %>%
  arrange(sex) %>%
  filter(grade < 4)
#
df %>% 
  group_by(sex) %>%
  summarise(mean_score = mean(sum),
            max_score = max(sum),
            min_score = min(sum),
            med_score = median(sum)
            )
#####
flights
names(flights)
#
flights %>%
  filter(arr_delay > 120)
flights %>%
  filter(arr_delay >120 & dep_delay == 0)
#
flights %>%
  filter(carrier %in% c("AA","DL","UA")) %>%
  filter(month == 5) %>%
  filter(arr_delay >300) %>%
  select(carrier,flight) %>%
  arrange(carrier,flight) %>%
  unique()
#
################ s4
conti_table_matrix <-matrix(data = c(40,10,20,10,10,10),nrow = 3, ncol = 2, byrow = TRUE)
conti_table_matrix

rownames(conti_table_matrix) = c("over 70%", "30% - 70%", "under 30%")
colnames(conti_table_matrix) = c("pass", "fail")

conti_table <- as.table(conti_table_matrix)
conti_table <- addmargins(conti_table)

chisq_test <- chisq.test(conti_table)
exp_tab <- chisq_test$expected
chisq <- chisq_test$statistic
Ca <- (chisq/(chisq+100)) %>% sqrt()
Ca
ca.corr <- (chisq/(chisq+100)*2) %>% sqrt()
ca.corr
###############b s6
dbinom(5,10,1/6)
#c
rbinom(20,20,0.3)
#d
pbinom(14,20,0.3)-pbinom(5,20,0.3)
#e
qbinom(c(0.25,0.5,0.75),20,0.3)

#nr4
#E(X) = 
dbinom(0,3,1/6)*-1 + dbinom(1,3,1/6)*1 + dbinom(2,3,1/6)*2 + dbinom(3,3,1/6)*3
#nr5
dbinom(2,5,15/20)

# 
0.2/0.3
1- pbinom(15,60,1/4)
#
p=0.00159
1- pbinom(0,10,p)
# normal distribzution
dnorm(2,2.5,1.5)
p <- pnorm(c(-10:10),2.5,1.5)
plot(p)
#central limit theorem
sigma <- sqrt(79.86)
pnorm(110.5,87.53,sigma)
pbinom(110,1000,0.08753)

#n2
1 - pnorm(300.5,291.6,5.4)
1- pbinom(300,324,0.9)
#######################
# s10
xi <- c(5.46,5.34,4.34,4.82,4.40,5.12,5.69,5.53,4.77,5.82)
yi <- c(5.45 ,5.31 ,4.11 ,4.69 ,4.18 ,5.05 ,5.72 ,5.54 ,4.62, 5.89, 5.60 ,5.19, 3.31 ,4.43 ,5.30, 4.09)

sdx <- 0.25; sdy <- 0.36
nx <- length(xi); ny <- length(yi)
gaus.test <- (mean(xi)-mean(yi))/sqrt(sdx^2/nx + sdy^2/ny)
pvalue <- pnorm(gaus.test,0,1)

# one sided: reject H0 mu.x >= mu.y,if test.stat < quantile
alpha <- 0.05
quantile <- qnorm(1 - alpha,0,1)
quantile

# nr 1
sdx <- 0.5; sdy <- 0.6
x <- c(5.46,5.34,4.34,4.82,4.40,5.12,5.69,5.53,4.77,5.82)
y <- c(5.45,5.31,4.11,4.69,4.18,5.05,5.72,5.54,4.62,5.89,5.60,
       5.19,3.31,4.43,5.30,4.09)
nx <- length(x); ny <- length(y)
# h0: mu1 >= mu2
gaus.test <- (mean(x)-mean(y))/sqrt(sdx^2/nx + sdy^2/ny)
gauss_quantile <- pnorm(gaus.test,0,1)
quantile <- qnorm(0.05)
gauss_quantile ; quantile
# nr 2





# how to read a csv file
read_csv("Path where your CSV file is located on your computer\\File Name.csv")

bound <- c(0, 18.5, 24.5, max(res$Body_Mass_Index))
res
hist(res$Body_Mass_Index, breaks = bound)
1-pnorm(130,115,10)
15**2
qnorm(0.975)
1-pnorm(550,492,30)
150*qnorm(0.99)+12299.5
leck_mich <- tibble(
  n= 300:500,
  prob = pnorm(50000,mean = n*123, sd = sqrt(n*15**2))
)
399

0.5-qnorm(0.95)*sqrt((1/4)/10)

## knitr::stitch('dateiname.R')

qnorm(0.9,66.8,sqrt(62.37))
pnorm(9800,10045,105)
(10045+105*qnorm(0.99))



#### pnorm(220.5,240*0.89,sqrt(240*0.89*0.11))

nr4 <- tibble(
  n = 300:350,
  prob= pnorm(300,mean = n*0.9, sd= sqrt(n*0.9*0.1))
)
nr4

#
c <- c(8, 9, 10, 13, 14, 16, 17, 20, 21)
mean(c)
sigma <- 2.8
qnorm(1-0.05/2)
xu <- 14.22-qnorm(1-0.05/2)*2.8/sqrt(9)
xo <- 14.22+qnorm(1-0.05/2)*2.8/sqrt(9)

qnorm(1-0.01/2)
xu2 <- 14.22-qnorm(1-0.01/2)*2.8/sqrt(9)
xo2 <- 14.22+qnorm(1-0.01/2)*2.8/sqrt(9)

1-0.058
1-0.8362
1-qnorm(0.99)*4.85+213.5
pnorm(225,213.6,4.85)

