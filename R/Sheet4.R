#nr1
x <- c(2,6,3,4,5)
y <- c(3,7,4,7,6)
plot(x,y,main = "scatterplot", xlim=c(0,7), ylim = c(0,8))
cov(x,y)
cor(x,y)
lm(y~x)
abline(lm(y~x),col ="blue")
#nr2
result <- tibble(
exercise = c(10,9,9,11,10,10,6,10,8,12,9,4,12),
hours = c(5,5,4,6,7,5,3,4,5,7,4,2,8))
result
plot(result$exercise,result$hours,main = "exercise per hour")
cov(result$exercise,result$hours)
cor(result$exercise,result$hours)
reg1 <- lm(result$hours~result$exercise)
a <- reg1$coefficients[1]
b <- reg1$coefficients[2]
a;b
predict_score <- a+b*8
predict_score
abline(lm(result$hours~result$exercise),col ="green")
result %>%
  add_row(exercise = 20,hours = 0)
#################################################################################################################################   Contingency table
#nr1
X4 <- c(3,8,7,9,5)
Y4 <- c(6,7,10,8,4)
rank(X4)
rank(Y4)
cor(rank(X4),rank(Y4),method = "spearman")
#nr2
data_mtrx <- matrix(c(40,10,20,10,10,10), ncol = 2, nrow = 3, byrow = T)
data_mtrx

rownames(data_mtrx) = c("Over 70%", "30-70%", "Under 30%")
colnames(data_mtrx) = c("pass", "fail")
data_mtrx

conti_table = as.table(data_mtrx)
conti_table

conti_table <- addmargins(conti_table)
conti_table

chisq_test <- chisq.test(conti_table)
chisq_test

chisq2 <- chisq_test$statistic

exp_val_table <- chisq_test$expected
exp_val_table
C4 <- (chisq2/(chisq2 + 100)) %>% sqrt()
C4

C4_corr <- (chisq2/(chisq2 + 100) * 2) %>% sqrt()
C4_corr
###################################################################################################### 
contingency <- function(x,n){

  sqrt(x/(x+n))
  
}

corrected_contingency <- function(x, cc){
  
  min_x <- min(nrow(x), ncol(x))
  
  sqrt(min_x/(min_x-1)) * cc
  
}

corrected_contingency(data_mtrx, c)
####################################################################################################
#nr3
#a
raw_data <- tibble(
  "class" = c(rep("first",325),
            rep("second", 285),
            rep("third", 706), 
            rep("staff", 885)),
  "state" = c(rep("not rescued",122),rep("rescued", 203),
            rep("not rescued",167),rep("rescued", 118),
            rep("not rescued",528),rep("rescued", 178),
            rep("not rescued",673),rep("rescued", 212))
)
raw_data
nobs <- length ( raw_data$class )
nobs
# contingency table 
cont_tab <-
raw_data %>%
  table() 
#andere schreibweise
table(raw_data)
# display
cont_tab
# add margins
addmargins ( cont_tab )
#b
cond_freq <- cont_tab / rowSums(cont_tab)
#cond_freq[,2]
cond_freq
#c
chisq <- chisq.test(cont_tab)$statistic
chisq
chisq.test(cont_tab)$expected
C <- chisq/(chisq + sum(cont_tab)) %>% sqrt()
C
CC <- (2*chisq/(chisq+sum(cont_tab)))^0.5
CC
############################################################################################################### matrix
# nr3 matrix
titanic_matrx <- matrix(c(122,203,167,118,528,178,673,212), ncol = 2, nrow = 4, byrow = TRUE)
titanic_matrx
rownames(titanic_matrx) = c("first","second","third","staff")
colnames(titanic_matrx) = c("not resc", "resc")
titanic_matrx
cont_table = as.table(titanic_matrx)
cont_table

cont_table <- addmargins(cont_table)
cont_table

chisq_test <- chisq.test(cont_table)
chisq_titanic <- chisq_test$statistic
chisq_titanic

C_matrx <- chisq_titanic/(chisq_titanic + sum(cont_table)) %>% sqrt()
C_matrx
CC_matrx <- (2*chisq/(chisq+sum(cont_tab)))^0.5
CC_matrx
cond_freq_tit <- cont_table / rowSums(cont_table)
#cond_freq[,2]
cond_freq_tit

exp_val_table <- chisq_test$expected
exp_val_table

################### das ist nur extra
contingency <- function(x,n){
  
  sqrt(x/(x+n))
  
}

c <- contingency(chisq, 100) 

corrected_contingency <- function(x, cc){
  
  min_x <- min(nrow(x), ncol(x))
  
  sqrt(min_x/(min_x-1)) * cc
  
}

corrected_contingency(data_mtrx, c)
#######################

chisq.test(cont_tab)$expected
C <- chisq/(chisq + sum(cont_tab)) %>% sqrt()
C
CC <- (2*chisq/(chisq+sum(cont_tab)))^0.5
CC
# Task 5

# a)
raw_data <- tibble(class = c(rep("first", 325),
                             rep("second", 285), 
                             rep("third", 706),
                             rep("staff", 885)),
                   state = c(rep("not_rescued", 122), rep("rescued", 203),
                             rep("not_rescued", 167), rep("rescued", 118),
                             rep("not_rescued", 528), rep("rescued", 178),
                             rep("not_rescued", 673), rep("rescued", 212)))
raw_data

###############################
raw_data2 <- tibble(class = c("first", "second", "third", "staff"),
                    not_rescued = c(122,167,673,528),
                    rescued = c(203,118,212,178))


###############################

no_obs <- length(raw_data$class)
no_obs

cont_table <- table(raw_data)
#cont_table <- addmargins(cont_table)
cont_table

# b)
# Die Tabelle wird punkweise mit den rowsSums verechnet
cond_freq <- cont_table / rowSums(cont_table)
cond_freq[,2]

# c)
chisq_test <- chisq.test(cont_table)
chisq <- chisq_test$statistic

exp_val_table <- chisq_test$expected
exp_val_table

contingency <- function(x,n){
  
  sqrt(x/(x+n))
  
}

corrected_contingency <- function(x, cc){
  
  min_x <- min(nrow(x), ncol(x))
  
  sqrt(min_x/(min_x-1)) * cc
  
}

c <- contingency(chisq, no_obs)
c

cc <- corrected_contingency(cont_table, c)
cc

# d)

cont_tab_new <- raw_data %>%
  mutate(class = if_else(class=="first" | class=="second", "first+second", "third+staff")) %>% 
  table()
cont_tab_new

chisq_test_new <- chisq.test(cont_tab_new)
chisq_new <- chisq_test_new$statistic
chisq_new

exp_val_table_new <- chisq_test_new$expected
exp_val_table_new

contingency <- function(x,n){
  
  sqrt(x/(x+n))
  
}

corrected_contingency <- function(x, cc){
  
  min_x <- min(nrow(x), ncol(x))
  
  sqrt(min_x/(min_x-1)) * cc
  
}

c <- contingency(chisq_new, no_obs)
c

cc <- corrected_contingency(cont_tab_new, c)
cc
