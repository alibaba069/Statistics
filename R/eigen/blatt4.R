#nr1
cafe <- tibble(
  Cafe = seq(1:5),
  X = c(3,8,7,9,5),
  Y = c(6,7,10,8,4)
)
cafe <- cafe %>%
  mutate(
    R_X = rank(X),
    R_Y = rank(Y)
  )
cafe

Sperman <- cor(rank(cafe$X),rank(cafe$Y),method = "spearman")
Sperman

cafe <- cafe %>% 
  mutate(
    RR_X = rank(-X),
    RR_Y = rank(-Y)
  )
cafe
Sperman2 <- cor(cafe$RR_X,cafe$RR_Y, method ="spearman")
Sperman2
#nr 3
Titanic_matrix <- matrix(c(122,203,167,118,528,178,673,212), ncol = 2, nrow = 4, byrow = TRUE)
Titanic_matrix
rownames(Titanic_matrix) = c("first","second","third","staff")
colnames(Titanic_matrix) = c("not resc", "resc")
Titanic_matrix

cont_table <- as.table(Titanic_matrix)
cont_table

cont_table <- addmargins(cont_table)
cont_table
#b
#fragen falkenberg warum nur die Hälfte angegeben werden
cond_freq <- cont_table / rowSums(cont_table)
cond_freq * 2 #[,2]
#c
chisq_test <- chisq.test(cont_table)
#indep_table
indp_tab <-  chisq_test$expected
indp_tab

#x^2

chisq2 <- chisq_test$statistic
chisq2

#C

C_titanic <- sqrt(chisq2/(chisq2+2201))
C_titanic

#c_corr

C_coor_titanic <- (2 * chisq2/(chisq2+2201))**0.5
C_coor_titanic

#d

