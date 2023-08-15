#nr 2
#a)
density.X1.X2 <-
permutations(n = 6,r = 2, v = 1:6 , repeats.allowed = TRUE) %>% as_tibble() %>%
  mutate ( prob = 1/36) 
density.X1.X2
#b)
density.Y<- density.X1.X2%>%
  mutate(Y = V1+V2) %>%
  group_by(Y) %>%
  mutate(prob.Y = sum(prob)) %>% select(Y, prob.Y) %>%
  unique ()
density.Y
#c)
density.U<- density.X1.X2%>%
  rowwise() %>%
  mutate(U = min(V1,V2)) %>% group_by(U) %>%
  mutate(prob.U = sum(prob)) %>% select(U, prob.U) %>%
  unique ()
density.U
#d)
density.V<- density.X1.X2%>%
  rowwise() %>%
  mutate(V = max(V1,V2)) %>% group_by(V) %>%
  mutate(prob.V = sum(prob)) %>% select(V, prob.V) %>%
  unique ()
density.V
#e)
density.UV<- density.X1.X2%>%
  rowwise() %>%
  mutate(UV = paste(min(V1,V2),max(V1,V2))) %>% group_by(UV) %>%
  mutate(prob.UV = sum(prob)) %>%
  select(UV, prob.UV) %>%
  unique ()
density.UV
#nr3
# with replacement
#a)
# X~B(n=20,p=0.3)
#b)
k<- 0:20
plot(k,dbinom(k,20,0.3),type = "h", main = "B(n= 20, p= 0.3)", xlab = "x" , ylab = "density")
#?plot()
?pbinom()
#c)
samp_20 <- rbinom(n=20,size = 20,prob = 0.3)
samp_20
#d)
diff_binomial <-(pbinom(14,20,0.3) -pbinom(5, 20, 0.3))
diff_binomial
#e)
quntil_binom <- qbinom(c(0.25 ,0.5 ,0.75) , size = 20, prob = 0.3)
quntil_binom 

#without replacement
#a)
# X  ̃ H(n=20,M=30,N=100)
# b)
plot(0:20,dhyper(0:20,m=30,n=70,k=20), type = "h", main ="H(n=20,M=30,N=100", xlab="x ", ylab="density ") 

#c)
samp_20_out <- rhyper (20 ,m=30,n=70,k=20)
samp_20_out

#d)  P(5 < X < 20)
     sum(dhyper(6:14 ,m=30,n=70,k=20)) #oder
     
diff_hyper <- phyper (14 ,m=30,n=70,k=20) - phyper (5 ,m=30,n=70,k=20) 
diff_hyper
#e)  quantile
quntil_hyper <- qhyper(c(0.25 ,0.5 ,0.75) ,m=30,n=70,k=20)
quntil_hyper

#nr4
R <- tibble(k=0:3)
R <-
R %>% mutate(prob=choose(3,k)*(1/6)**k*(5/6)**(3-k)) %>% mutate(r = if_else (k==0,-1,-1+1+k))

exp_r <- sum(R[,2]*R[,3])
exp_r
#nr5
fp <- 1
sp <- 4
riv <- 15
n <- 5
#a)
#dhyper(x = anzahl des jeweiligen Ereignisses, m = anzahl des gesamtpakets woraus es gezogen wird, n = anzahl des rests , k = Wie viele male gezogen wird)
pa <- dhyper(2,riv ,fp+sp,n)
pa
#b)
#choose(15,2) => 15 über 2 
pb <- choose(riv ,2)*choose(sp,2)*choose(fp ,1) /
  choose( riv+fp+sp ,n) 
pb
#c)
pc <- (15/20 * 14/19 * 13/18 * 12/17) * 5/16
pc
#oder 
pc_alt <- ((choose(15,4))/choose(20,4)) * 5/16
pc_alt

# with replacement
#dbinom(x =  wie viele werden rausgezogen, size= wie viel insgesamt von der Art x, prob = Wahrscheinlichkeit, log = FALSE)
#a)
ba <- dbinom(2,5,15/20)
ba
#b)
#?
#c)
bc <- power((1-riv/riv+fp+sp),4)/n/riv+fp+sp
bc
#nr6
dis <-
tibble( n=seq(1,4000), p=1-phyper(2,500,3500,n))
dis
min(which(dis$p >=0.99))
#b)
dis %>%
  filter(n <= 75) %>%
  ggplot(mapping=aes(x=n,y=p))+
  geom_point()+
  geom_abline(slope = 0, intercept = 0.99)
#normal distribution
#nr1
#a)
x <- seq(-10, 10, by = .1)
y <- dnorm(x, mean = 2.5, sd = 1.5)
plot(x,y, type = "l")
#b)
x2 <- seq(-10,10,by= 0.1)
y2 <- pnorm(x2,mean=2.5,sd=1.5)
plot(x2,y2, type = "l")
#c)
x3 <- seq(-10,10,by= 0.1)
y3 <- pnorm(x2,mean=2.5,sd=1.5)
plot(x3,y3, type = "l")
#d)
x4 <- rnorm(100,mean= 2.5, sd = 1.5)
hist(x4, main = "Normal Distribution")
#nr2
#a)
score_2a <- 1-pnorm(34,35,10)
score_2a
#b)
score_2b <- pnorm(42,35,10)
score_2b
#c)
score_2c <- pnorm(34,35,10) - pnorm(28,35,10)
score_2c
#nr 3
u <- qnorm(0.175,70,12)
o <- qnorm(0.825,70,12)
u
o
#nr 5
qnorm(0.1,)
###################################

# hier stehen die aufgaben zu norm disb.

####################################
#Central Limit Theorem
#nr1
#a)
n <- 1000
pa <- 0.01
pb <- 0.01
pc <- 0.05
pcp <- 0.02
# prob. of no error
p_no_error <- (1-pa)*(1-pb)*(1-pc)*(1-pcp) # expected value
exp_n_def <- n*(1- p_no_error) # variance
var_n_def <- n*(1-p_no_error)*p_no_error 
p_no_error;exp_n_def;var_n_def
#b)
guarantee_b <- pbinom(110 ,n,1-p_no_error )
guarantee_b
guarantee_b_norm <- pnorm(110,mean = exp_n_def,sd = sqrt(var_n_def))
guarantee_b_norm

#c)
p_no_error_new <- (1-pa)*(1-pb)*(1-0.01)*(1 -pcp) 
100*(1-p_no_error)- 100*(1-p_no_error_new)

# Nr2
#a)
pbinom(301,324,0.9)

pnorm(324,270,27)
#b)
qnorm(0.99,mean = 0,sd = 1)
