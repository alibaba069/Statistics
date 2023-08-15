#nr1
# barplot()
#pie()
library(tidyverse)

result2017 <- c(0.268 ,0.205 ,0.126 ,0.107 ,0.092 , 0.089 ,0.062 ,0.05)
result2013 <- c(0.341 ,0.257 ,0.047 ,0.048 ,0.086 , 0.084 ,0.074 ,0.062)
difference <- result2017-result2013
party <- c("CDU","SPD","AFD","FDP","Die Linke","Gruene","CSU","Others")

nat_el <- tibble(
  res.2017 = c(0.268 ,0.205 ,0.126 ,0.107 ,0.092 , 0.089 ,0.062 ,0.05),
  res.2013 = c(0.341 ,0.257 ,0.047 ,0.048 ,0.086 , 0.084 ,0.074 ,0.062),
  party = c("CDU","SPD","AFD","FDP","Die Linke","Gruene","CSU","Others"))

    nat_el %>% 
    mutate(
      diff = res.2017 - res.2013
    )
nat_el
#nat_el <- 
#  nat_el %>%
#  mutate(
#    diff = res.2017 - res.2013
#  )
#nat_el
par(mar= c(2,2,0.5,0.5),mfrow=c(3,1),cex = 0.45)
pie(x= result2017, paste(lables=party,result2017), main = "Result 2017")

barplot (result2017 , names.arg=party ,
          ylim=c (0 ,0.7) , xlab="Parties" , ylab="2017 Votes (%)")
barplot(difference , names.arg=party ,
        ylim=c ( -0.1 ,0.1) , xlab="Parties", ylab="Difference to 2013")
#descriptive Statistics
#nr1
#same mean but diffrent sd
xa1 <- c(1,2,3,4,5)
yb1 <- c(0,0,5,5,5)
mean(xa1)
mean(yb1)
sd(xa1)
sd(yb1)
# same mean but different median
xa2 <- c(1,3,5,7,9)
yb2 <- c(1,3,6,7,8)
mean(xa2)
mean(yb2)
median(xa2)
median(yb2)
# same median but different mean
xa3 <- c(1,3,5,7,9)
yb3 <- c(1,3,5,7,14)
mean(xa3)
mean(yb3)
median(xa3)
median(yb3)
#nr2
r <- c(0.13,0.22,0.12,-0.05,-0.13)
r <- r+rep(1,5)
R <- 1000*cumprod(r)
r
R
geo.mean <- prod(r)**0.2
1000*geo.mean**20
#nr5
obs <- c(4,3,2,4,10)
mean(obs)
median(obs)
mode(obs)
quantile(obs,probs = 0.2)
mean(obs,trim = 0.2)
#nr6
freq_table <- tibble(number=1:8, Abs_freq=c(5,4,1,7,2,3,1,2))
freq_table
mean(freq_table)
x <- rep( freq_table$number , freq_table$Abs_freq )
#arethmetic
mean(x)
#geometric
geom_mean <- prod(x)^(1/length(x))
mean(geom_mean)
#harmonic
length (x)/sum(1/x)
#trimmed mean
mean(x, trim = 0.1)
 #nr8
f <- c(3,7,2,5,6,10,6,3,6,5)

quantile(sort(f),c(0.25,0.5,0.75))
quantile(f, probs = seq(0,1,0.25))
mean(f, trim = 0.05)
#nr10
mychess <- tibble(nonplayer = c(22.1,22.3,26.2,29.6,31.7,33.5,38.9,39.7,43.2,43.2),
                  Beginners= c(32.5,37.1,39.1,40.5,45.5,51.3,52.6,55.7,55.9,57.7),
                  TournementPlayers = c(40.1,45.6,51.2,56.4,58.1,71.1,74.9,75.9,80.3,85.3))
mychess
# nonplayer
mean(mychess$nonplayer)
median(mychess$nonplayer)
min(mychess$nonplayer)
max(mychess$nonplayer)
quantile(mychess$nonplayer,probs = seq(0,1,0.25))
Q3 <- quantile(mychess$nonplayer , probs = 0.75)
Q1 <- quantile(mychess$nonplayer ,probs = 0.25)
IQ <- Q3-Q1
IQ
#beginners
mean(mychess$Beginners)
median(mychess$Beginners)
min(mychess$Beginners)
max(mychess$Beginners)
quantile(mychess$Beginners,probs = seq(0,1,0.25))
Q3b <- quantile(mychess$Beginners , probs = 0.75)
Q1b <- quantile(mychess$Beginners ,probs = 0.25)
IQb <- Q3b-Q1b
IQb
#Tournament
mean(mychess$TournementPlayers)
median(mychess$TournementPlayers)
min(mychess$TournementPlayers)
max(mychess$TournementPlayers)
quantile(mychess$TournementPlayers ,probs = seq(0,1,0.25))
Q3t <- quantile(mychess$TournementPlayers , probs = 0.75)
Q1t <- quantile(mychess$TournementPlayers ,probs = 0.25)
IQt <- Q3t-Q1t
IQt

#boxplot
boxplot(mychess$nonplayer,mychess$Beginners,mychess$TournementPlayers,
        main = "Values per group",
        xlab = "Groups",
        ylab = "Values",
        names = c("non","beg","tournament"))


#falkenberg version
library(tidyverse)
data <- tibble(
  type = c(rep("non-player",10), rep("beginner",10),rep("tournament",10)),
  res = c(22.1,22.3,26.2,29.6,31.7,33.5,38.9,39.7,43.2,43.2,
          32.5,37.1,39.1,40.5,45.5,51.3,52.6,55.7,55.9,57.7,
          40.1,45.6,51.2,56.4,58.1,71.1,74.9,75.9,80.3,85.3))
measures <- data %>%
  group_by(type) %>%
  summarise(Min = min(res),Mx=max(res),
            q1=quantile(res,0.25,type=1),q2=quantile(res,0.5,type=1),
            q3=quantile(res,0.75,type=1),
            Mean=mean(res),variance=var(res),
            interquartile_range=q3-q1)

measures

# Boxplots
# changing the order in the side by side boxplots by adding a factor to type
data$type <- factor(data$type, levels = c("non-player", "beginner","tournament"))
ggplot(data = data) + 
  geom_boxplot(mapping = aes(x=type, y=res, group = type)) +
  geom_point(mapping = aes(x=type,y=res,group=type)) +
  xlab("player type") +
  ylab("rem. chess positions") +
  ggtitle("side by side boxplots with marked values") +
  theme_bw()

# eps-file
dev.copy2eps(file="../pictures/chess_bp.eps")
#nr10
# generate the data
distance <- c(12.5,29.9,14.8,18.7,7.6,16.2,16.5,27.4,12.1,17.5)
altitude <- c(342,1245,502,555,398,670,796,912,238,466)
# sorted data
sort(distance)
sort(altitude)

# mean and median
mean(distance)
mean(altitude)

# R offers several ways of calculating quantiles. Use type=1 
# to apply the method we have introduced.
quantile(distance,probs = c(0.25,0.5,0.75),type=1)
quantile(altitude,probs = c(0.25,0.5,0.75),type=1)

# interquartial range
quantile(distance,probs=0.75,type=1)- quantile(distance,probs=0.25,type=1)
quantile(altitude,probs=0.75,type=1)- quantile(altitude,probs=0.25,type=1)

# variance
var(altitude)
var(distance)

# coefficients of variation
sd(distance)/mean(distance)
sd(altitude)/mean(altitude)

# boxplots
par(mfrow=c(1,2))
boxplot(altitude,xlab="",ylab="Altitude (in m)", 
        main = "altitude",
        cex.axis=1.5,lwd=3,cex.lab=1.75,cex.main=1.75) 
boxplot(distance,xlab="",ylab="Distance (in km)", 
        main = "distance",
        cex.axis=1.5,lwd=3,cex.lab=1.75,cex.main=1.75) 

# eps-file
dev.copy2eps(file="../pictures/hiking_bp.eps")
###############################################################################
#other way
# boxplots with ggplot
ggplot(data = tibble(d=distance)) +
  geom_boxplot(mapping = aes(x=d)) +
  geom_point(mapping = aes(x=d,y=0)) +
  scale_y_continuous(labels = NULL, breaks = NULL) +
  ylab("") +
  xlab("distance (in km)") +
  coord_flip() +
  ggtitle("distance") +
  theme_bw()
ggplot(data = tibble(a=altitude)) +
  geom_boxplot(mapping = aes(x=a)) +
  geom_point(mapping = aes(x=a,y=0)) +
  scale_y_continuous(labels = NULL, breaks = NULL) +
  ylab("") +
  xlab("altitude (in m)") +
  coord_flip() +
  ggtitle("altitude") +
  theme_bw()

############################## wichtig
# grouped data
bounds <- c(5,15,20,30)
dist_cut <- cut(distance, breaks = bounds)

df_cut <- tibble(values = dist_cut)
df_cut_tab <-
  df_cut %>%
  count(values) %>%
  mutate(rel = n / length(distance),
         cum_rel_freq = cumsum(rel))
df_cut_tab
# tex table
tex_tab <- xtable(df_cut_tab)
print(tex_tab, include.rownames = FALSE, floating = FALSE)

midpoints.classes <- (bounds[-1]+bounds[-4])/2
mean.grouped <- sum(midpoints.classes * df_cut_tab$rel)
mean.grouped
# weighted.mean(c(10,17.5,25),c(4/10,4/10,2/10))
#########################################
#nr11
?mpg()
names(mpg)
tab <- 
  mpg %>%
  select(displ,hwy) %>%
  mutate(displ_class = cut(displ,breaks = c(1,3,5,8),
                           labels = c("small","medium","big")))
tab
?cut()
?group_by()
#mean,max,min....
stat_hwy_displ <-
  tab %>%
  group_by(displ) %>%
  summarise(mean=mean(hwy),min=min(hwy),max=max(hwy),
            q1=quantile(hwy,0.25, type=2),
            q2=quantile(hwy,0.5, type=2),
            q3=quantile(hwy,0.75, type=2)
  )
stat_hwy_displ  
boxplot(hwy ~ displ_class, data = tab, xlab = "displ_class", ylab = "hwy")
