#Nummer 1
#sum of 
52.3 + 47.8 + 3.17
#square root of 144
sqrt(144)
# log basis 10 von 200 *sin(Pi/4)
log10(200*sin(3.14/4))
#cumulutive sum
cumsum(c(1,3,18,20,2))
#sample
sample(10,20,replace = TRUE,prob = NULL)
#Nummer 2
x<- 5
y<-10
x+y
z <- x+y
z
myvec <- c(x,y,z)
min(myvec)
max(myvec)
mean(myvec)
remove(myvec)
#nummer 3
rainfall <- c(0.1,0.5,2.3,1.1,11.3,14.7,23.4,15.7,0,0.9)
rainfall <- c(0,1,2,3,4,5)
mean(rainfall)
sd(rainfall)
cumsum(rainfall)
sum(rainfall)
max(rainfall)
which.max(rainfall)
which(rainfall <1)
rainfall[rainfall == 0 | rainfall == 5]
#nummer 4
len <- c(2.5,3.4,4.8,3.1,1.7)
diam <- c(0.7,0.4,0.5,0.5,0.9)
vol <- len * (0.5*diam)**2 * pi

#nr6
matrix <- matrix(data = c(seq(from=0, to= 18,by= 2), as.integer(runif(70,0,100))), nrow = 8, ncol = 10, byrow = TRUE)
matrix
#nummer 7
library(ggplot2)
library(tidyverse)
names(mpg)
head(mpg)

str_mpg <- tibble(name = character(), type = character(), level = character(), dc = character())
str_mpg <- str_mpg %>%
  add_row(name = "manufacturer", type = "qualitative", level = "nominal", dc = "discrete") %>%
  add_row(name = "model", type = "qualitative", level = "nominal", dc = "discrete") %>%
  add_row(name = "displ", type = "quantitative", level = "ratio", dc = "continous") %>%
  add_row(name = "year", type = "quantitative", level = "interval", dc = "discrete") %>%
  add_row(name = "cyl", type = "quantitative", level = "ratio", dc = "discrete") %>%
  add_row(name = "trans", type = "qualitative",level = "nominal", dc = "discrete") %>%
  add_row(name = "drv", type = "qualitative", level = "nominal", dc = "discrete") %>%
  add_row(name = "cty", type = "quantitative", level = "ratio", dc = "continous") %>%
  add_row(name = "hwy", type = "quantitative", level = "ratio", dc = "continous") %>%
  add_row(name = "fl", type = "qualitative", level = "nominal", dc = "discrete") %>%
  add_row(name = "class", type = "qualitative", level = "nominal", dc = "discrete")
str_mpg
#display
str(str_mpg)
#subset
subset(str_mpg, subset = (type == "quantitative" & dc == "discrete"))
