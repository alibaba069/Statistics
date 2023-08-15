#nr 1
sum(52.3,47.8,3.17)

sqrt(144)

log10(200 * sin(pi/4))

cumsum(c(1,3,18,20,2))

sample(10,20,replace = TRUE,prob = NULL)

#nr 2
x <- 5
y <- 10
x+y
z <- x+y
myvec <- c(x,y,z)
min(myvec)
max(myvec)
mean(myvec)
remove(prod)
remove(myvec)
#nr 3
rainfall <- c(0.1,0.5,2.3,1.1,11.3,14.7,23.4,15.7,0,0.9)
mean(rainfall)
sd(rainfall)
cumsum(rainfall)
sum(rainfall)
max(rainfall)
which.max(rainfall)
which(rainfall <1)
rainfall[rainfall == 0 | rainfall ==1.1]
#nr 4
len <- c(2.5,3.4,4.8,3.1,1.7)
diam <- c(0.7,0.4,0.5,0.5,0.9)
vol <- len * (0.5*diam)**2 * pi
#nr7
library(ggplot2)
library(tidyverse)
library(ggplot2)
library(tidyverse)
help(mpg)
names(mpg)
head(mpg)
# Create an empty tibble str_mpg with variables name, type,
# level and dc of type character(). 
str_mpg <- tibble(name = character(), type = character(), level = character(), dc = character())
# Add for every variable in the dataset mpg a row containing for every variable the
# name, the type, the level of measurement and discrete/continous. 
str_mpg <- str_mpg1 %>%
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
head(str_mpg)
# Display the structure
str(str_mpg)
# Use the tibble to display all variables which are quantitative and discrete 
# applying the R function subset().
subset(str_mpg, subset = (type == "quantitative" & dc == "discrete"))
