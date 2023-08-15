#num 1
student1 <- tibble(
  student = c("Adam","Bernd","Christian","Doris"),
  algebra = c(NA, 5, 3, 4),
  analysis = c(2, NA, 1,3),
  diskrete.math = c(3,NA,2,4),
)
student1

student2 <- tibble(
  name = rep(c("Adam", "Bernd", "Christian", "Doris"), each = 2),
  type = rep(c("height", "weight"), 4),
  measure = c(1.83, 81, 1.75, 71, 1.69, 55, 1.57, 62))
student2

student3 <- tibble(
  name = c("Adam", "Bernd", "Christian", "Doris"),
  ratio = c("81/1.83", "71/1.75", "55/1.69", "62/1.57"))
student3

student1 %>% 
  gather('algebra','analysis', 'diskrete.math', key = "exam", value = "grade")

student2 %>%
  spread(key = type, value =measure)

student3 %>% 
  separate(col = ratio, into =c("weight","height"), sep = "/")

#num 2
result <- (5 + 3) %>%
  sqrt() %>%
  log()%>%
  sin()
result

vec <- seq(from = 0.5, to = 5, by= 0.5)
vec
result2 <- vec^2 %>%
  log() %>%
  sum() %>%
  round(2)
result2
# num 3
df <- tibble(
  id = 1:10,
  sex = sample(x =c("f","m"), size = 10,
               replace = TRUE), age = round(runif(10,20,35)),
  score1 = round(runif(10,0,25)) )
df
#a
df %>% filter(sex == "m")
#b 
add_row(df, id = 11, sex = "m", age = 25, score1 = 4)
df
#c 
df <- 
  df %>%
  mutate(score2 = round(runif(10,0,25)),
         score3 = round(runif(10,0,25)),
         scoresum = score1 + score2 + score3,
 # df <- 
  #  df %>%
 #   mutate(score3 = round(runif(10,100,125))) %>%
 # mutate(scoresum = score1 + score2 + score3)
 # df
grade = case_when(
    scoresum <= 37 ~ 5,
    scoresum > 37 & scoresum <= 45 ~ 4,
    scoresum > 45 &scoresum <= 55 ~ 3,
    scoresum > 55 & scoresum <= 65 ~ 2,
    scoresum > 65 ~ 1))
df 
# add_row(df, id = 11, sex = "m", age = 25, score1 = 4,score2 = 55, score3 = 40,scoresum = score1 + score2 + score3, grade =-1)
#d
df %>%
  arrange(sex) %>% #sortiert
  select(id,sex,grade) %>% #wählt aus
  filter(grade <5) #mit der bedingung
df
#e
df %>% 
  group_by(sex) %>%
  summarise(mean_score = mean(scoresum),
            max_score = max(scoresum),
            min_score = min(scoresum),
            med_score = median(scoresum))
df
#nr4
library(tidyverse)
library(nycflights13) 
?flights()
flights
#a
flights <-
flights %>%
  filter(arr_delay >120)
flights
#b
flights %>%
  filter(arr_delay >120 & dep_delay <= 0)
flights
#c
flights %>%
  filter(carrier %in% c("AA","DL","UA")) %>%
  filter(arr_delay <= 0)
flights
#d
flights %>%
  filter(carrier %in% c("UA","AA","DL")) %>%
  filter(month == 5) %>%
  filter(arr_delay > 300) %>%
  select(carrier,flight) %>%
  arrange(carrier,flight) %>%
  unique()
flights
#e
flights %>%
  mutate(dep_time =
           (dep_time %/% 100)*60 + dep_time %% 100) %>%
  mutate(arr_time =
           (arr_time %/% 100)*60 + arr_time %% 100)
#h
flights %>%
  mutate(speed = distance / air_time * 60) %>% select(carrier,flight,speed) %>%
  arrange(desc(speed)) %>%
  top_n(10,speed)
#i
flights %>%
  # remove the NA's
  filter(!is.na(arr_delay)) %>%
  # boolean variable indicating a delay
  mutate(bool_del = if_else(arr_delay < 10,1,0)) %>% group_by(carrier) %>%
  # new columns: nof = number of flights,
  # ndel = number of delays, del_ratio = ratio
  # values calculate per carrier
  mutate(nof = n(), ndel = sum(bool_del), del_ratio = ndel / nof) %>%
  select(carrier, nof, del_ratio) %>%
  # remove multiple entries
  unique() %>% arrange(desc(del_ratio))
#j
flights %>%
  # remove NA's
  filter(!is.na(arr_delay)) %>%
  # boolean variable indicating a delay
  mutate(bool_del = if_else(arr_delay < 10,1,0)) %>%
  # Calculation grouped by carrier and month
  group_by(month,carrier) %>%
  # new columns: nof = number of flights,
  # ndel = number of delays, del_ratio = ratio # values calculate per carrier
  mutate(nof = n(), ndel = sum(bool_del),
         del_ratio = ndel / nof,
         max_ratio = max(del_ratio)) %>%
  # keep only 4 columns
  select(month, carrier, nof, max_ratio) %>%
  # calculation per month
  group_by(month) %>%
  # only highest ratio
  filter(max_ratio == max(max_ratio)) %>%
  # remove multiple entries
  unique() %>% arrange(month)
#k
# 3 tables are generated with values per month and day # which are joined by these variables
full_join(
  flights %>% filter(is.na(dep_delay)) %>% group_by(month, day) %>%
    # number of cancelled flights 
    summarise(nof_canc = n()),
  flights %>%
    group_by(month,day) %>%
    # number of no departure delays
    filter(dep_delay <= 5 & dep_delay >= -5) %>%
    summarise(nof_no_delay = n()) ,
  by = c("month","day") ) %>%
  full_join( flights %>%
               group_by(month,day) %>%
               # means
               summarise(mean_dep_del = mean(dep_delay, na.rm = TRUE),
                         mean_arr_del = mean(arr_delay, na.rm = TRUE))
             ,by = c("month","day") )
corona <- table2
corona %>%
  spread(key = type, value = count)
