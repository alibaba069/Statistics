melanoma <- read_csv("melanoma.csv")

# b
# time: type = quantitative, scale = ratio, absolute
# status: type = qualitative, scale nominal
# sex: type = qualitative, scale = nominal
# age: type= quantitative , scale = absolute
# year: type = quantitative , scale = interval
# thickness: type = quantitative, scale = ratio
# ulcer: type = qualitative, scale = nominal

# c

melanoma <- melanoma %>%
  mutate(sex= case_when(sex == 1 ~ "male",
                        sex == 0 ~ "female"),
         status = case_when(status == 1 ~ "dead",
                            status == 2 ~ "alive",
                            status == 3 ~ "cause of death not melanoma"),
         ulcer = case_when( ulcer == 1 ~ "present",
                            ulcer == 0 ~ "absent"),
         live.status = case_when(status == "dead" ~ "dead",
                                 status == "alive" ~ "alive",
                                 status == "cause of death not melanoma" ~ "dead"))
melanoma
# d
conti_tab <- table(melanoma$sex,melanoma$live.status)
addmargins(conti_tab)


chisq_melanoma <- chisq.test(conti_tab)
chisq_melanoma$observed
chisq_melanoma$expected
# e
tabl <- melanoma %>% filter(time >= 3*365) %>% group_by(sex)
cont.tab2 <-chisq.test (tabl$sex, tabl$live.status)$observed
cont.tab2
# melanoma$sex alive dead
# female    91   19
# male      42   15
rel.risk <- (91*15)/(19*42)
rel.risk
# this means females have higher risk to die
# f
summaryofsex <- melanoma %>%
  group_by(sex) %>%
  summarise(
    min = min(age),
    max = max(age),
    mean = mean(age),
    first_quantile = quantile(age, 0.25),
    second_quantile = quantile(age, 0.5),
    third_quantile = quantile(age,0.75)
  )
summaryofsex
# g

boxplot(melanoma$age~melanoma$sex)
#######
ggplot(melanoma, aes(x = sex, y = age, fill = sex)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#FF9900", "#0099CC")) +
  ggtitle("Age by Sex") +
  xlab("Sex") +
  ylab("Age") +
  theme_classic()
# h
add.data.melanoma <- read_csv("add.data.melanoma.csv")
add.data.melanoma
# i
#no because in the 5. column three variables are located in one column instead three seperate column

####################### ws 21
#a
melanoma <- read_csv("melanoma.csv")
#b
view(melanoma)
# time: type = quantitative , scale = ratio/absolute , discrete
# status: type = qualitative, scale = nominal, discrete
# sex: type = qualitative, scale = nominal, discrete
# age: type = quantitative, scale = absolute, discrete
# year: type = quantitative, scale = interval, discrete
# thickness: type = quantitative, scale = ratio, continous
# ulcer: type = qualitative, scale = nominal, discrete

#c

melanoma <- melanoma %>%
  mutate(sex= case_when(sex == 1 ~ "male",
                        sex == 0 ~ "female"),
         status = case_when(status == 1 ~ "dead",
                            status == 2 ~ "alive",
                            status == 3 ~ "cause of death not melanoma"),
         ulcer = case_when( ulcer == 1 ~ "present",
                            ulcer == 0 ~ "absent"))

melanoma <- melanoma %>%
  mutate(live.status = case_when(status =="dead" ~ "dead",
                                  status == "alive" ~ "alive",
                                  status == "cause of death not melanoma" ~ "dead"))
melanoma 
#d 
cont_table <- table(melanoma$sex,melanoma$live.status)
cont_table

chisq_melanoma2 <- chisq.test(melanoma$sex,melanoma$live.status)
chisq_melanoma2$observed
#e
tabl <- melanoma %>% filter(time >= 3*365) %>% group_by(sex)
cont.tab2 <-chisq.test (tabl$sex, tabl$live.status)$observed
cont.tab2
# melanoma$sex alive dead
# female    91   19
# male      42   15
rel.risk <- (91*15)/(19*42)
rel.risk
# this means females have higher risk to die
#f
summary_of_age <- melanoma %>%
  group_by(sex) %>%
  summarise(
    min_age = min(age),
    max_age = max(age),
    mean_age = mean(age),
    q1_age = quantile(age,0.25),
    q2 = quantile(age, 0.5),
    q3 = quantile(age,0.75))
summary_of_age

#g
boxplot(melanoma$age ~ melanoma$sex,horizontal = T)
#extreme values in the group of females in the
# both boxplots seem to be symmetric-› the median is in the center of the box
# looking at the age of both groups we can say the group of males has a higher spread 
#the maximum of males is higher then the females the minimum i5 lower in females

#h
add.data.melanoma <- read_csv("add.data.melanoma.csv")
add.data.melanoma %>%
  separate(col = sex_age_year, into = c("sex","age","year"), sep = "/")


