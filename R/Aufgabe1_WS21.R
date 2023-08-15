#nr1
#a)
melanoma <- read_csv("/Users/username/Desktop/Statistic/R/eigen/melanoma.csv") # <- for mac
melanoma <- read_csv("C:\\Users\\username\\Desktop\\melanoma.csv") # <- for Windows
#b)
names(melanoma) # see all attributes data
description_Table <- tibble(
   name = character(),
   type = character(), 
   level = character(), 
   dc = character() # hier nicht wirklich nützlich
   
)
description_Table <-  description_Table %>%
  add_row(name = "time", type="quantitative ", level = "ratio", dc="diskrete")%>%
  add_row(name = "status", type="qualitative ", level = "nominal", dc="diskrete") %>%
  add_row(name = "sex", type="qualitative ", level = "nominal", dc="diskrete") %>%
  add_row(name = "age", type="quantitative ", level = "absolute", dc="diskrete") %>%
  add_row(name = "year", type="quantitative ", level = "interval", dc="diskrete") %>%
  add_row(name = "thickness", type="quantitative ", level = "interval", dc="continous")%>%
  add_row(name = "ucler", type="qualitative ", level = "nominal", dc="diskrete")
description_Table
# time: type = quantitative, scale = ratio, absolute
# status: type = qualitative, scale nominal
# sex: type = qualitative, scale = nominal
# age: type= quantitative , scale = absolute
# year: type = quantitative , scale = interval
# thickness: type = quantitative, scale = ratio
# ulcer: type = qualitative, scale = nominal
#c)
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
#################################

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
##################################
#d 
conti_tab <- table(melanoma$sex,melanoma$live.status)
addmargins(conti_tab)


chisq_melanoma <- chisq.test(conti_tab)
chisq_melanoma$observed
chisq_melanoma$expected
#e
cont.tab2 <-chisq.test (melanoma$sex, melanoma$live.status)$observed
cont.tab2
# melanoma$sex alive dead
# female    91   35
# male      43   36
(91*15)/(19*42)
# this means females have higher risk to die
#f
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
#g
# ggplot
ggplot(melanoma, aes(x = sex, y = age, fill = sex)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#FF9900", "#0099CC")) +
  ggtitle("Age by Sex") +
  xlab("Sex") +
  ylab("Age") +
  theme_classic()
# boxplot
boxplot(melanoma$age~melanoma$sex)
#h
add.data.melanoma <- read_csv("/Users/aliasger/Desktop/Statistic/R/eigen/add.data.melanoma.csv")
add.data.melanoma
#i
#no because in the 5. column three variables are located in one column instead three seperate column






#####################
#zusatz
#c)
# wenn man neu haben will
new_melanoma <- melanoma %>%
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
new_melanoma

#d)
#### other method 
#count how many dead or alive
melanoma %>%
  #select(sex) %>%
  filter(sex == "female" & live.status == "alive") %>%
  count()
# check
melanoma %>%
  #select(sex) %>%
  filter(sex == "male") %>%
  count()
#making a matrix
d_val <- c(36,35,43,91)

cont_matrix <- matrix(data =d_val,nrow = 2,ncol = 2,byrow = TRUE )
cont_matrix
rownames(cont_matrix) = c("male","female")
colnames(cont_matrix) = c("dead", "alive")
addmargins(cont_matrix)
#transfer to tibble
cont_tab <- as.table(cont_matrix)
cont_tab

#g
maleage <- melanoma %>% select(age) %>% filter(melanoma$sex == "male" )
maleage

femaleage <- melanoma %>% select(age) %>% filter(melanoma$sex == "female")
femaleage

boxplot(maleage,femaleage,
        main = "Side by Side Boxplot for the age depending on their sex ",
        xlab = "sex",
        ylab = "age",
        names = c("m","f")
)

#j
add.data.melanoma %>% 
  separate(col=sex_age_year, into = c("sex","age","year"), sep = "/")

table1
