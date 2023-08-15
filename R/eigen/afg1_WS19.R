#WS 2019
# nr 1
class.data <- read_csv2("class.data.csv")
class.data
view(class.data)
names(class.data)
#nr 2
# first name: type = qualitative, scale = nominal, dc = discrete
# last name: type = qualitative, scale = nominal, dc = discrete
# ins no: type = qualitative, scale = nominal, dc = discrete
# canteen rating: type = quantitative, scale = ordinal, dc = discrete
# gender: type = qualitative, scale = nominal, dc = discrete
# age: type = quantitative, scale = absolute, dc = discrete
# height: type = quantitative, scale = interval,ratio, dc = continous
# weight: type = quantitative, scale = interval,ratio, dc = continous

#nr 3
class.data <- class.data %>%
  mutate(height = (height*2.51)/100,
         weight = weight*0.45)
class.data

#nr 4
class.data <- class.data %>%
  mutate(bmi = weight/(height)**2)
# nr 5
class.data <- class.data %>%
  mutate(bmi.category = case_when(bmi < 18.5 ~ "underweight",
                                  bmi >= 18.5 & bmi < 24.5 ~ "normalweight",
                                  bmi >= 24.5 & bmi < 30 ~ "overweight",
                                  bmi > 30 ~ "obesity"))
class.data                                  
# nr 6
summary_of_std <- class.data %>% 
  group_by(gender) %>%
  summarise(
    mean_h = mean(height),
    standard_deviation = sd(height),
    min_h = min(height),
    max_h = max(height),
    first_quantile = quantile(height, 0.25),
    second_quantile = quantile(height, 0.5),
    third_quantile = quantile(height,0.75)
  )
summary_of_std
# nr 7
boxplot(class.data$height ~ class.data$gender, horizontal = T)
#nr 8
# male more weigth than women 
#mean of man more right than mean of women
#nr 9
bound <-c(0, 18.5, 24.5,30,max(class.data$bmi))
hist(class.data$bmi, breaks = bound)
#nr 13
cont_tab <- table(class.data$gender,class.data$bmi)
cont_tab
chisq_table <- chisq.test(cont_tab)
chisq_table$observed


