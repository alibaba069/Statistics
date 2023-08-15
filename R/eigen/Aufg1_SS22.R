#Aufgabe 1 SS22
#a
galtonfamilies <- read_csv("GaltonFamilies.csv")
#b
names(galtonfamilies)
# familiy_id: qualitative, nominal, discrete
# father(height): quantitative, ratio, continous
# mother(heigt): quantitative, ratio, continous
#midparentheight: quantitative, ratio, continous
#childeren: quantitative, absolute, discrete
# childnum: qualitative, ordinal, discrete
# gender: qualitative, nominal, discrete 
# childheight: quantitative, ratio, continous

#c
galtonfamilies <- galtonfamilies %>%
  mutate(father = father*2.54,
         mother = mother*2.54,
         midparentHeight = midparentHeight*2.54,
         childHeight = childHeight*2.54)
galtonfamilies

# d
heights.fm <- galtonfamilies %>% 
  gather('father', 'mother', key = type, value = height) %>%
  select(type,height)
heights.fm
#e
summary_of_height <- heights.fm %>%
  group_by(type) %>%
  summarise(
    n = n(),
    min = min(height),
    max = max(height),
    mean = mean(height),
    median = median(height),
    Q1 = quantile(height, 0.25),
    Q2 = quantile(height,0.5),
    Q3 = quantile(height,0.75)
  )
summary_of_height
#f
boxplot(heights.fm$height ~ heights.fm$type,horizontal = T)
#i
parents <- read_csv("parent.csv")
children <- read_csv("children.csv")
parents
children
#j
#left join - adding data from other table in this table 
children <- children %>% left_join(parents, by="...1")
children  
# full join - adding data in other tibble
galtonfamilies %>% full_join(children,by = "...1") %>% select(-X.x,-X.y)
