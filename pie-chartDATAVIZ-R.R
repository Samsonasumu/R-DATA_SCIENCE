library(readxl)

m1 <- read_excel("C:/Users/samso/Downloads/10 - College Energy Drink - M1 - Initial dataset.xlsx")
m2 <- read_excel("C:/Users/samso/Downloads/10 - College Energy Drink - M2 - Change Over Time.xlsx")
m3 <- read_excel("C:/Users/samso/Downloads/10 - College Energy Drink - M3 - Data Composition.xlsx")

library(dplyr)
library(tidyverse)
summary(m2)
head(m1)
tail(m1)
unique(m1$Q6)

# checking for missing values
is.na(m1)
sum(is.na(m1))
sum(is.na(m2))
sum(is.na(m3))


# group by race - ethnicity is labbeled Q2 and then grouping according to income Q6
viz1 <- m1 %>% group_by(Q2) %>%  summarise(meanQ6 = mean(Q6))
viz1
label <- viz1$Q2  
label
slice <- viz1$meanQ6
slice
#  pie chart mean income per race
pie(slice, labels = label, main="mean income per race")

#  pie chart according to classification
viz2 <- m1 %>% group_by(Q2) %>%  summarise(meanClassification = mean(Q4))
head(viz2)
label2 <- viz2$Q2  
slice2 <- viz2$meanClassification
pie(slice2, label = label2, main="mean classification per race")

#m3 
head(m3)
tail(m3)
#question 8 how likely to prchase energy and the scale -2, -1, 1, 2, 

viz3 <- m3 %>% group_by(Q9) %>%  summarise( sumOfTimesTheyDrink = sum(Q7))
a <- c("very unlikely","unlikely","not sure","1 likely","2 llikely","very likely")
b <- c(10,20,61,92,57,0)
# Create the pie chart
pie(b, labels = a, col = rainbow(6))
# Customize the pie chart
title("likelihood to purchase and sum of times they drink")
 
#group number of times they drink vs mean of the hrs spent on internet
viz4 <- m3 %>% group_by(Q7) %>%  summarise( sumOfTimesTheyDrink = mean(Q12))
viz4
bs <- c("0 hrs","1 hr","2hr","3hr","4hrs","5hrs","6hrs")
bg <- c(3.21,3.5,3.15,3,3.15,5,2.67)
pie(bg, labels = bs, col = rainbow(6))
# Customize the pie chart
title("group number of times they drink vs mean of the hrs spent on internet")
