
library(readxl)

m1 <- read_excel("C:/Users/samso/Downloads/10 - College Energy Drink - M1 - Initial dataset.xlsx")
m2 <- read_excel("C:/Users/samso/Downloads/10 - College Energy Drink - M2 - Change Over Time.xlsx")
m3 <- read_excel("C:/Users/samso/Downloads/10 - College Energy Drink - M3 - Data Composition.xlsx")

library(dplyr)
library(tidyverse)
#histogram
library(ggplot2)
likely_2_purchaseNextweek <- m3$Q8
hist(likely_2_purchaseNextweek,
     col="darkmagenta",
     
     freq=FALSE)



#bubble chart
hrs <- c("0 hrs","1 hr","2hr","3hr","4hrs","5hrs","6hrs")
number_times_drink <- c(3.21,3.5,3.15,3,3.15,5,2.67)
d <- data.frame(cbind(hrs, number_times_drink))
d

ggplot(data = d, aes(x = cyl, y = mpg, size = mpg)) +
  geom_point(shape = 21, fill = "blue", color = "black", alpha = 0.8) +
  labs(x = "Hours", y = "MPG", title = "group number of times they drink vs
       mean of the hrs spent on internet") +
  theme_classic()
