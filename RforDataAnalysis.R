install.packages("tidyverse")
library(tidyverse)
setwd("/cloud/project/pset2")
do <- read_csv("others.csv")
dl <- read_csv("liedetector.csv")
dd <- read_csv("otherdata.csv")
dd$size <- as.factor(dd$size)
ddt2 <- dd %>% filter(time=="t2")
ddt2

# name       species  type   time  health income height size   disability
#<chr>      <chr>    <chr>  <chr>  <dbl>  <dbl>  <dbl> <fct>  <lgl>     
 # 1 bear bear  bear     mammal t2        70  150.      24 medium FALSE     
#2 cuddly     bear     mammal t2        66   77.8     18 medium TRUE      
#3 super size bear     mammal t2        54   58.8    164 large  FALSE     
#4 big bunny  bunny    mammal t2        62   98.0     44 large  TRUE      
#5 bluey      bunny    mammal t2        57   88.0     49 large  TRUE      
#6 pinky      bunny    mammal t2        74   67.6     38 large  FALSE     
#7 rabbit     bunny    mammal t2        61  139.      17 medium TRUE      
#8 whitey     bunny    mammal t2        67  136       12 small  FALSE     
 
do
dd
head(do, 5)

# QUESTION 1 :A Pearson correlation test was used to test for the 
#correlation between height and scariness.
cor.test(do$height, do$scariness, 
                method = "pearson")

#Pearson's product-moment correlation

#data:  do$height and do$scariness
#t = 2.7898, df = 5, p-value = 0.03846
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
 # 0.06602661 0.96582502
#sample estimates:
 # cor 
#0.7802874 


#QUESTION 4B
dd <- read_csv(file=here("otherdata.csv"))
head(dd)

#name       species type   time  health income height size   disability
#<chr>      <chr>   <chr>  <chr>  <dbl>  <dbl>  <dbl> <fct>  <lgl>     
 # 1 bear bear  bear    mammal t1        77   98       24 medium FALSE     
#2 bear bear  bear    mammal t2        70  150.      24 medium FALSE     
#3 cuddly     bear    mammal t1        74   89.3     18 medium FALSE     
#4 cuddly     bear    mammal t2        66   77.8     18 medium TRUE      
#5 super size bear    mammal t1        68  110      164 large  FALSE     
#6 super size bear    mammal t2        54   58.8    1

t.test(dd$health ~ dd$time, var.equal = TRUE)
 
#Two Sample t-test

#data:  dd$health by dd$time
#t = 3.4088, df = 64, p-value = 0.001134
#alternative hypothesis: true difference in means between group t1 and group t2 is not equal to 0
#95 percent confidence interval:
 # 3.988911 15.283816
#sample estimates:
 # mean in group t1 mean in group t2 
#76.45455         66.81818 

 
disabled <- dd[dd$disability == 'TRUE',]
dim(disabled)
ddt2 <- dd %>% filter(time=="t2")
dim(ddt2)
head(ddt2)

#head(ddt2)
# A tibble: 6 × 9
#name       species type   time  health income height size   disability
#<chr>      <chr>   <chr>  <chr>  <dbl>  <dbl>  <dbl> <fct>  <lgl>     
 # 1 bear bear  bear    mammal t2        70  150.      24 medium FALSE     
#2 cuddly     bear    mammal t2        66   77.8     18 medium TRUE      
#3 super size bear    mammal t2        54   58.8    164 large  FALSE     
#4 big bunny  bunny   mammal t2        62   98.0     44 large  TRUE      
#5 bluey      bunny   mammal t2        57   88.0     49 large  TRUE      
#6 pinky      bunny   mammal t2        74   67.6     38 large  FALSE   

t.test(ddt2$health ~ ddt2$size, var.equal = TRUE)
oneway.test(health ~ size,
            data = ddt2,
            var.equal = TRUE)



disabl_t1 <- dd[dd$time == 't1' & dd$disability == "TRUE"]


#QUESTION 5 CHISQUARE TEST
gfg_table =table(dd$time, dd$disability )
gfg_table
#  FALSE TRUE
#t1    25    8
#t2    17   16
chisq.test(dd$time, dd$disability, correct=FALSE)
#Pearson's Chi-squared test

#data:  dd$time and dd$disability
#X-squared = 4.1905, df = 1, p-value = 0.04065

#QUESTION 8 
anova(lm(health ~ size, data=ddt2))


#QUESTION 9
fit <- lm(health ~ height, data = ddt2)
summary(fit)

#QUESTION 10
fit1 <- lm(health ~ income, data = ddt2)
summary(fit1)


dl <- read_csv(file=here("liedetector.csv"))
head(dl)
t.test(hr ~ type, data = dl)


# QUESTION 3: A two sided two sample t test was used to test for the difference in 
#the average of the heartbeat
head(dl)
t.test(hr ~ type, mu=0,alt="two.sided",data = dl)
#
#Welch Two Sample t-test

#data:  hr by type
#t = 0.25164, df = 93.552, p-value = 0.8019
#alternative hypothesis: true difference in means between group control and group test is not equal to 0
#95 percent confidence interval:
 # -6.339469  8.179469
#sample estimates:
 # mean in group control    mean in group test 
#125.18                124.26 

#QUESTION TWO VISUALIZING THE BOXPLOT
library(ggplot2)
ggplot(data=dl,aes(x=name,y=hr,fill=type))+
  geom_boxplot()+
   ggtitle("Performance on lie detector test")+
  xlab("Name of the person")+
  ylab("Heart Rate")+
  geom_boxplot(trim=FALSE,,)+
  theme(
        legend.text = element_text(size = 6),
        legend.title = element_text(size=18,hjust = 0.5),
        legend.key.height = unit(1,"cm"),
        legend.key.width = unit(1,"cm"),
        legend.position=c(0.1,0.9))+
  guides(fill=guide_legend(title = "disp"))



![boxplot](https://github.com/Samsonasumu/R-DATA_SCIENCE/assets/99386103/8cff5514-8dca-4195-a3cd-a5d4ab3ab7a2)

