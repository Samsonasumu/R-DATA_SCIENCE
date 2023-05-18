library(ggplot2)  
library(tidyverse)


#QUESTION ONE
MKmart <- read_csv("sammyR/MKmart_raw.csv")

#function one : structure of the dataset
str(MKmart)#date is character and all others are numerical
dim(MKmart)#6435 rows and 8 columns
summary(MKmart)#min mean median 1st and 3rd quartiles

#2. Determine the variables with missing values (NAs) and print the total number
#of NAs in each of the variable.
#is.na(MKmart)
sum(is.na(MKmart))#122 missing values
colnames(MKmart)
sum(is.na(MKmart$Store))#zero null
sum(is.na(MKmart$CPI))#52 null values
sum(is.na(MKmart$Date))#zero null
sum(is.na(MKmart$Unemployment))#zero null
sum(is.na(MKmart$Weekly_Sales))#37null
sum(is.na(MKmart$Holiday_Flag))#zero null
sum(is.na(MKmart$Temperature))#7 null
sum(is.na(MKmart$Fuel_Price))#26 null



#3. Determine the outliers in Weekly_Sales, Temperature, Fuel_Price, CPI and
#Unemployment variables and remove all the outliers. Make sure at the end,
#you must produce a dataframe named “MKmart2” without outliers in all those
#four variables.

#checking for outliers using a boxplot
boxplot(MKmart$Weekly_Sales)#some outlier present
boxplot(MKmart$Temperature)#some outlier present
boxplot(MKmart$CPI)#no outlier
boxplot(MKmart$Fuel_Price)#no outlier
boxplot(MKmart$Unemployment)#some outlier present

#removing outliers in weekly sales
summary(MKmart$Weekly_Sales)
IQR_sales = 1415679 - 551601
up_sale = 1415679 + 1.5*IQR_sales
low_sale = 551601 - 1.5*IQR_sales
up_sale#2711796
#removing outliers in Temperature
summary(MKmart$Temperature)
IQR_temp = 74.94 - 47.42
up_temp = 74.94 + 1.5*IQR_temp
low_temp = 47.42  - 1.5*IQR_temp

up_temp#116.22
#removing outliers in 
summary(MKmart$Unemployment)
IQR_un = 8.622 - 6.891
up_une = 8.622 + 1.5*IQR_un
low_une = 6.891  - 1.5*IQR_un


summary(MKmart)
MKmart2 = subset(MKmart,Unemployment<=11.2185 & Unemployment>=low_une 
                  & Temperature<=116.22 & Temperature>=low_temp & 
                     Weekly_Sales<=2711796 & Weekly_Sales>=low_sale & CPI<=227.2 
                   & Fuel_Price<=4.468)
boxplot(MKmart2$Weekly_Sales)#no outliers



#4. Remove all the rows with NAs in “MKmart2” and assign a new name to the
#dataframe as “MKmart_clean”.
MKmart_clean <- na.omit(MKmart2) 
#is.na(MKmart_clean)

#5. Visualize the distribution of the continuous variable Weekly_Sales in the
#"MKmart_clean" dataframe, using a histogram function from ggplot2 R
#package. Add title and x-axis label to the histogram.
library(ggplot2)
hist(MKmart_clean$Weekly_Sales)

#6. Create a bar chart using the ggplot2 R package to visualize the comparison
#between Holiday_Flag and Weekly_Sales, based on the data in the
#"MKmart_clean" d
MKmart_clean$Holiday_Flag <- as.factor(MKmart_clean$Holiday_Flag) 
ggplot(MKmart_clean, aes(x=Weekly_Sales, y=Holiday_Flag)) + 
  geom_bar(stat = "identity")+
  scale_fill_brewer(palette = "steelblue") +
  theme(legend.position="none")

#7. Interpret the relationship between Holiday_Flag and Weekly_Sales in your
#own words.
  #days without holiday had more sales compared to those with



#8. Using the ggplot2 R package, create a correlation heatmap with correlation
#oefficient labels (2 decimal places) to evaluate the relationship between
#Weekly Sales, Temperature, and Fuel_Price
tmp <- MKmart_clean %>% 
  dplyr::select('Weekly_Sales','Temperature','Fuel_Price')
head(tmp)
install.packages("lattice")
library(lattice)

# rounding to 2 decimal places
corr_m <- round(cor(tmp),2) 
head(corr_m)  
#CORRELATION HEATMAP
install.packages("reshape2")
library(reshape2)

  
# reduce the size of correlation matrix
melted_corr_mat <- melt(corr_m)
# head(melted_corr_mat)

# section c questio 2 plotting the correlation heatmap
library(ggplot2)
ggplot(data = melted_corr_mat, aes(x=Var1, y=Var2,
                                   fill=value)) +
  geom_tile()

 



####################################33########333#
#SECTION B (20 Marks)
#creating the two  dataframes
StudentID <- c(101,102,103,104,105,106)
Product <- c("Biology","Math","English","Science","Polical Science","Physics")
df1 <- cbind(StudentID,Product)
head(df1) 

#creating the second dataframe
StudentID <- c(102,104,106,107,108)
State <- c("Kuala Lumpur","Johor","Penang","Melaka","Kuala Lumpu")
df2 <- cbind(StudentID,State)
head(df2)
install.packages("tidyverse")
library(tidyverse)
#left_join(df1,df2,by="StudentID")
#df3                        


#3. Write R codes to display only the StudentId and Product which contain
#missing values for State in df2. Show the output of the new dataframe as
#“df4”.
#df3 %>%
  #select(StudentID,Product) %>%
  #filter(df3, State == 'NA')



#4. Create “df5” with two variables, StudentId and Marks for 10 students with IDs
#ranging from 101 until 110. Add th
StudentID <- c(101,102,103,104,105,106,107,108,109,110)
Marks <- c(70,90,87,95,93,86,NA,NA,NA,NA)
df5 <- cbind(StudentID,Marks)

head(df5)
#
#df6 
#df7 <- inner_join(df5,df6,by="StudentID")


########################################################33
#SECTION C 
#QUESTION ONE

weather <- read_csv("sammyR/weather.csv")
summary(weather)#descriptive statistics
head(weather,3)#gives the first 3 variables 
names(weather)#column names ie the variables


#2. Using the ggplot2 R package, create a correlation heatmap with correlation
#coefficient labels (1 decimal place) to evaluate the relationship between all the
#numerical variables (predictor variables) of weather.
library("dplyr")
library(MASS)

 fg <- weather %>% 
  dplyr::select('precipitation','temp_max','temp_min','wind')
head(fg)
install.packages("lattice")
library(lattice)
# rounding to 2 decimal places
corr_mat <- round(cor(fg),1) 
head(corr_mat)  
#CORRELATION HEATMAP
# Install and load reshape2 package
install.packages("reshape2")
library(reshape2)
# creating correlation matrix
corr_mat <- round(cor(fg),1)
# reduce the size of correlation matrix
melted_corr_mat <- melt(corr_mat)
# head(melted_corr_mat)
# section c questio 2 plotting the correlation heatmap
library(ggplot2)
ggplot(data = melted_corr_mat, aes(x=Var1, y=Var2,
                                   fill=value)) +
  geom_tile()

weather

#3 From the correlation heatmap above it is evident that there 
#is a high correlation between temperature and minimum temperature maximum
#a correlationof 0.9
#you can also observed that there is a low negative correlation between precipitation and the temperature minimum of -0.1 
#There is a low correlation between wind and temperature minimum this 
#correlation is negative
#there is a positive correlation between wind and precipitation correlation
#of 0.3 there is a high correlation between temperature maximum and
#precipitation this correlation is negative


#question 4

 
ggplot(data=weather,aes(x=weather ,y=wind) )+
  geom_boxplot()

#5.  Snow does not contain any outliers 




#6, Wind has a high correlation with the  precipitation and this correlation is positive ie 0.3 
