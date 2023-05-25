 
library(tidyverse)
setwd('/cloud/project/SamFolder')
employee_data <- read_csv('EmployeeData.csv')
head(employee_data,4)
str(employee_data)
# Convert character column to factor
employee_data$left_company <- as.factor(employee_data$left_company) 
employee_data$department <- as.factor(employee_data$department) 
employee_data$job_level <- as.factor(employee_data$job_level) 
employee_data$business_travel <- as.factor(employee_data$business_travel) 
employee_data$job_satisfaction <- as.factor(employee_data$job_satisfaction)
employee_data$performance_rating <- as.factor(employee_data$performance_rating) 
employee_data$marital_status <- as.factor(employee_data$marital_status) 
#transformed dataset
str(employee_data)
summary(employee_data)
summary(employee_data$salary)

#creating a summary table
library(psych)
describe(employee_data)


employee_data %>% 
  dplyr::group_by(department, left_company) %>% 
  dplyr::summarise(cnt = n()) %>% 
  dplyr::mutate(freq = (cnt / sum(cnt))*100) %>% 
  ggplot(aes(x = department, y = freq, fill = left_company)) +
  geom_bar(position = position_stack(), stat = "identity", width = .7) +
  geom_text(aes(label = paste0(round(freq,0), "%")), 
            position = position_stack(vjust = 0.5), size = 3) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(title = "Department and Attrition", x = "department  ", y = "Percentage") +
  scale_x_discrete(breaks = c("Finance and Operations", "IT and Analytics", "Marketing",
                              "Product Development","Research","Sales "),
                   labels =  c("Finance and Operations", "IT and Analytics", "Marketing",
                               "Product Development","Research","Sales ")) +
  scale_fill_manual(values = c("#fde725",  "#21918c"))



#correlation of the features
correl <- employee_data %>% 
  dplyr::select(c( 
                  "salary", "weekly_hours", 
                  "yrs_at_company",
                  "yrs_since_promotion", "previous_companies", "miles_from_home"))
cormatrix <- cor(correl)
cormatrix

round(cormatrix, 2)
library(ggcorrplot)
install.packages("ggcorrplot")
ggcorrplot(cormatrix, hc.order = TRUE,outline.color = "white", lab = TRUE, colors = c("#52c569", "white", "#fde725"), lab_size = 2.5) +
  labs(title="Correlation of Numeric Variables")


is.null(employee_data)
str(employee_data)
 
ggplot(employee_data, aes(x=salary, fill=left_company, color=left_company)) +
  geom_histogram(position="identity", alpha=0.5) +
  labs(title = "Distribution of Monthly Income") +
  scale_color_manual(values=c("#fde725", "#21918c")) +
  scale_fill_manual(values=c("#fde725", "#21918c"))



#summary table
employee_data %>% group_by(left_company) %>% 
  summarise(n_employees = n(),
            min_salary = min(salary),
            avg_salary = mean(salary),
            max_salary = max(salary),
            sd_salary = sd(salary),
            pct_less_60k = mean(salary <= 60000))




ggplot(data = employee_data, aes(x = salary, fill = left_company)) + 
  geom_histogram(aes(y = ..density..), color = "white", bins = 20) +
  facet_wrap(~ left_company, nrow = 2) +
  labs(title = "Employee Salary Distribution by Status (Left the Comapny - Yes/No)",
       x = "Salary (US Dollars", y = "Proportion of Employees")
