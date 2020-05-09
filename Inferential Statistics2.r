
#install.packages("readxl")
#install.packages("ggplot2")
#install.packages("pastecs")
#install.packages ("magrittr")
#install.packages("dplyr")

#Import packages
library("readxl")
library("ggplot2")
library("ggpubr")
library("pastecs")
library("magrittr")
library("dplyr")

# Reading xls data files
my_data <- read_excel("SAS_Data.xlsx")

#res <- stat.desc(my_data,basic=F)
res <- stat.desc(my_data$Measure)
round(res, 2)

# get means for variables in data frame mydata
# excluding missing values
sapply(my_data$Measure, mean, na.rm=TRUE)
 
# mean,median,25th and 75th quartiles,min,max
summary(my_data)
# Box plot 
ggboxplot(my_data, x = "Level", y = "Measure", color = "Level", 
          palette = c("#343445","#e094a5"), fill = c("#343445","#e094a5"))

ggboxplot(my_data, x = "Task", y = "Measure", color = "Task", 
          palette = c("#00AFBB", "#082970"), fill = c("#00AFBB", "#082970"))

# Histogram of the data 
gghistogram(my_data, x = "Measure", color = "Level" , add = "mean", bins = 9, fill = c("#e094a5"))
gghistogram(my_data, x = "Measure", color = "Task" , add = "mean", bins = 9, fill = c("#00AFBB"))

# Q plot
ggqqplot(my_data, x = "Measure", y = "Level", color = "Level", palette = c("#343445","#e094a5"))
ggqqplot(my_data, x = "Measure", y = "Task", color = "Task", palette = c("#00AFBB", "#082970"))

#Shapiro test
shapiro.test(my_data$Measure)

#Two factor ANOVA
# Check the structure
str(my_data)

#Fit an Analysis of Variance Model
#Use task as factor variable
res.aov <- aov(Measure ~ Task + Level, data = my_data)
summary(res.aov)
model.tables(res.aov, type="means", se = TRUE)

###EXTRA

TukeyHSD(res.aov, which = "Task")
#diff: difference between means of the two groups
#lwr, upr: the lower and the upper end point of the confidence interval at 95% (default)
#p adj: p-value after adjustment for the multiple comparisons.

# 1. Homogeneity of variances
plot(res.aov, col = c("#082970"), 1)

# 2. Normality
plot(res.aov, col = c("#082970"), 2)
