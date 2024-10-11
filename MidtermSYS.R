# Carson Dod 
# xbh4ya
# Midterm Exam Pt B

#install
install.packages("ggplot2")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("psych")

library(ggplot2)
#install.packages('dplyr')
library(dplyr)
#install.packages('tidyverse')
library(tidyverse)
library(psych)

housingdir <- "/Users/carsondod/Desktop/SYSPro/SYS3501LAB/Data"
sourcedir <-" /Users/carsondod/Desktop/SYSPro/SYS3501LAB/Code/MidtermSYS.R"

setwd(housingdir)

getwd()
dir()

housing_data <- read.csv("housing-prices.csv")

head(housing_data)



#Question 1: 

ggplot(housing_data, aes(y = Price)) + geom_boxplot()
summary(housing_data)

housing_data

#Question 2: 

lowest <- which.min(housing_data$Price)
lowest
housing_data$Price[lowest]

#Question 3: 

expensive <- which.max(housing_data$Price)
expensive_size <- housing_data$size[expensive]
expensive_size

#Question 4: 

ggplot(housing_data, aes(y = Size)) + geom_boxplot()

Q1 <- quantile(housing_data$Size, 0.25)
Q1

Q3 <- quantile(housing_data$Size, 0.75)
Q3

IQR <- Q3 - Q1
IQR

lower_whisker <- Q1 - (1.5*IQR)
upper_whisker <- Q3 + (1.5*IQR)

which(housing_data > upper_whisker)
outliers <- sum(housing_data$Size < lower_whisker | housing_data$Size > upper_whisker, na.rm = TRUE)
outliers

#Question 6: 

outlier_set <- housing_data %>%
  filter(Size < lower_whisker | Size > upper_whisker)
outlier_set
head(outlier_set)
summary(outlier_set)

#Question 5: 
#read data set^

#Question 7: 

average_price_outliers <- mean(outlier_set$Price, na.rm = TRUE)

average_price_outliers

#Question 8: 

ggplot(housing_data, aes(y = Baths)) + geom_boxplot()

#Question 9: 

pairs.panels(housing_data[, c("Rooms", "Size", "Age", "Price")], cor = TRUE)

#Question 10: 

ggplot(housing_data, aes(x = Size)) + geom_histogram()

#Question 11: 

pairs.panels(housing_data[, c("Rooms", "Size", "Age", "Price")], cor = TRUE)

#Question 12: 
old_homes <- housing_data %>%
  filter(Age == "Old")
old_homes
sum(old_homes$Price)

#Question 13: 

ggplot(housing_data, aes(x = Baths, y = Rooms)) + geom_tile()
table <- table(housing_data$Baths, housing_data$Rooms)

head("airquality")

# Install It
install.packages("VIM")
# Load the VIM library
library(VIM)

# Load the airquality dataset
data("airquality")

# "airquality" loaded into your working environment. Display the first few rows of the dataset
head(airquality)


#Question 14: 

onehundredpercent_variables <- sapply(airquality, function(x) all(!is.na(x)))

onehundredpercent_variables_names <- names(onehundredpercent_variables[onehundredpercent_variables])
onehundredpercent_variables_names

data("airquality")
#Question 15:

library(dplyr)
library(tidyverse)

data("airquality")

str(airquality)

airquality$temp <- as.numeric(as.character(airquality$temp))

na_count <- sum(is.na(airquality$Temp))
na_count

May <- airquality %>% filter(Month == 5)
mean_temp_May <- mean(May$Temp, na.rm = TRUE)
mean_temp_May

June <- airquality %>% filter(Month == 6)
mean_temp_June <- mean(June$Temp, na.rm = TRUE)
mean_temp_June

July <- airquality %>% filter(Month == 7)
mean_temp_July <- mean(July$Temp, na.rm = TRUE)
mean_temp_July

August <- airquality %>% filter(Month == 8)
mean_temp_August <- mean(August$Temp, na.rm = TRUE)
mean_temp_August

September <- airquality %>% filter(Month == 9)
mean_temp_September <- mean(September$Temp, na.rm = TRUE)
mean_temp_September

#Question 16:

ggplot(airquality, aes(x = Solar.R)) + geom_histogram()


#Question 17:


#Question 18:

less_than <-airquality %>%
  filter(Solar.R <= 100)
nrow(less_than)

#Question 19

pairs.panels(airquality[, c("Ozone", "Solar.R", "Wind", "Temp")], cor = TRUE)

#Question 20:


#Question 21:


#Question 22:

ggplot(airquality, aes(group = Month, y = Wind)) + geom_boxplot()

#Question 23:

#Question 24:

#Question 25:
