#loading necessary libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(caTools)

#Read the Titanic dataset
tested<-read.csv("internship tasks/tested.csv")

#Check for any missing values
any(is.na(tested))

#Display the structure of the dataset
str(tested)

#Explore the data in the tested data frame
dim(tested)
head(tested)
tail(tested)

#Removing the 11th row from dataset which is Cabin having missing values
tested<-tested[-11]
tail(tested)

#Count the missing values
sum(is.na(tested))
sum(is.na(tested$Age))
colSums(is.na(tested))

#Remove rows with missing values
tested.clean<-na.omit(tested)
nrow(tested.clean)

#Finding the summary of the data
table(tested.clean$Survived)
table(tested.clean$Sex)
summary(tested.clean$Survived)
names(tested.clean)
var(tested.clean$Fare)
sd(tested.clean$Fare)

#Predicting Survival with Sex
ggplot(tested, aes(x = Sex, fill = Survived)) +
  geom_bar() +
  #scale_fill_manual(values = c("blue", "orange"))
  theme_bw() +
  labs(x = "Sex", title = "Predicting Survival with Age")

#Predicting Survival with Age
ggplot(tested, aes(x = Age, fill = factor(Survived))) + 
  geom_histogram(binwidth = 1) + 
  scale_fill_manual(values = c("yellow", "gray")) + 
  labs(x = "Age", title = "Predicting Survival with Age")

#Predicting Survival with Pclass
ggplot(tested, aes(x = Pclass, fill = factor(Survived))) + 
  geom_bar(binwidth = 1) +
  scale_fill_manual(values = c("orange", "blue"))+
  theme_bw() +
  labs(x = "Pclass", title = "Predicting Survival with Pclass")

#Model the data to train and test data
set.seed(123)
data_sample = sample.split(tested$Survived, SplitRatio=0.80)
train_data = subset(tested,data_sample==TRUE)
test_data = subset(tested,data_sample==FALSE)
dim(train_data)
dim(test_data)


#Using Logistic Regression Model
Logistic_Model <- glm(Survived~ Sex + Age,train_data,family = binomial())
summary(Logistic_Model)
plot(Logistic_Model)
prediction <- predict(Logistic_Model,train_data,type = "response", probability = TRUE)
summary(prediction)

