# Load Libraries
library(corrplot)
library(dplyr)
library(ggplot2)

# Load datasets
gender = read.csv("gender_submission.csv",header = TRUE)
data = read.csv("train.csv",header = TRUE)

# Remove Rows with incomplete cases 
data <- na.omit(data)
data <- subset(data, select = -c(1,4,9)) #Removed ID, Name, and Ticket number 
write.csv(data,"train_filtered.csv")
dim(data)

hist(data[data$Survived==1,]$Age, col="lightgreen", xlab="Age",main = "Survived by Age")
hist(data[data$Survived==0,]$Age, col="firebrick2", xlab="Age",main = "Died by Age")

pairs(data)

# Bifurcate into those who survived and those who died
# data_lived <- subset(data, Survived==1)
# data_died <- subset(data, Survived==0)
data_lived <- subset(data, Survived==1, select = -c(Survived))
data_died <- subset(data, Survived==0, select = -c(Survived))

summary(data_lived)
summary(data_died)

## Investigate Male vs Female Survival Rates
ggplot(data,aes(x=Sex)) +
  geom_bar()

ggplot(data_lived,aes(x=Sex)) +
  geom_bar()

ggplot(data_died,aes(x=Sex)) +
  geom_bar()

ggplot(data_lived,aes(x=Pclass)) +
  geom_bar()

ggplot(data_died,aes(x=Pclass)) +
  geom_bar()

ggplot(data_lived,aes(x=Embarked)) +
  geom_bar()

ggplot(data_died,aes(x=Embarked)) +
  geom_bar()