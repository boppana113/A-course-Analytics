# Load Libraries
library(corrplot)
library(dplyr)
library(ggplot2)

# Load datasets
# gender = read.csv("gender_submission.csv",header = TRUE)
data = read.csv("train.csv",header = TRUE)

# Remove Rows with incomplete cases 
# data <- na.omit(data)
# data <- subset(data, select = -c(1,4,9)) #Removed ID, Name, and Ticket number 
# write.csv(data,"train_filtered.csv")
dim(data)
str(data)
# Name Columns
colnames(data) = c("PassengerID", "Survived", "Pclass", "Name", "Sex", "Age", "Sibsp",
                   "Parch", "Ticket", "Fare", "Cabin", "Embarked")
str(data)
plot(data$Sex, data$Age, main = "Age Distribution by Sex for All Passengers")
outliers <- list(673, 746, 117, 97, 494, 852, 631)
points(data$Sex[unlist(outliers)], data$Age[unlist(outliers)], pch=21, bg="red", cex=1.5)

pairs(~Age+Survived, data=data, main="Variable Scatterplot Matrix", cex=0.7, cex.labels=1.4)

hist(data[data$Survived==1,]$Age, col="lightgreen", xlab="Age",main = "Survived by Age")
hist(data[data$Survived==0,]$Age, col="firebrick2", xlab="Age",main = "Died by Age")

pairs(data)

# Bifurcate into those who survived and those who died
# data_lived <- subset(data, Survived==1)
# data_died <- subset(data, Survived==0)
data_lived <- subset(data, Survived==1, select = -c(Survived))
data_died <- subset(data, Survived==0, select = -c(Survived))

summary(data)
summary(data_lived)
summary(data_died)

## Investigate Male vs Female Survival Rates
SexPlot<- ggplot(data,aes(x=Sex,)) +
  geom_bar()
print(SexPlot + ggtitle("All Passengers by Sex"))

SurvivedSexPlot <- ggplot(data_lived,aes(x=Sex)) +
  geom_bar()
print(SurvivedSexPlot + ggtitle("Surviving Passengers by Sex"))

DiedSexPlot <- ggplot(data_died,aes(x=Sex)) +
  geom_bar()
print(DiedSexPlot + ggtitle("Dead Passengers by Sex"))

##Investigate Class Survival Rates
ClassPlot<- ggplot(data,aes(x=Pclass,)) +
  geom_bar()
print(ClassPlot + ggtitle("All Passengers by Class"))

SurvivedClassPlot <- ggplot(data_lived,aes(x=Pclass)) +
  geom_bar()
print(SurvivedClassPlot + ggtitle("Surviving Passengers by Class"))

DiedClassPlot <- ggplot(data_died,aes(x=Pclass)) +
  geom_bar()
print(DiedClassPlot + ggtitle("Dead Passengers by Class"))

##Investigate Embarked Survival Rates
EmbarkedPlot <- ggplot(data,aes(x=Embarked)) +
  geom_bar()
print(EmbarkedPlot + ggtitle("All Passengers by Point of Embarkation"))

ggplot(data_lived,aes(x=Embarked)) +
  geom_bar()

ggplot(data_died,aes(x=Embarked)) +
  geom_bar()

pairs(~Survived+Sex+Age+Pclass,data=data, main="Survival Scatterplot Matrix", cex=0.7, cex.labels=1.4)
