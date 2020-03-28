# Load Libraries
library(corrplot)
library(dplyr)
library(ggplot2)

# Load datasets
# gender = read.csv("gender_submission.csv",header = TRUE)
data = read.csv("data_filtered.csv",header = TRUE)

dim(data)
str(data)
# Name Columns
colnames(data) = c("Count", "PassengerID", "Survived", "Pclass", "lastName", "firstName", "Sex", "Age", "Sibsp",
                   "Parch", "Ticket", "Fare", "Cabin", "Embarked")
str(data)
plot(data$Sex, data$Age, ylab = "Age", main = "Age Distribution by Sex for All Passengers")
outliers <- list(375, 133, 310, 16, 241, 300, 10, 99, 255, 618, 15, 124, 141, 11, 157, 217, 521, 294, 542, 137, 41, 367, 26, 394, 499, 641, 603, 510, 60, 393, 374, 513, 704, 496, 617, 389, 74, 188, 466, 551, 263, 495, 204, 442, 452, 663, 111, 386, 352, 434, 226, 532, 593, 92, 75, 680)
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

# Box and Whiskers Plot for Survived/Died
plot(data_lived$Sex, data_lived$Age, ylab = "Age", main = "Age Distribution by Sex for Surviving Passengers")

plot(data_died$Sex, data_died$Age, ylab = "Age", main = "Age Distribution by Sex for Dead Passengers")

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

## Investigate Class Survival Rates
ClassPlot<- ggplot(data,aes(x=Pclass,)) +
  geom_bar()
print(ClassPlot + ggtitle("All Passengers by Class"))

SurvivedClassPlot <- ggplot(data_lived,aes(x=Pclass)) +
  geom_bar()
print(SurvivedClassPlot + ggtitle("Surviving Passengers by Class"))

DiedClassPlot <- ggplot(data_died,aes(x=Pclass)) +
  geom_bar()
print(DiedClassPlot + ggtitle("Dead Passengers by Class"))

## Investigate Embarked Survival Rates
EmbarkedPlot <- ggplot(data,aes(x=Embarked)) +
  geom_bar()
print(EmbarkedPlot + ggtitle("All Passengers by Point of Embarkation"))

ggplot(data_lived,aes(x=Embarked)) +
  geom_bar()

ggplot(data_died,aes(x=Embarked)) +
  geom_bar()

pairs(~Survived+Sex+Age+Pclass,data=data, main="Survival Scatterplot Matrix", cex=0.7, cex.labels=1.4)

## Fractional Fill ggplot
#Sex
ggplot(data, aes(x=factor(Sex), fill=factor(Survived))) +
  xlab("Sex") +
  ylab("Probability of Survival") +
  guides(fill=guide_legend("Survived")) +
  ggtitle("Effect of Sex on Survival") +
  theme(plot.title = element_text(size = rel(1.7), face="bold")) +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=16)) +
  theme(legend.title=element_text(size=14)) +
  theme(legend.text=element_text(size=14)) +
  geom_bar(position="fill")

# Separate age into 20 year bands and plot those
ggplot(data, aes(x=factor(Age), fill=factor(Survived))) +
  xlab("Age") +
  ylab("Probability of Survival") +
  guides(fill=guide_legend("Survived")) +
  ggtitle("Effect of Age on Survival") +
  theme(plot.title = element_text(size = rel(1.7), face="bold")) +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=16)) +
  theme(legend.title=element_text(size=14)) +
  theme(legend.text=element_text(size=14)) +
  geom_bar(position="fill")

# Class
ggplot(data, aes(x=factor(Pclass), fill=factor(Survived))) +
  xlab("Class") +
  ylab("Probability of Survival") +
  guides(fill=guide_legend("Survived")) +
  ggtitle("Effect of Class on Survival") +
  theme(plot.title = element_text(size = rel(1.7), face="bold")) +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=16)) +
  theme(legend.title=element_text(size=14)) +
  theme(legend.text=element_text(size=14)) +
  geom_bar(position="fill")

# Sibsp
ggplot(data, aes(x=factor(Sibsp), fill=factor(Survived))) +
  xlab("Siblings or Spouses Aboard") +
  ylab("Probability of Survival") +
  guides(fill=guide_legend("Survived")) +
  ggtitle("Effect of Having Siblings or Spouses Aboard on Survival") +
  theme(plot.title = element_text(size = rel(1.7), face="bold")) +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=16)) +
  theme(legend.title=element_text(size=14)) +
  theme(legend.text=element_text(size=14)) +
  geom_bar(position="fill")

# Parch
ggplot(data, aes(x=factor(Parch), fill=factor(Survived))) +
  xlab("Parents or Children Aboard") +
  ylab("Probability of Survival") +
  guides(fill=guide_legend("Survived")) +
  ggtitle("Effect of Having Parents or Children Aboard on Survival") +
  theme(plot.title = element_text(size = rel(1.7), face="bold")) +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=16)) +
  theme(legend.title=element_text(size=14)) +
  theme(legend.text=element_text(size=14)) +
  geom_bar(position="fill")

# Embarked
ggplot(data, aes(x=factor(Embarked), fill=factor(Survived))) +
  xlab("Port of Embarkment") +
  ylab("Probability of Survival") +
  guides(fill=guide_legend("Survived")) +
  ggtitle("Effect of Port of Embarkment on Survival") +
  theme(plot.title = element_text(size = rel(1.7), face="bold")) +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=16)) +
  theme(legend.title=element_text(size=14)) +
  theme(legend.text=element_text(size=14)) +
  geom_bar(position="fill")

data_Class1 <- subset(data, Pclass==1)
data_Class1Females <- subset(data_Class1, Sex=="female")
