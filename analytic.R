# Load Libraries
library(corrplot)

# Load datasets
gender = read.csv("gender_submission.csv",header = TRUE)
data = read.csv("train.csv",header = TRUE)

# Remove Rows with incomplete cases 
data <- na.omit(data)
data <- subset(data, select = -c(1,4,9))
write.csv(data,"train_filtered.csv")
dim(data)

hist(data[data$Survived==1,]$Age, col="lightgreen", xlab="Age",main = "Survived by Age")
hist(data[data$Survived==0,]$Age, col="firebrick2", xlab="Age",main = "Died by Age")


# Bifurcate into those who survived and those who died
# data_lived <- subset(data, Survived==1)
# data_died <- subset(data, Survived==0)
data_lived <- subset(data, Survived==1, select = -c(Survived))
data_died <- subset(data, Survived==0, select = -c(Survived))

summary(data_lived)
summary(data_died)

## Investigate Male vs Female Survival Rates
hist(data[data$Survived==1,]$Sex, col="lightgreen", xlab="Age",main = "Survived by Gender")
hist(data[data$Survived==0,]$Sex, col="firebrick2", xlab="Age",main = "Died by Gender")
