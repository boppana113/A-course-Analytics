# Load datasets
gender = read.csv("gender_submission.csv",header = TRUE)
data = read.csv("train.csv",header = TRUE)

# Remove Rows with incomplete cases 
data <- na.omit(data)
data <- subset(data, select = -c(1,4,9))
write.csv(data,"train_filtered.csv")
dim(data)
pairs(data[,1:9])

# Decision Tree Model