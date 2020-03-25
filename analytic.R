gender = read.csv("gender_submission.csv",header = TRUE)
data = read.csv("train.csv",header = TRUE)

# Remove Rows with incomplete cases 
data[complete.cases(data), ] # Keep only the complete rows
data_complete <- data[complete.cases(data), ] # Store the complete cases subset in a new data frame

summary(data_complete)
write.csv(data_complete,"train_complete.csv")
