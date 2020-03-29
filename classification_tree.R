# Binary Classification Tree
library(rpart)
library(rpart.plot)
ds_raw = read.csv("data_filtered.csv",header=TRUE)
ds <- subset(ds_raw,select=-c(X,PassengerId, firstName,lastName,Cabin,Ticket))

## Set tree regression model
ctrl = rpart.control(minsplit = 15,cp=0.000001, maxdepth = 4)

# Create and plot the classification tree
f <- ds$Survived~.
tree = rpart(f,data=ds,method="class",control=ctrl)
prp(tree)

# Predicted values
wp = as.integer(predict(tree,data=ds)[,2] >= 0.5)
predictTest = predict(tree,data=ds)[,2]
# Prediction accuracy -- Training data
acc = sum(wp == ds$Survived)/nrow(ds)
acc
conf = table(ds$Survived, predictTest>0.5)

# Prediction accuracy -- Testing data
ds_test_raw = read.csv("testing_filtered.csv")
ds_test <- subset(ds_test_raw,select=-c(X,PassengerId,lastName,firstName,Cabin,Ticket))
prediction <- as.integer(predict(tree,newdata=ds_test)[,2] >= 0.5)

#Write to CSV
output <- data.frame(PassengerId=ds_test_raw$PassengerId,Survived=prediction)
write.csv(output,"predict.csv")
