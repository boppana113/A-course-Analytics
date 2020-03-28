# Binary Classification Tree
library(rpart)
library(rpart.plot)
ds_raw = read.csv("data_filtered.csv",header=TRUE)
ds <- subset(ds_raw,select=-c(X,PassengerId, firstName,lastName,Cabin,Ticket))

## Linear Regression model
# f <- ds$Survived~.
# reg <- glm(f, data = ds, family = binomial)
# summary(reg)

## Set tree regression model
ctrl = rpart.control(minibucket=5,cp=0.1)

# Create and plot the classification tree
f <- ds$Survived~.
tree = rpart(f,data=ds,method="class",control=ctrl)
prp(tree)

# Predicted values
wp = as.integer(predict(tree,data=ds)[,2] >= 0.5)

# Prediction accuracy -- Training data
acc = sum(wp == ds$Survived)/nrow(ds)
acc

# Prediction accuracy -- Testing data
ds_test_raw = read.csv("test.csv")
ds_test <- subset(ds_test_raw,select=-c(PassengerId,Name,Cabin,Ticket))
# wp_test = as.integer(predict(tree,data=ds_test)[,2] >= 0.5)
p <- round(predict(tree,data=ds_test)[,2],0)
output<-cbind(ds_test_raw$PassengerId,p)
write.csv(output,"predict.csv")
