# Load Libraries
library(corrplot)
library(dplyr)
library(ggplot2)
library(tidyr)
# Load datasets
data = read.csv("train.csv",header = TRUE)
output <- data
data <- separate(data = data, col=Name, into = c("lastName","firstName"), sep=",")

# Create Data frame that removes rows with "NA" fields
df <- na.omit(data)

# Seperate into Adults, Children, and those w/o age listed
adults = subset(df,Age>=18)       # Adults
children = subset(df,Age<18)      # Children 
NPerson = subset(data,is.na(Age)) # No Age Listed
sib_sp = subset(data,SibSp>0)     # Siblings/Spouses
par_ch = subset(data, Parch>0)    # Parent/Children

# Find mean age
mean_age = mean(df$Age)               # Mean Age
mean_age_adult = mean(adults$Age)     # Mean Adult Age
mean_age_child = mean(children$Age)   # Mean Child Age

# Check if they are Married Women
for (k in 1:dim(NPerson)[1])
{
  # Update the age to be average adult age
  if ((is.na(NPerson$Age[k]) & grepl("Mrs.",NPerson$firstName[k],fixed = TRUE)))
  {
    NPerson$Age[k] <- mean_age_adult
  }
}

# Append df with updated data from NPerson
temp <- subset(NPerson,!is.na(Age))
df <- rbind(df,temp)

#Update NPerson based upon who is still unknown
NPerson = subset(NPerson,is.na(Age))

# Check if individuals in NPerson had a sibling/spouse in df
for (i in 1:(dim(NPerson)[1]))
{
  # Check if they have siblings/spouses
  if (NPerson$SibSp[i] > 0) 
  {
    # Check their last name across the sib_sp dataset 
    for (j in 1:dim(df)[1]) 
    {
      # Check if they have the same last name
      if (NPerson$lastName[i] == df$lastName[j])
      {
        # Check if they have the same number of spouse/siblings
        if (NPerson$SibSp[i] == df$SibSp[j])
        {
          # Give the Pair the same age
          NPerson$Age[i] = df$Age[j]
        }
      }
    }
  }
}

# Append df with updated data from NPerson
temp <- subset(NPerson,!is.na(Age))
df <- rbind(df,temp)

#Update NPerson based upon who is still unknown
NPerson = subset(NPerson,is.na(Age))

# Check if individuals in NPerson had a parent/child 
for (i in 1:(dim(NPerson)[1]))
{
  # Check if they have parents/children
  if (NPerson$Parch[i] > 0) 
  {
    # Check their last name across the complete data set 
    for (j in 1:dim(df)[1]) 
    {
      # Make sure you are not looking at the same row of data
      # Check if they have the same last name
      if (NPerson$lastName[i] == df$lastName[j])
      {
        # If reference is adult assume the unknown person is a child
        if (df$Age[j] > 18)
        {
          NPerson$Age[i] <- mean_age_child
          
        }
        # If reference is child assume the person is an adult
        else if (df$Age[j] < 18)
        {
          NPerson$Age[i] <- mean_age_adult
        }
        
        else 
        {
          NPerson$Age[i] <- mean_age
        }
      }
    }
  }
}

# Append df with updated data from NPerson
temp <- subset(NPerson,!is.na(Age))
df <- rbind(df,temp)

# Update NPerson based upon who is still unknown
NPerson = subset(NPerson,is.na(Age))

# Assume the rest have the average age of the entire group
NPerson$Age <- mean_age

# Append df with updated data from NPerson
temp <- subset(NPerson,!is.na(Age))
df <- rbind(df,temp)

# Update NPerson based upon who is still unknown
NPerson = subset(NPerson,is.na(Age))

data$Age <- df$Age
write.csv(data,"data_filtered.csv")
