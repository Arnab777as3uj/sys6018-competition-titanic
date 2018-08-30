library(tidyverse)

data_train <- read_csv("train.csv")

data_test <- read_csv("test.csv")

data_full  <- bind_rows(data_train, data_test)

data_full["size_family"] <- NA
  
data_full$size_family <- data_full$SibSp + data_full$Parch + 1

data_full["title"] <- NA

data_full$title <- gsub('(.*, )|(\\..*)', '', data_full$Name) 

# Generating random age values for different types of individuals
for (i in 1:length(data_full$PassengerId)) {
  if (is.na(data_full$Age[i])) {
   if (data_full$Parch[i]<=2 & (data_full$title[i]=='Mr'|data_full$title[i]=='Mrs')){
     data_full$Age[i]<- sample(18:60, 1)
   }
    else if (data_full$Parch[i]>2 & (data_full$title[i]=='Mr'|data_full$title[i]=='Mrs')){
      data_full$Age[i]<- sample(18:60, 1)
    }
    else if (data_full$Parch[i]<=2 & (data_full$title[i]=='Master'|data_full$title[i]=='Miss')){
      data_full$Age[i]<- sample(1:18, 1)
    }
    else if (data_full$Parch[i]>2 & (data_full$title[i]=='Master'|data_full$title[i]=='Miss')){
      data_full$Age[i]<- sample(18:60, 1)}
      else{data_full$Age[i]<- sample(1:60, 1)}
    }
  } 
 
# Converting categorical variables to factor -

data_full$Pclass <- factor(data_full$Pclass)
data_full$Sex <- factor(data_full$Sex)
data_full$title <- factor(data_full$title)
data_full$Embarked <- factor(data_full$Embarked)

# For prediction, let's split the data back into train and test sets - 

data_train <- data_full[1:891,]
data_test <- data_full[892:1309,]

# Let's further split the data_train into train and valid sets -

sub <- sample(1:891,size=445)
d.train <- data_train[sub,]     # Select subset for cross-validation
d.valid <- data_train[-sub,]

# Using logistic regression

d.lg1 <- glm(Survived~Pclass+Sex+Age+Embarked+size_family+title, data=d.train, family = "binomial")
summary(d.lg1)

# For correct model let's drop embarked and title

d.lg2 <- glm(Survived~Pclass+Sex+Age+size_family, data=d.train, family = "binomial")
summary(d.lg2)

# let's see what happens when
# we cross validate.

d.lg2 <- glm(Survived~Pclass+Sex+Age+size_family, data=d.train, family = "binomial")
probs<-as.vector(predict(d.lg2,newdata=d.valid, type="response"))
preds <- rep(0,445)  # Initialize prediction vector
preds[probs>0.5] <- 1 # p>0.5 -> 1
table(preds,d.valid$Survived)

#preds   0   1
#0     247  60
#1      31 108

# Let's combine the d.train and d.valid into the full training set and train the model

d.lg <- glm(Survived~Pclass+Sex+Age+size_family, data=data_train, family = "binomial")
summary(d.lg)

probs<-as.vector(predict(d.lg, newdata=data_test))
preds <- rep(0,418)  # Initialize prediction vector
preds[probs>0.5] <- 1

data_test$Survived <- preds

final_ans <- data_test[,1:2]

write.csv(final_ans, "final_ans.csv",row.names = FALSE)