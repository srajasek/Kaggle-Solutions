list.files()

library(caret)
library(randomForest)

## Get datasets into dataframes
dfgenderclassmodel <- read.csv("genderclassmodel.csv")
dfgendermodel <- read.csv("gendermodel.csv")


## Get Training and Test Data set
dftrain <- read.csv("train.csv", sep=",", header = T)
dftest <- read.csv("test.csv", sep=",", header = T)

## Get column names of each dataset
names(dftrain)
names(dftest)

head(dftrain)
str(dftrain)
summary(dftrain)
summary(dftest)

## Replace NA values
dftrain$Age[is.na(dftrain$Age)] <- 28.0
dftest$Fare[is.na(dftest$Fare)] <- 14.5
dftest$Age[is.na(dftest$Age)] <- 27

## Use CrossTabs to pick best predictors for Categorical variables.
table(dftrain[c("Survived", "Pclass")])

table(dftrain[c("Survived", "Sex")])

table(dftrain[c("Survived","SibSp")])

table(dftrain[c("Survived", "Parch")])

table(dftrain[c("Survived", "Embarked")])


## Use plots for continuous variables
install.packages("fields")
library(fields)

# Compare Survived & Age

attach(dftrain)
bplot.xy(Survived, Age)
detach(dftrain)

attach(dftrain)
bplot.xy(Survived, Fare)
detach(dftrain)

attach(dftrain)
bplot.xy(Survived, Parch)
detach(dftrain)

###### Convert Survived to factor

dftrain$Survived <- as.factor(dftrain$Survived)

set.seed(32)

## Train the model & do K-fold Cross validation
model <- train(Survived ~ Pclass
                        + Sex
                        + Embarked
                        + SibSp
                       ## + Parch
                        + Fare,
                         data = dftrain, method = "rf"
                        , validateModel = trainControl(method = "cv", number = 5))

model

## Predict against the test set

dftest$Survived <- predict(model, newdata = dftest)

## Save required columns to a new dataset
submission <- dftest[c("PassengerId", "Survived")]
write.table(submission, "submission.csv", sep=",", row.names = F, col.names = T)



