install.packages("readr")
install.packages("caret")
install.packages("randomForest")

library(readr)
library(stringr)
library(caret)
library(car)

set.seed(1)
##setwd("C:\\Users\\User\\Desktop\\Kaggle Code\\AIRBNB")
df_train = read.csv("train_users_2.csv")
df_test = read.csv("test_users.csv")
df_train1 = df_train[names(df_test)]
df_all = rbind(df_train1,df_test)


names(df_all)
str(df_all)
summary(df_all)

df_all$age[is.na(df_all$age)] <-33
boxplot(df_all$age)
summary(df_all$age)
df_all$age[df_all$age < 14]<-14
df_all$age[df_all$age > 100]<-100
summary(df_all$age)
boxplot(df_all$age)

str(df_all)
summary(df_all)

df_all$first_browser<-as.numeric(df_all$first_browser)
sort(unique(X_train$first_browser))


X_train= df_all[df_all$id %in% df_train1$id,]
X_test = df_all[df_all$id %in% df_test$id,]


X_train$country_destination<-df_train$country_destination
str(X_train)

library(randomForest) ## 
set.seed(100)

#X_train$first_browser<-as.numeric(X_train$first_browser)
#sort(unique(X_train$first_browser))
#training_frame$language_fctr<-NULL
fit_rf<-randomForest(country_destination~
                       gender+
                       age+signup_method+signup_flow+language+affiliate_channel+
                       affiliate_provider+affiliate_channel+affiliate_provider+
                       first_affiliate_tracked+signup_app+
                       first_device_type+first_browser
                      ,data=X_train,
                     ntree=20, ### no of threes u will build
                     mtry=4,  ## number of variables u wills ele ## reg mtry =n/3 -- n^0.5
                     do.trace=T,
                     maxnodes=70)

pred_rf<-data.frame(predict(fit_rf,X_test,type="prob"))
head(pred_rf)
dim(pred_rf)

predictions_top5 <- as.vector(apply(pred_rf, 1, function(x) names(sort(x)[12:8])))

X_test$id<-df_test$id
X_test$id<-as.character(X_test$id)
ids <- NULL
for (i in 1:NROW(X_test)) {
  idx <- X_test$id[i]
  ids <- append(ids, rep(idx,5))
}
submission <- NULL
submission$id <- ids
submission$country <- predictions_top5

# generate submission file
submission <- data.frame(submission)
head(submission)
write.csv(submission, "Simple_RF.csv", quote=FALSE, row.names = FALSE)

