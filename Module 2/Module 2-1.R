library(e1071)
set.seed(1)

extractFeatures <- function(data) {
  features <- c("Pclass",
                "Age",
                "Sex",
                "Parch",
                "SibSp",
                "Fare",
                "Embarked")
  fea <- data[,features]
  fea$Age[is.na(fea$Age)] <- -1
  fea$Fare[is.na(fea$Fare)] <- median(fea$Fare, na.rm=TRUE)
  fea$Embarked[fea$Embarked==""] = "S"
  fea$Sex <- as.factor(fea$Sex)
  fea$Embarked <- as.factor(fea$Embarked)
  return(fea)
} 

train<-read.csv("train.csv",header=TRUE,sep=",",stringsAsFactors=FALSE)
train<-within(train,{
  Survived<-as.factor(Survived)})
test<-read.csv("test.csv",header=TRUE,sep=",",stringsAsFactors=FALSE)
result<-read.csv("gender_submission.csv",header=TRUE,sep=",",stringsAsFactors=FALSE)

result<-within(result,{
  Survived<-as.factor(Survived)})

fit.svm<-svm(train$Survived~.,data=extractFeatures(train))

svm.pred<-predict(fit.svm,extractFeatures(test))

table(svm.pred,result$Survived,dnn=c("Pred","Actul"))

#mean(svm.pred == result$Survived)  #0.9617225

#submission
submission.svm <- data.frame(PassengerId = test$PassengerId)
submission.svm$Survived <- svm.pred
write.csv(submission.svm, file = "2_svm_submission.csv", row.names=FALSE)

