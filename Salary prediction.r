setwd("c://R")
library(ggplot2)
library(dplyr)
library(corrplot)
library(e1071)
library(caTools)
library(randomForest)
library(rpart)
library(rpart.plot)



df<-read.csv("salary.csv")
head(df)
df <- select(df,-c(X))
head(df)
summary(df)
dim(df)
str(df)
any(is.na(df))
df.fact <- sapply(df, is.factor)
colnames(df[,df.fact])
distinct(df,Workclass,education,occupation,race,native.country)


df$Workclass = as.numeric(df$Workclass)
df$education = as.numeric(df$education)
df$marital.status = as.numeric(df$marital.status)
df$occupation = as.numeric(df$occupation)
df$relationship = as.numeric(df$relationship)
df$race = as.numeric(df$race)
df$sex = as.numeric(df$sex)
df$native.country = as.numeric(df$native.country)
head(df)

# Correlation
df_cor=cor(df)
corrplot(df_cor,method="color")
df_cor


ggplot(df,aes(x=age)) + geom_histogram(aes(fill=..count..),color="skyblue",binwidth=10)
ggplot(df,aes(x=fnlwgt)) + geom_histogram(aes(fill=..count..),color="skyblue",binwidth=25)
ggplot(df,aes(x=Workclass)) + geom_histogram(aes(fill=..count..),color="skyblue",binwidth=10)
ggplot(df,aes(x=education)) + geom_histogram(aes(fill=..count..),color="skyblue",binwidth=10)
ggplot(df,aes(x=education.num)) + geom_histogram(aes(fill=..count..),color="skyblue",binwidth=10)
ggplot(df,aes(x=relationship)) + geom_histogram(aes(fill=..count..),color="skyblue",binwidth=10)
ggplot(df,aes(x=capital.gain)) + geom_histogram(aes(fill=..count..),color="skyblue",binwidth=25)
ggplot(df,aes(x=capital.loss)) + geom_histogram(aes(fill=..count..),color="skyblue",binwidth=25)
ggplot(df,aes(x=hours.per.week)) + geom_histogram(aes(fill=..count..),color="skyblue",binwidth=10)
ggplot(df,aes(x=native.country)) + geom_histogram(aes(fill=..count..),color="skyblue",binwidth=10)
ggplot(df,aes(x=occupation)) + geom_histogram(aes(fill=..count..),color="skyblue",binwidth=10)

#it will convert Income(integer) into factor
df$Income = factor(df$Income)
str(df)


ggplot(df, aes(age)) + geom_bar(aes(fill = Income),position="dodge") 
ggplot(df, aes(Workclass)) + geom_bar(aes(fill = Income),position="dodge")
ggplot(df, aes(factor(fnlwgt))) + geom_bar(aes(fill = Income),position="dodge")
ggplot(df, aes(education)) + geom_bar(aes(fill = Income),position="dodge")
ggplot(df, aes(education.num)) + geom_bar(aes(fill = Income),position="dodge")
ggplot(df, aes(marital.status)) + geom_bar(aes(fill = Income),position="dodge")
ggplot(df, aes(occupation)) + geom_bar(aes(fill = Income),position="dodge")
ggplot(df, aes(relationship)) + geom_bar(aes(fill = Income),position="dodge")
ggplot(df, aes(sex)) + geom_bar(aes(fill = Income),position="dodge")
ggplot(df, aes(factor(capital.gain))) + geom_bar(aes(fill = Income),position="dodge")
ggplot(df, aes(factor(capital.loss))) + geom_bar(aes(fill = Income),position="dodge")
ggplot(df, aes(hours.per.week)) + geom_bar(aes(fill = Income),position="dodge")
ggplot(df, aes(native.country)) + geom_bar(aes(fill = Income),position="dodge")



#skewness
skewness(df$age)
skewness(df$fnlwgt)
skewness(df$education.num)
skewness(df$hours.per.week)



df$fnlwgt = sqrt(df$fnlwgt)
skewness(df$fnlwgt) %>% 



set.seed(101)
sample <- sample.split(df,SplitRatio = 0.70)
train = subset(df,sample ==TRUE)
test = subset(df,sample ==FALSE)

m1 = glm(Income ~ . , family = binomial(link='logit'),data=train)
summary(m1)

probabilities <- predict(m1,newdata=test,type='response')
results <- ifelse(probabilities > 0.5,1,0)
confuse.matrix <- table(test$Income,results)
confuse.matrix

tn <- confuse.matrix[1]
tn

tp <- confuse.matrix[4]
tp

fn <- confuse.matrix[2]
fn

fp <- confuse.matrix[3]
fp

accuracy <- (tp+tn)/(tp+tn+fp+fn)
accuracy

precision <- (tp)/(tp+fp)
precision

recall <- (tp)/(tp+fn)
recall


#decision tree
tree <- rpart(Income ~ . , method='class', data=train)
prp(tree)

results <- predict(tree,newdata=test,type='class')
confuse.matrix1 <- table(test$Income,results)
confuse.matrix1

tn <- confuse.matrix1[1]
tn

tp <- confuse.matrix1[4]
tp

fp <- confuse.matrix1[3]
fp

fn <- confuse.matrix1[2]
fn

accuracy <- (tp+tn)/(tp+tn+fp+fn)
accuracy

precision <- (tp)/(tp+fp)
precision

recall <- (tp)/(tp+fn)
recall


#pruning
tree2 <- rpart(Income ~ . , method='class', data=train,control=rpart.control(maxdepth = 4))
prp(tree2)



#Random forest
m2 <- randomForest(Income ~ .,data=train)
print(m2)


results <- predict(m2,newdata=test,type='class')
confuse.matrix2 <- table(test$Income,results)
print(confuse.matrix2)
tn <- confuse.matrix2[1]
tn

tp <- confuse.matrix2[4]
tp

fp <- confuse.matrix2[3]
fp

fn <- confuse.matrix2[2]
fn

accuracy <- (tp+tn)/(tp+tn+fp+fn)
accuracy

precision <- (tp)/(tp+fp)
precision


recall <- (tp)/(tp+fn)
recall


#SVM
prediction <- function(m){
  
  results <- predict(m,newdata=test,type='class')
  confusion.matrix <- table(test$Income,results)
  print(confusion.matrix)
  tp=confusion.matrix[4]
  tp
  tn=confusion.matrix[1]
  tn
  fp=confusion.matrix[3]
  fp
  fn=confusion.matrix[2]
  fn
  accuracy <- (tp+tn)/(tp+tn+fp+fn)
  print(paste("accuracy:",accuracy))
  precision <- (tp)/(tp+fp)
  print(paste("precision:",precision))
  recall <- (tp)/(tp+fn)
  print(paste("recall:",recall))
  
  
}


#Linear
m1<- svm(Income ~ .,
              data=train,
              type="C-classification",
              kernel = "linear")

prediction(m1)


#soft margin
m2 <- svm(Income ~ .,
              data=train,
              type="C-classification",
              kernel = "linear",
              cost=10)

prediction(m2)

#polynomial 
m3 <- svm(Income ~ .,
              data=train,
              type="C-classification",
              kernel = "polynomial")

prediction(m3)


#Radial
m4 <- svm(Income ~ .,
              data=train,
              type="C-classification",
              kernel = "radial")

prediction(m4)

