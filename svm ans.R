library(kernlab)
library(caret)
library(plyr)
library(ggplot2)
library(psych)
library(e1071)

sal_train <- read.csv(file.choose())
View(sal_train)
str(sal_train)

sal_train[sapply(sal_train, is.character)] <- lapply(sal_train[sapply(sal_train, is.character)],as.factor)
str(sal_train)

sal_train$educationno <- as.factor(sal_train$educationno)
class(sal_train)

sal_test <- read.csv(file.choose())
View(sal_test)
str(sal_test)

sal_test[sapply(sal_test, is.character)] <- lapply(sal_test[sapply(sal_test, is.character)],as.factor)
str(sal_train)

sal_test$educationno <- as.factor(sal_test$educationno)
class(sal_test)
ggplot(data = sal_train,aes(x=sal_train$Salary,y=sal_train$age,fill = sal_train$Salary))+geom_boxplot()+ ggtitle("boxplot")
plot(sal_train$workclass,sal_train$Salary)

model1<-ksvm(sal_train$Salary~., 
             data= sal_train, kernel = "vanilladot")
model1

salary_prediction <- predict(model1, sal_test)
table(salary_prediction,sal_test$Salary)

agreement <- salary_prediction ==sal_test$Salary
table(agreement)
prop.table(table(agreement))

model_rfdot<-ksvm(sal_train$Salary~.,data= sal_train, kernel = "rbfdot")
model_rfdot
pre_rfdot<- predict(model_rfdot,newdata=sal_test)
mean(pre_rfdot==sal_test$Salary)



###########forestdata#########
library(kernlab)
library(caret)
library(plyr)
forest <- read.csv(file.choose())
View(forest)
class(forest)
str(forest)

forest[sapply(forest, is.character)] <- lapply(forest[sapply(forest, is.character)],as.factor)
str(forest)
summary(forest)

hist(forest$area)
forest1 <- mutate(forest, y = log(area+1))
hist(forest1$area)
hist(forest1$y)

normalize <- function(x){return((x-min(x))/(max(x)-min(x)))
}
forest$temp <- normalize(forest$temp)
forest$RH <- normalize(forest$RH)
forest$wind <- normalize(forest$wind)
forest$rain <- normalize(forest$rain)

attach(forest)
set.seed(222)
ind <- sample(2,nrow(forest),replace = TRUE,prob = c(0.7,0.3))
forest_train <- forest[ind==1,]
forest_test <- forest[ind==1,]
View(forest_test)

model1<-ksvm(size_category~temp+rain+wind+RH,
             data= forest_train, kernel = "vanilladot")
model1

pred_area <- predict(model1, forest_test)
table(pred_area,forest_test$size_category)

agreement <- pred_area == forest_test$size_category
table(agreement)
prop.table(table(agreement))


model_rfdot<-ksvm(size_category~temp+rain+wind+RH,
             data= forest_train, kernel = "rbfdot")
model_rfdot

pred_rfdot <- predict(model_rfdot,newdata = forest_test)
table(pred_rfdot,forest_test$size_category)

agreement <- pred_rfdot == forest_test$size_category
table(agreement)
prop.table(table(agreement))
