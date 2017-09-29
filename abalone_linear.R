#Different model tried, here is the linear regression tried. 
rm(list = ls(all=TRUE))
setwd("C:/Users/RaghuTeja/Desktop/intern/")
mydata <- read.table("abalone.txt", header=FALSE, sep=",")
mydata$V9<-mydata$V9+1.5  #adding 1.5 to the entire coloumn(V9) as mentioned in description for age of the abalone
head(mydata)
tail(mydata)
str(mydata)
install.packages("dummies")
library(dummies)
dummy1<-dummy(mydata$V1)
mydata$V1<-NULL
class(dummy1)
dummy1<-as.data.frame(dummy1)
mydata<-cbind(dummy1,mydata)
sum(is.na(mydata))
summary(mydata)
#mydata$V9<-as.factor(mydata$V9)
#class(mydata$V9)

#mydata$V1M<-NULL
target<-mydata$V9
mydata$V9<-NULL


dim(mydata)
mydata<-scale(mydata[,1:10], center = TRUE, scale = TRUE)
class(mydata)
mydata<-as.data.frame(mydata)
mydata<-cbind(mydata,target)
str(mydata)
set.seed(100)

# the "sample()" function helps us to randomly sample 70% of the row indices of the dataset

train_rows <- sample(x = 1:nrow(mydata), size = 0.7*nrow(mydata))

# We use the above indices to subset the train and test sets from the data

train_data <- mydata[train_rows, ]

test_data <- mydata[-train_rows, ]

model1 <- lm(formula = target ~., data = train_data)
#levels(target)
summary(model1)
par(mfrow = c(2,2)) 
plot(model1)

#Step AIC
install.packages("MASS")
library(MASS)

# Here we use the "backward" direction as model_basic used all the variables to build the regression model

model_aic <- stepAIC(model1, direction = "both")

summary(model_aic)

par(mfrow = c(2,2))

plot(model_aic)

#VIF

install.packages("car")
library(car)

vif(model1)

vif(model_aic)

library(corrplot)
corrplot(cor(mydata))
cor(mydata$V5,mydata$V6)
mydata$V5<-NULL
train_data$V5<-NULL
test_data$V5<-NULL
str(train_data)
#Without V5
model2 <- lm(formula = target ~., data = train_data)
summary(model2)
#pred<-predict(model2,test_data)
model_aic_2 <- stepAIC(model2, direction = "both")

summary(model_aic)

par(mfrow = c(2,2))

plot(model_aic)

vif(model_aic_2)

#class(pred)
model3<-lm(formula = target ~., data=train_data)
summary(model3)

mae <- function(actual, predicted){
  
  error <- actual - predicted
  
  mean(abs(error))
  
}

mse <- function(actual, predicted){
  
  error <- actual - predicted
  
  mean(error^2)
  
}
rmse <- function(actual, predicted){
  
  error <- actual - predicted
  
  sqrt(mean(error^2))
  
}
preds <- predict(model1, test_data[, !(names(test_data) %in% c("target"))])

mae(test_data[, "target"], preds)

mse(test_data[, "target"], preds)

rmse(test_data[, "target"], preds)


table(mydata$V1M)
x=data.frame(cbind(preds,test_data$V9))
x

