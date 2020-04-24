 
library(tidyverse) #data manipulation
library(caret) #ML workflow
library(rpart) #decision tree models


#Create a model based upon existing data
#data variables
dat_tree<- dat_UB_by_order [,-(1:2)]

#The outcome variable is continuous, so 
#instead of decision tree shoudl be a regression tree
#split data and create train/test data
set.seed(123)
training.samples <- dat_tree$discount. %>% createDataPartition(p=0.8, list=FALSE) 
train.data <- dat_tree [training.samples,]
test.data <- dat_tree [-training.samples,]

#Create model for train data
model <- train( discount. ~., data=train.data, method ="rpart", 
                trControl =trainControl("cv",number=10),
                tuneLength=10
              )

plot (model)
model$bestTune

par(xpd=NA)
plot(model$finalModel)
text(model$finalModel,digits=3)
print(model,digits=3)

#Test model for test data
predictions <- model %>% predict (test.data)
head(predictions)
RMSE(predictions,test.data$discount.)

  
  
#Make prediction for one single data
newdata <- data.frame(Sepal.Length=6.5, Sepal.Width=3.0,
                      Petal.Length=5.2,Petal.Width=2.0)

model %>% predict(newdata, "class")


