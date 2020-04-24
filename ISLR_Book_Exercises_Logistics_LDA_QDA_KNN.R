# Libraries -------------------------------------------------------
library (ISLR) #data sets in the book

library(skimr) #for dataframe basic statistis
library(ggplot2) 
library(dplyr)
library(caret) #needed for findcorrelation 

# Load data and analyze-------------------------------------------------------
View(Smarket)

summary(Smarket)
#Analyze outcome from the linerar regression
plot(Smarket$Today)

correlationMatrix =cor(Smarket [,-9])
library(corrplot)
corrplot(correlationMatrix, method="circle")
findCorrelation(correlationMatrix, cutoff=0.3,exact =TRUE,verbose=TRUE)
#only significant correlation between Volume and Year
ggplot(Smarket$Year, Smarket$Volume)

ggplot(Smarket, aes(y=Volume, x=Year, group=Year)) +
  geom_boxplot() + 
  labs(x = "Year", y ="Volume", title ="2001-2005 shares transactions")


# Logistic regression-------------------------------------------------------

#Split data
trainIndex = createDataPartition(Smarket$Direction,
                                 p=0.7, list=FALSE,times=1)
train = Smarket[trainIndex,]
test = Smarket[-trainIndex,]

#Fit into a model
glm.fits=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume ,
             data=train ,family =binomial )
summary (glm.fits) #P-values are large, so the performance of the model doesn't look good

#Predict probabilities
glm.probs =predict(glm.fits,test, type ="response")
length(glm.probs)

#Turn probabilities into classes
glm.pred=rep ("Down" ,length(glm.probs))
glm.pred[glm.probs >.5]=" Up"
#Confusion matrix through contigency table
table(glm.pred,test$Direction)
#The diagonal elements of the confusion matrix indicate correct predictions,
#while the off-diagonals represent incorrect predictions
#Bad results --> Past perofrmance won't predict future performance



# LDA (Linear Discriminant Analysis)-------------------------------------------------------
library(MASS)
lda.fit=lda(Direction~Lag1+Lag2 ,data=train)

lda.fit 
plot(lda.fit)
#Predict over test data
lda.pred=predict (lda.fit , test)

#Confusion matrix through contigency table
table(PRED = lda.pred$class ,TEST= test$Direction)


# QDA (Linear Discriminant Analysis)-------------------------------------------------------
qda.fit=qda(Direction~Lag1+Lag2 ,data=train)

qda.fit 
#Predict over test data
qda.pred=predict (qda.fit , test)

#Confusion matrix through contigency table
table(PRED = qda.pred$class ,TEST= test$Direction)


# KNN - Example for trading data-------------------------------------------------------
library (class)
train.X=cbind(train$Lag1,train$Lag2)
test.X=cbind(test$Lag1,test$Lag2)
train.Direction =train$Direction

#Create model and predict
set.seed (23) # ensure reproducebility
knn.pred=knn (train.X, 
              test.X,
              train.Direction,
              k=3)

#Confusion matrix through contigency table
table(PRED = knn.pred,TEST= test$Direction)

#Try changing the parameter K, the number of neighbours to be included

# KNN - Example for insurance data (normalization needed)-------------------------------------------------------
View(Caravan)
dim(Caravan)


#Split data
trainIndex = createDataPartition(Caravan$Purchase,
                                 p=0.7, list=FALSE,times=1)
train = Caravan[trainIndex,]
test = Caravan[-trainIndex,]

# AZEILPL (column 81) removed, due to NA
train = train[,-81]
test = test[,-81]

anyNA(test)
anyNA(train)

#Due to the fact that variables have different scales, we need to normalize upfront
standardized.train=scale(train[,-85]) #standardize in N(0,1) except the variable to predict
standardized.test=scale(test[,-85]) #standardize in N(0,1) except the variable to predict


train.X=standardized.train[,1-84]
test.X=standardized.test[,1-84] 
train.Y =train$Purchase
test.Y =test$Purchase

#QC
anyNA(train.X)
anyNA(train.Y)
anyNA(test.X)
anyNA(test.Y)

#seek errors
#ok = complete.cases(test.X)
#sum(!ok)
#test.X[ok,]

#Create model and predict
set.seed (23) # ensure reproducebility
knn.pred=knn (train.X,test.X,train.Y,k=5)

#Confusion matrix through contigency table
confusion.mat = table(PRED = knn.pred,TEST= test.Y)
confusion.mat

#Tasa de acierto de predicci?n de compra
confusion.mat[2,2] / (confusion.mat[2,1]+confusion.mat[2,2]) 

