# Libraries -------------------------------------------------------
library (ISLR) #data sets in the book

library(skimr) #for dataframe basic statistis
library(ggplot2) 
library(dplyr)
library(caret) #needed for findcorrelation 

# Load data and analyze-------------------------------------------------------
View(Auto)
skim(Auto)

set.seed(1)

#Train and test data sets
#train=sample(1:392 ,196)
#lm.fit =lm(mpg~horsepower ,data=Auto ,subset =train )
#mean((Auto$mpg -predict(lm.fit ,Auto))[-train ]^2)
trainIndex = createDataPartition(Auto$mpg,
                                 p=0.50, list=FALSE,times=1)
train =as.data.frame(Auto[trainIndex,])
test = as.data.frame(Auto[-trainIndex,])
lm.fit =lm(mpg~horsepower, train )
mean((train$mpg -predict(lm.fit ,train))^2)


#Cross Validation
library("boot")
#LOOCV for polinomial regression 
glm.fit=glm(mpg~poly(horsepower,5) ,data=Auto)

cv.err =cv.glm(Auto ,glm.fit) 
cv.err$delta

#10-K-fold validation for polinomical regresion
set.seed (17)
cv.error.10= rep(0,10)
for (i in 1:10) {
    glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
    cv.error.10[i]=cv.glm(Auto ,glm.fit ,K=10)$delta [1]
                }
cv.error.10


#BOOTSTRAPPING
boot.fn=function (data ,index )
  + return (coef(lm(mpg???horsepower ,data=data ,subset =index)))

boot.fn(Auto,1:392)

boot(Auto ,boot.fn ,1000)
#
ggplot(Weekly, aes(y=Volume, x=Year, group=Year)) +
  geom_boxplot() + 
  labs(x = "Year", y ="Volume", title ="2001-2005 shares transactions")

#
ggplot(Weekly, aes(y=Today, x=Year, group=Year)) +
  geom_boxplot() + 
  labs(x = "Year", y ="Volume", title ="2001-2005 shares transactions")

#
ggplot(Weekly, aes(Today)) +
geom_histogram(binwidth = 1) + 
facet_wrap(Year ~ .) +
labs(x = "Year", y ="Count", title ="1990-2010 shares transactions")

# Logistic regression-------------------------------------------------------

#Split data
library(caret) #needed for findcorrelation 
trainIndex = createDataPartition(Weekly$Direction,
                                 p=0.7, list=FALSE,times=1)
train = Smarket[trainIndex,]
test = Smarket[-trainIndex,]

#Fit into a model
glm.fits=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume ,
             data=train ,family =binomial )
summary (glm.fits) #P-values are large, so the performance of the model doesn't look good

glm.probs =predict(glm.fits,test, type ="response")
length(glm.probs)

#Turn probabilities into classes
glm.pred=rep ("Down" ,length(glm.probs))
glm.pred[glm.probs >.5]=" Up"
#Confusion matrix through contigency table
table(PRED = glm.pred,REAL = test$Direction)
