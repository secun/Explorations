 # Libraries -------------------------------------------------------
library (ISLR) #data sets in the book

library(skimr) #for dataframe basic statistis
library(ggplot2) 
library(dplyr)
library(data.table)
library(caret)

# Single linear regression-------------------------------------------------------
View(Auto)
lm.fit =lm(mpg~horsepower ,data=Auto )

summary(lm.fit)
#Analyze outcome from the linerar regression
plot(lm.fit)

#Plot response and predictor as well as LSS regression line
ggplot(Auto, aes(y=mpg, x=horsepower)) +
  geom_point () + 
  labs(x = "horsepower", y ="miles per galon", title ="Auto data")+
 geom_abline(
    aes(intercept=lm.fit$coefficients[1], slope=lm.fit$coefficients[2]),
    color= "red"
    )
#Predict for a horsepower of 98
predict(lm.fit,data.frame(horsepower=c(98)),interval="confidence")

# Multiple linear regression-------------------------------------------------------
View(Auto)

pairs(~.,data=Auto,
      main="Simple Scatterplot Matrix")

correlationMatrix =cor(Auto[,c(1:8)])
library(corrplot)
corrplot(correlationMatrix, method="circle")
findCorrelation(correlationMatrix, cutoff=0.6,exact =TRUE,verbose=TRUE)


lm.fit =lm(mpg~.-name ,data=Auto )

summary(lm.fit)
#Analyze outcome from the linerar regression
plot(lm.fit)
