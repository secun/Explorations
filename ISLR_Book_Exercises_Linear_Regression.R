   # Libraries -------------------------------------------------------
library (ISLR) #data sets in the book
library (MASS) #data sets from R as examples

library (car)

library(skimr) #for dataframe basic statistis
library(ggplot2) 
library(dplyr)
library(data.table)
library(caret)

# Single linear regression-------------------------------------------------------
View(Boston)
lm.fit =lm(medv~lstat ,data=Boston )

summary(lm.fit)
#Analyze outcome from the linerar regression
plot(lm.fit)

#Plot data against the single linear regression
ggplot(Boston, aes(y=medv, x=lstat)) +
  geom_point () + 
  labs(x = "lstat", y ="medv", title ="XXXX")+
  geom_abline(
    aes(intercept=lm.fit$coefficients[1], slope=lm.fit$coefficients[2]),
    color= "red"
    )

#Plot predictions against residuals
ggplot(lm.fit, aes(x=lm.fit$model$medv, y=residuals(lm.fit))) +
  geom_point () +
  labs(x = "medv", y ="Residuals", title ="XXXX")
#CONCLUSION: There seems to be a trend in the residuals

#Include in the plot the regression for the residuals
lm.fit.res =lm(residuals(lm.fit)~lm.fit$model$medv )
ggplot(lm.fit, aes(x=lm.fit$model$medv, y=residuals(lm.fit))) +
  geom_point () +
  labs(x = "medv", y ="Residuals", title ="XXXX")+
geom_abline(
  aes(intercept=lm.fit.res$coefficients[1], slope=lm.fit.res$coefficients[2]),
  color= "red"
)
#CONCLUSION: Residuals can be plotted against the prediction


#Plot predictions against studentized residuals
ggplot(lm.fit, aes(y=predict(lm.fit), x=rstudent(lm.fit))) +
  geom_point () 

#Plot leveraged statistics
plot(hatvalues (lm.fit )) 
which.max (hatvalues (lm.fit))

Boston$medv[375]
Boston$lstat[375]

# Multiple linear regression-------------------------------------------------------
View(Boston)
lm.fit =lm(medv~. ,data=Boston )

summary(lm.fit)


vif(lm.fit) #Larger values for one predictor means that correlation is present
#remove tax
lm.fit =lm(medv~.-tax ,data=Boston )
vif(lm.fit)
#remove tax & nox
lm.fit =lm(medv~.-tax-nox ,data=Boston )
vif(lm.fit)
summary(lm.fit)


# Non-linear transformation of predictors-------------------------------------------------------
View(Boston)
lm.fit2=lm(medv~lstat +I(lstat ^2), ,data=Boston )
summary(lm.fit2)
plot(lm.fit2)

#Compare linear model vs transformation of predictors
anova(lm.fit ,lm.fit2)
ggplot(lm.fit, aes(x=lm.fit$model$medv, y=residuals(lm.fit))) +
  geom_point () +
  labs(x = "medv", y ="Residuals", title ="XXXX")

ggplot(lm.fit2, aes(x=lm.fit2$model$medv, y=residuals(lm.fit2))) +
  geom_point () +
  labs(x = "medv", y ="Residuals", title ="XXXX")

# Others- Identify important features -------------------------------------------------------
# prepare training scheme
control <- trainControl(method="cv", number=10)
# train the model
model <- train(medv~., data=Boston, method="treebag", preProcess="scale", trControl=control)
# estimate variable importance
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)

# Create model for most important features -------------------------------------------------------
lm.fit3=lm(medv~lstat+rm+nox+crim+dis+ptratio  ,data=Boston )
summary(lm.fit3)
plot(lm.fit3)
#Eliminamos variables n? 372,373,369 de datos
Boston.clean = Boston [-(372:373),]
Boston.clean = Boston.clean [-369,]
Boston.clean = Boston.clean [-(370:371),]
Boston.clean = Boston.clean [-366,]
Boston.clean = Boston.clean [-(368:370),]

lm.fit3=lm(medv~lstat+rm+nox+crim+dis+ptratio  ,data=Boston.clean )

summary(lm.fit3)
plot(lm.fit3)
    