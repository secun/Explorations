 library(readxl)  # for reading the xls files
setwd("C:/Trash") # move to right directory

Sage_NPS<- read_excel("NPS_sage.xlsx", sheet = 1)
Sage_NPS<- Sage_NPS[,-1] # remove first column

#load  : Caret and plm are useful for PCA, PLS et others
install.packages("plm")
install.packages("caret")
install.packages("plsdepot")

library(caret)
# Train PLS model in caret
mod1 <- train(NPS ~ ., data = Sage_NPS,
              method = "pls",
              trcontrol = trainControl("repeatedcv", index = myfolds, selectionFunction = "oneSE"),
              metric = "Rsquared",
              preProc = c("zv","center","scale"))

# Check CV profile
plot(mod1)

# Train PLS model in plsdepot
library(plsdepot)
NPS_predict<- Sage_NPS[, -27]
pls1 = plsreg1(NPS_predict, Sage_NPS[, 27, drop = FALSE], comps = 10) 
# 10 latent variables