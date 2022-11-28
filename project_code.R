##################### Libraries needed ############################################################

library(randomForest)
library(ISLR)
library(tree)
library(leaps)
library(glmnet)
library(class)
library(readxl)
library(ggplot2)
library(factoextra)
library(car)
library(e1071)

##################### Loading Train Data ############################################################
### Change directory and excel sheet as per your saved diectory

data_p1 = read_excel("trainData_Clean.xlsx", sheet="completeData_Clean")
df_p1 = data.frame(data_p1)
df_train = subset(df_p1, select = -c(Date, Hospitalizations))
sum(is.na(df_p1))

##########################   PCA - Train   ##############################################################
p_comp_train = c(1:length(df_train))

#PCA using covariance matrix
pca_cov_train = prcomp(df_train, scale = FALSE)
var_explained_cov = round((pca_cov_train$sdev)^2/(sum((pca_cov_train$sdev)^2)),3)
#Scree plot of PCs using covariance matrix
fviz_eig(pca_cov_train, addlabels = TRUE) #From the scree plot, we finalize the PCA using covariance matrix
#eigen values and variance explained in covariance matrix
eig_train <- get_eig(pca_cov_train)
eig_train                     #from the eig values we consider 10 PCs

##################### Loading Test Data ############################################################
### Change directory and excel sheet as per your saved diectory

data_p2 = read_excel("testData.xlsx", sheet="Clean_Data")
df_p2 = data.frame(data_p2)
df_test = subset(df_p2, select = -c(Date, Hospitalizations))
sum(is.na(df_p2))

##########################   PCA - Test  ##############################################################
p_comp_test = c(1:length(df_test))

#PCA using covariance matrix
pca_cov_test = prcomp(df_test, scale = FALSE)
var_exp_cov = round((pca_cov_test$sdev)^2/(sum((pca_cov_test$sdev)^2)),3)
#Scree plot of PCs using covariance matrix
fviz_eig(pca_cov_test, addlabels = TRUE) #From the scree plot, we finalize the PCA using covariance matrix
#eigen values and variance explained in covariance matrix
eig_test <- get_eig(pca_cov_test)
eig_test                     #from the eig values we consider 10 PCs

################ Building new data Frame for regression ##########################################################

dftrain = data.frame(df_p1$Hospitalizations, pca_cov_train$x[,1:10])
dftest = data.frame(df_p2$Hospitalizations, pca_cov_test$x[,1:10])

y_test = dftest[,"df_p2.Hospitalizations"]
y_train = dftrain[,"df_p1.Hospitalizations"]

#############################  Random Forest #######################################################

set.seed(1)
rf.df2.p1 = randomForest(df_p1.Hospitalizations~., data = dftrain, mtry=3, importance = TRUE)
set.seed(1)
rf.df3.p1 = randomForest(df_p1.Hospitalizations~., data = dftrain, mtry=5, importance = TRUE)
set.seed(1)
rf.df10.p1 = randomForest(df_p1.Hospitalizations~., data = dftrain, mtry=10, importance = TRUE)

yhat.train.3 = predict(rf.df2.p1, newdata = dftrain)
yhat.train.5 = predict(rf.df3.p1, newdata = dftrain)
yhat.train.10 = predict(rf.df10.p1, newdata = dftrain)

mean((yhat.train.3-y_train)^2)
mean((yhat.train.5-y_train)^2)
mean((yhat.train.10-y_train)^2)

yhat.test.3 = predict(rf.df2.p1, newdata = dftest)
yhat.test.5 = predict(rf.df3.p1, newdata = dftest)
yhat.test.10 = predict(rf.df10.p1, newdata = dftest)

mean((yhat.test.3-y_test)^2)
mean((yhat.test.5-y_test)^2)
mean((yhat.test.10-y_test)^2)

############################  Improvement #######################################################

# For first improvement, replace line #57 and #58 with these lines: 
  #dftrain = data.frame(df_p1$Hospitalizations, pca_cov_train$x[,1:10])
  #dftest = data.frame(df_p2$Hospitalizations, pca_cov_test$x[,1:10])
  #change mtry in line#66, 68 & 70 to 1, 2 and 3 instead of 3, 5 and 10.

# For second improvement, add this code to the original code, df_p1 = data.frame(data_p1[116:nrow(df_p1),]) below line# 18
