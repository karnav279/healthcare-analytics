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

##################### Loading Data ############################################################
### Change directory and excel sheet as per your saved diectory

data = read_excel("trainData_Clean.xlsx", sheet="completeData_Clean")
df = data.frame(data)
df1 = subset(df, select = -c(Date, Hospitalizations))
sum(is.na(df))

##########################   PCA   ##############################################################
p_comp = c(1:length(df1))

#PCA using covariance matrix
pca_cov = prcomp(df1, scale = FALSE)
var_explained_cov = round((pca_cov$sdev)^2/(sum((pca_cov$sdev)^2)),3)
#Scree plot of PCs using covariance matrix
fviz_eig(pca_cov, addlabels = TRUE) #From the scree plot, we finalize the PCA using covariance matrix
#eigen values and variance explained in covariance matrix
eig <- get_eig(pca_cov)
eig                     #from the eig values we consider 10 PCs

################ Building new data Frame for regression ##########################################################

df2 = data.frame(df$Hospitalizations, pca_cov$x[,1:10])
df3 = data.frame(colnames(df1), pca_cov$rotation)

################### Spliting Data set to training and test dataset ##################################

set.seed(1)
train = sample(1:nrow(df2),0.8*nrow(df2))
train_data= df2[train,]
test_data = df2[-train,]
test_y = test_data[,"df.Hospitalizations"]
train_y = train_data[,"df.Hospitalizations"]

####################### Multiple Linear regression #####################################################

linear = lm(df.Hospitalizations~., data = train_data)
summary(linear)

predict_train = predict(linear, newdata = train_data)
mean((predict_train-train_y)^2)

predict = predict(linear, newdata = test_data)
head(predict)
mean((predict-test_y)^2)

par(mfrow=c(2,2))
plot(linear)

vif(linear)

################################ linear regression on full data #####################################

glm.fit = glm(df.Hospitalizations~., data = df2)

############# Cross validation of linear ################################

set.seed(1)
cv_error = cv.glm(df2, glm.fit, K=10)
cv_error$delta

################################ Transforming for linear space ##################################
############### Logarithimic transformation ###################################
df2 = data.frame(df$Hospitalizations, pca_cov$x[,1:10])
for (i in 1:10)
{
  
  df2 = cbind(df2,log(abs(df2[,i+1])))
  colnames(df2)[which(names(df2) == "log(abs(df2[, i + 1]))")] <- paste("log","PC",i)
}

head(df2)
# Spliting Data set to training and test dataset
set.seed(1)
train = sample(1:nrow(df2),0.8*nrow(df2))
train_data= df2[train,]
test_data = df2[-train,]
test_y = test_data[,"df.Hospitalizations"]
train_y = train_data[,"df.Hospitalizations"]

#fitting linear model
lin =lm(df.Hospitalizations~., data = train_data)
predict_train = predict(lin, newdata = train_data)
mean((predict_train-train_y)^2)

#test Error
predict = predict(lin, newdata = test_data)
head(predict)
mean((predict-test_y)^2)
summary(lin)

#Diagonisis
par(mfrow=c(2,2))
plot(lin)

############## Polynomial tranformation (degree=2) ##############################
df2 = data.frame(df$Hospitalizations, pca_cov$x[,1:10])
for (i in 1:10)
{
  
  df2 = cbind(df2,I(df2[,i+1]^2))
  colnames(df2)[which(names(df2) == "df2[, i + 1]^2")] <- paste("sq","PC",i)
}

head(df2)
# Spliting Data set to training and test dataset
set.seed(1)
train = sample(1:nrow(df2),0.8*nrow(df2))
train_data= df2[train,]
test_data = df2[-train,]
test_y = test_data[,"df.Hospitalizations"]
train_y = train_data[,"df.Hospitalizations"]

#fitting linear model
lin =lm(df.Hospitalizations~., data = train_data)
predict_train = predict(lin, newdata = train_data)
mean((predict_train-train_y)^2)

#test Error
predict = predict(lin, newdata = test_data)
head(predict)
mean((predict-test_y)^2)
summary(lin)

#Diagonisis
par(mfrow=c(2,2))
plot(lin)


############## Polynomial tranformation (degree=3) ##############################

df2 = data.frame(df$Hospitalizations, pca_cov$x[,1:10])
for (i in 1:10)
{
  
  df2 = cbind(df2,I(df2[,i+1]^3))
  colnames(df2)[which(names(df2) == "df2[, i + 1]^3")] <- paste("cub","PC",i)
}

head(df2)
# Spliting Data set to training and test dataset
set.seed(1)
train = sample(1:nrow(df2),0.8*nrow(df2))
train_data= df2[train,]
test_data = df2[-train,]
test_y = test_data[,"df.Hospitalizations"]
train_y = train_data[,"df.Hospitalizations"]

#fitting linear model
lin =lm(df.Hospitalizations~., data = train_data)
predict_train = predict(lin, newdata = train_data)
mean((predict_train-train_y)^2)

#test Error
predict = predict(lin, newdata = test_data)
head(predict)
mean((predict-test_y)^2)
summary(lin)

#Diagonisis
par(mfrow=c(2,2))
plot(lin)


##############  Random Forest #######################################################

df2 = data.frame(df$Hospitalizations, pca_cov$x[,1:10])

set.seed(1)
train = sample(1:nrow(df2),0.8*nrow(df2))
train_data= df2[train,]
test_data = df2[-train,]
test_y = test_data[,"df.Hospitalizations"]
train_y = train_data[,"df.Hospitalizations"]

set.seed(1)
rf.df2 = randomForest(df.Hospitalizations~., data = train_data, mtry=3, importance = TRUE)
set.seed(1)
rf.df3 = randomForest(df.Hospitalizations~., data = train_data, mtry=5, importance = TRUE)
set.seed(1)
rf.df10 = randomForest(df.Hospitalizations~., data = train_data, mtry=10, importance = TRUE)

#Predicting train data
yhat.train.rf3 = predict(rf.df2, newdata = train_data)
yhat.train.rf5 = predict(rf.df3, newdata=train_data)
yhat.train.rf10 = predict(rf.df10, newdata=train_data)
#Training Mean Squared Error
mean((yhat.train.rf3-train_y)^2)
mean((yhat.train.rf5-train_y)^2)
mean((yhat.train.rf10-train_y)^2)

#Predicting test data
yhat.rf = predict(rf.df2, newdata=test_data)
yhat.rf.5 = predict(rf.df3, newdata=test_data)
yhat.rf.10 = predict(rf.df10, newdata=test_data)
#Test Mean Squared Error
mean((yhat.rf-test_y)^2)
mean((yhat.rf.5-test_y)^2)
mean((yhat.rf.10-test_y)^2)

#important PCs for the randomForest models
importance(rf.df2)
importance(rf.df3)
importance(rf.df10)

#randomForest summary
summary(rf.df2)
summary(rf.df3)
summary(rf.df10)

#Plotting importance of PCs

varImpPlot(rf.df2)
varImpPlot(rf.df3)
varImpPlot(rf.df10)

#Plotting MSE v/s ntree for various mtrys

error = function(x) {mean((x-test_y)^2)}
err_data_mtry = c()
err_data_ntree = c()
err_data_ntree1 = c()
err_data_ntree2 = c()

###### Varying ntree
j = 1
for (i in 5:200)
{rf_fit = randomForest(df.Hospitalizations~.,data = df2, subset = train,mtry = 3,ntree = i, importance = TRUE)
yhat_rf = predict(rf_fit,newdata = test_data)
err_data_ntree[j] = error(yhat_rf)
j = j+1}

j = 1
for (i in 5:200)
{rf_fit = randomForest(df.Hospitalizations~.,data = df2, subset = train,mtry = 5,ntree = i, importance = TRUE)
yhat_rf = predict(rf_fit,newdata = test_data)
err_data_ntree1[j] = error(yhat_rf)
j = j+1}

j = 1
for (i in 5:200)
{rf_fit = randomForest(df.Hospitalizations~.,data = df2, subset = train,mtry = 10,ntree = i, importance = TRUE)
yhat_rf = predict(rf_fit,newdata = test_data)
err_data_ntree2[j] = error(yhat_rf)
j = j+1}

plot(5:200,err_data_ntree,main ="ntree vs errors",xlab = "ntree", ylab ="Test MSEs",ylim = c(200,800))
lines(5:200,err_data_ntree,col ="blue")
lines(5:200,err_data_ntree1,main ="ntree vs errors",xlab = "ntree", ylab ="Test MSEs",col = "green")
lines(5:200,err_data_ntree2,main ="ntree vs errors",xlab = "ntree", ylab ="Test MSEs",col = "red")
legend("topright", legend=c("mtry=3", "mtry=5", "mtry=10"), col=c("blue", "green", "red"), lty=1, cex=1)



############## Support Vector Regression #######################################################

#Linear kernel
set.seed(1)
svm_model_l = svm(df.Hospitalizations~., data = train_data, kernel = 'linear', cost=0.001)
set.seed(9)
tune.out.l = tune(svm, df.Hospitalizations~., data = train_data, kernel="linear", ranges=list(cost=c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
summary(tune.out.l)
plot(tune.out.l)
#best cost for linear kernel = 0.001

#Radial kernel
set.seed(1)
svm_model_r = svm(df.Hospitalizations~., data = train_data, kernel = 'radial', cost=1, gamma=0.5)
set.seed(10)
tune.out.r = tune(svm, df.Hospitalizations~., data = train_data, kernel="radial", ranges=list(cost=c(0.1, 1, 10, 100, 1000), gamma=c(0.5, 1, 2, 3, 4)))
summary(tune.out.r)
plot(tune.out.r)
#best parameters for radial kernel: cost = 1, gamma = 0.5

#Polynomial
set.seed(1)
svm_model_p = svm(df.Hospitalizations~., data = train_data, kernel = 'polynomial', cost=0.1, degree=3)
set.seed(11)
tune.out.p = tune(svm, df.Hospitalizations~., data = train_data, kernel="polynomial", ranges=list(cost=c(0.1, 1, 10, 100, 1000), degree=c(1, 2, 3, 4, 5)))
summary(tune.out.p)
plot(tune.out.p)
#best parameters for radial kernel: cost = 0.1, degree = 3

#SVR training error

yhat.svm.train.l = predict(svm_model_l, newdata=train_data)
mean((yhat.svm.train.l-train_y)^2)

yhat.svm.train.r = predict(svm_model_r, newdata=train_data)
mean((yhat.svm.train.r-train_y)^2)

yhat.svm.train.p = predict(svm_model_p, newdata=train_data)
mean((yhat.svm.train.p-train_y)^2)

#SVR test error

yhat.svm.l = predict(svm_model_l, newdata=test_data)
mean((yhat.svm.l-test_y)^2)

yhat.svm.r = predict(svm_model_r, newdata=test_data)
mean((yhat.svm.r-test_y)^2)

yhat.svm.p = predict(svm_model_p, newdata=test_data)
mean((yhat.svm.p-test_y)^2)

