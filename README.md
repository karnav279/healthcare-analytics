# healthcare-analytics
Machine learning model in R Studio tp predict COVID-19 hospitalization in a county in Texas based on mobility and foot traffic data.

The objective of this project is to predict COVID-19 hospitalization numbers in Bryan/College Station area
based on mobility and foot trafficking data. Project was executed in two phases. Phase one involved
building three regression models using the training data and selecting the best model that has least validation
Mean Squared Error (MSE). The original training data consists of 189 predictors (Mobility, Foot Traffic,
Demographics, age & income per zip code) with Hospitalizations as response variable. The mobility data
provides the mobility changes in places of activity in Bryan/College Station with respect to a baseline
established prior to the start of the pandemic. The rest of the demographics data pertains to each of the 27
zip codes. The data was modified to take into account the infection from virus 10 days prior to the said date.
To achieve this, lags were added to mobility data which lead to 351 predictors.

Data preprocessing and feature reduction was done to eliminate blank fields in the data as well as to reduce
the predictor space from 351 predictors to 10 using Principal Component Analysis. These 10 Principal
Components were considered as they explained up to 97% variance of the original features. Performing
regression on these PCs is similar to performing regression on all the 351 original features. The processed
data was then split into training and validation set for feeding into different models for prediction of MSEs.
Out of multiple models, linear model, Support Vector regression and Random forest tree regression were
reported for test error because of their best performance.

All 10 principal components when fitted into linear model with training set and validation set, the training
and validation error came out to be around 1300 and 780 respectively. To improve the prediction,
transformed linear model was fitted, which resulted in around 1000 training and test error. For Support
Vector regression (SVR) 3 type of kernels were considered (Linear, Radial and Polynomial), important
parameters considered were cost, gamma, and degree of polynomial. The best SVR turned out to be radial
kernel and it predicted around 490 training error and 530 test error. Random forest showed best performance
among all the three models with test error around 300. The test error for random forest came out to be
minimum for number of predictors equal to 5. This model was then finalized for competition for prediction
of test data in phase two. After identifying the best model consisting of 10 PCs, an importance graph was
plotted to identify the best PCs that contribute the most towards the prediction for the number of
hospitalizations. PC1, PC6, and PC10 had the best percentage increase. These three PCs represent the influx
data for zip codes 77840 and 77845. The influx for 7-9 days prior to the hospitalization date into this area,
mattered the most in predicting hospitalizations for the said date. As per research in covid infections, the
infection time predicted by PCs seem logical.

For phase-II (competition) the test MSE for given test data turned out to be around 2700. The competition
model requires further improvement to minimize the test MSE. The test MSE reduces by 13.1% when the
best 3 PCs (Shown above) are considered instead of all the PCs. It can be observed that the test MSE can
be reduced by 33.8% considering only a certain portion of the training data that gives weightage to higher
response values to train the model. One can reduce the test MSE by 29.48% when above methods of
reduction are combined.
