train = read.csv("train.csv")
test = read.csv("test.csv")

str(train)
str(test)
library(DMwR)

trainImputeN = train

# Central imputation is done on the train dataset to fit the regression and find statistically significant variables
trainCentral = centralImputation(train)

lm.fit = lm(SalePrice~., data = trainCentral)
summary(lm.fit)
par(mfrow=c(2,2))
plot(lm.fit)
# Select statistically significant variables and ignore others (ignoring p-value > 0.05)

# Dummyfying few statistically significant variables
KitchenQualVars = dummy(trainImputeN$KitchenQual)
trainImputeN = cbind(trainImputeN,KitchenQualVars)
trainImputeN  = subset(trainImputeN, select=-c(KitchenQual))

GarageQualVars = dummy(trainImputeN$GarageQual)
trainImputeN = cbind(trainImputeN,GarageQualVars)
trainImputeN  = subset(trainImputeN, select=-c(GarageQual))

# Also observed heteroscedasticity in the residual plots. Hence taking SalePrice^1/4 as the newly transformed response for predictors
# Increasing the power of LotArea and YearBuilt and run the regression few times to identify that they are all statistically significant.
# With these observations, the new regression fit would be as below - 

lm.fit = lm((SalePrice^(1/4))~MSZoning+LotArea+GarageArea+LandSlope+Neighborhood+Condition1+YearBuilt+OverallQual+YearBuilt*OverallCond+YearRemodAdd+RoofMatl+BsmtFinSF1+BsmtFinSF2+BsmtUnfSF+X1stFlrSF+X2ndFlrSF+KitchenQualEx+KitchenQualFa+KitchenQualGd+ScreenPorch+PoolArea+GarageQualFa+GarageQualGd+GarageQualPo+GarageQualTA+WoodDeckSF+SaleCondition+I(LotArea^2)+I(LotArea^3)+I(LotArea^4)+I(LotArea^5)+I(LotArea^6)+I(YearBuilt^2)+I(YearBuilt^3)+I(YearBuilt^4), data = trainImputeN)
summary(lm.fit)
par(mfrow=c(2,2))
plot(lm.fit)

# Removing the outliers and high leverage points
trainImputeN = trainImputeN[-c(314,707,524,89,633),]

lm.fit = lm((SalePrice^(1/4))~MSZoning+LotArea+GarageArea+LandSlope+Neighborhood+Condition1+YearBuilt+OverallQual+YearBuilt*OverallCond+YearRemodAdd+RoofMatl+BsmtFinSF1+BsmtFinSF2+BsmtUnfSF+X1stFlrSF+X2ndFlrSF+KitchenQualEx+KitchenQualFa+KitchenQualGd+ScreenPorch+PoolArea+GarageQualFa+GarageQualGd+GarageQualPo+GarageQualTA+WoodDeckSF+SaleCondition+I(LotArea^2)+I(LotArea^3)+I(LotArea^4)+I(LotArea^5)+I(YearBuilt^2)+I(YearBuilt^3), data = trainImputeN)
summary(lm.fit)
par(mfrow=c(2,2))
plot(lm.fit)

# Removing the outliers and high leverage points
trainImputeN = trainImputeN[-c(250,336,463,1325),]

lm.fit = lm((SalePrice^(1/4))~MSZoning+LotArea+GarageArea+LandSlope+Neighborhood+Condition1+YearBuilt+OverallQual+YearBuilt*OverallCond+YearRemodAdd+RoofMatl+BsmtFinSF1+BsmtFinSF2+BsmtUnfSF+X1stFlrSF+X2ndFlrSF+KitchenQualEx+KitchenQualFa+KitchenQualGd+ScreenPorch+PoolArea+GarageQualFa+GarageQualGd+GarageQualPo+GarageQualTA+WoodDeckSF+SaleCondition+I(LotArea^2)+I(LotArea^3)+I(LotArea^4)+I(LotArea^5)+I(YearBuilt^2)+I(YearBuilt^3), data = trainImputeN)
summary(lm.fit)
par(mfrow=c(2,2))
plot(lm.fit)

# Imputing the values for test dataset
testImpute = test

boxplot(test$YearBuilt~test$MSZoning)
# The yearbulit  for 791 row of test dataset is 1900. From the box plot, infer that the zoning type = RM 
testImpute[c(791),"MSZoning"] = "RM"

# The yearbulit  for 791 row of test dataset is 1910,1952,1951. From the box plot, infer that the zoning type = RM 
testImpute[c(456,757,1445),"MSZoning"] = "RM"


library(dummies)

KitchenQualVars = dummy(testImpute$KitchenQual)
testImpute = cbind(testImpute,KitchenQualVars)
testImpute  = subset(testImpute, select=-c(KitchenQual))

GarageQualVars = dummy(testImpute$GarageQual)
testImpute = cbind(testImpute,GarageQualVars)
testImpute  = subset(testImpute, select=-c(GarageQual))

num_attr = c("LotArea","MSSubClass","OverallQual","OverallCond","YearBuilt","BsmtFinSF1","BsmtFinSF2","BsmtUnfSF","X1stFlrSF","X2ndFlrSF","BedroomAbvGr","KitchenAbvGr","GarageCars","GarageArea","WoodDeckSF","ScreenPorch","PoolArea","KitchenQualEx","KitchenQualFa","KitchenQualGd","GarageQualFa","GarageQualGd","GarageQualPo","GarageQualTA")

testImpute[,num_attr] = knnImputation(data = subset(testImpute, select=num_attr),k=5)

testImpute[,"Id"] = NULL

testImpute = testImpute[ , colSums(is.na(testImpute)) == 0]

mypredictions = predict(lm.fit,newdata = testImpute)

write.csv(mypredictions,file = "mypred.csv")
