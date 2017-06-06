# remove all env variables
rm(list=ls(all=TRUE))

# setup a working directory
setwd(choose.dir())
getwd()

# read the training data
df = read.csv("train.csv", header = T)

# read the test data
df_test = read.csv("test.csv", header = T)

# append "Survived" column to the testData and initialize it will all zeros
df_test = as.data.frame(append(df_test, list(Survived = 0), after = 1))

# bind the rows of training and test data so that everything can be standardized together
df = rbind(df,df_test)

# remove test data post bind
rm(df_test)

# load the needed libraries
library(vegan)
library(infotheo)
library(DMwR)
library(dummies)

# factorize the following
EmbarkedVars = dummy(df$Embarked)
df = cbind(df,EmbarkedVars)
df  = subset(df, select=-c(Embarked))

ClassVars = dummy(df$Pclass)
df = cbind(df,ClassVars)
df  = subset(df, select=-c(Pclass))

SexVars = dummy(df$Sex)
df = cbind(df,SexVars)
df  = subset(df, select=-c(Sex))
names(df)
str(df)
colnames(df)

# figured out the numerical attributes
num_attr = c("Age", "Fare", "SibSp", "Parch")

# set diff to get the categorical attributes
cat_attr = setdiff(colnames(df), num_attr)

# segregate the dataset based on numerical attributes column and categorical attributes column
df_num = df[,num_attr]
df_cat = df[,cat_attr]

# factorize all the categorical attributes atonce using apply method and convert back to dataframe
df_cat = data.frame(apply(df_cat, MARGIN = 2, FUN = function(x) as.factor(as.character(x))))
str(df_cat)

# bind together the categorical col and numerical colms to get the final dataset
df = cbind(df_cat,df_num)
str(df)
sum(is.na(df))

unique(df$Survived)
table(df$Survived)

#df_omitNA = na.omit(df)
#rm(df_omitNA)
#summary(df)

# extract the list of titles
library(qdapRegex)
MrTitle = c("Mr", "Don", "Major", "Capt", "Jonkheer", "Rev", "Col", "Sir")
MrsTitle = c("Countess", "Mme", "Mrs")
MissTitle = c("Mlle", "Ms", "Miss", "Lady", "the Countess")
DrTitle = c("Dr")
MasterTitle = c("Master")

# create a new titles column and extract the title. Consolidate few titles under broad categories
titles = sapply(df$Name, FUN = function(x) {
  if (is.element(rm_between(x, ",", ".", extract=TRUE), MrTitle)) {
    return("Mr") }
  else if (is.element(rm_between(x, ",", ".", extract=TRUE), MrsTitle)) {
    return("Mrs")} 
  else if (is.element(rm_between(x, ",", ".", extract=TRUE), MissTitle)) {
    return("Miss")}
  else if (is.element(rm_between(x, ",", ".", extract=TRUE), MasterTitle)) {
    return("Master")}
  else if (is.element(rm_between(x, ",", ".", extract=TRUE), DrTitle)) {
    return("Dr")}
  else {
    return("Ha")}
})

# add the titles col back to the dataset
df = cbind(df,titles)
table(df$titles)

# Identify the dummy titles and bind it to the dataset
TitlesVars = dummy(df$titles)
df = cbind(df,TitlesVars)
df  = subset(df, select=-c(titles))
df  = subset(df, select=-c(titlesHa))
str(df)

# factorize the dummy titles
df$titlesDr = as.factor(as.character(df$titlesDr))
df$titlesMaster = as.factor(as.character(df$titlesMaster))
df$titlesMiss = as.factor(as.character(df$titlesMiss))
df$titlesMr = as.factor(as.character(df$titlesMr))
df$titlesMrs = as.factor(as.character(df$titlesMrs))
str(df)

# Here we are extracting the cabin class information. If its not present assign "U" for unknown
classInfo = sapply(df$Cabin, function(x) {
  if(substr(x, 1, 1) != "") {
    return(substr(x, 1, 1))
  } else {
    return ("U")
  }
}
)

table(classInfo)

# bind the Cabin class information 
df = cbind(df,classInfo)
classInfoVars = dummy(df$classInfo)
df = cbind(df,classInfoVars)
df  = subset(df, select=-c(classInfo))

# factorize the cabin class information in the dataset
df$classInfoA = as.factor(as.character(df$classInfoA))
df$classInfoB = as.factor(as.character(df$classInfoB))
df$classInfoC = as.factor(as.character(df$classInfoC))
df$classInfoD = as.factor(as.character(df$classInfoD))
df$classInfoE = as.factor(as.character(df$classInfoE))
df$classInfoF = as.factor(as.character(df$classInfoF))
df$classInfoG = as.factor(as.character(df$classInfoG))
df$classInfoT = as.factor(as.character(df$classInfoT))
df$classInfoU = as.factor(as.character(df$classInfoU))

str(df)
sum(is.na(df))

unique(df$Survived)

# remove the passenger id col as its not required
df$PassengerId =  NULL

# now impute the values for age. 
df_imputed <- knnImputation(data = df,k=8) #KNN Imputation
#df_imputed <- centralImputation(data = df) #Central Imputation

# Standardizing the dataset
num_attr = c("Age", "Fare", "SibSp", "Parch")
cat_attr = setdiff(colnames(df), num_attr)

df_imputed_num = df_imputed[,num_attr]
df_imputed_cat = df_imputed[,cat_attr] 

# using decostand function to standardize using z-scores method
df_imputed_num1 = decostand(df_imputed_num, "standardize")

df_final = cbind(df_imputed_num1,df_imputed_cat)
rm(df_imputed_num1, df_imputed_num, df_imputed_cat)
rm(df_num)
rm(df_cat)
sum(is.na(df_final))

library(RColorBrewer)
hist(df_final$Age)
hist(df_final$Age,col=brewer.pal(12,"Set3"))
boxplot(df_final$Age)
barplot(table(df_final$Survived),col = brewer.pal(3,"Set3")) 

plot(df_final$Fare, df_final$Age, xlab = "Fare", ylab = "Age" )
boxplot(Age~Survived,data = df_final,xlab ="TARGET",ylab = "Age",main = "Continuous v/s Categorical")
boxplot(Fare~Survived,data = df_final,xlab ="TARGET",ylab = "Fare",main = "Continuous v/s Categorical")

# remove the non-numerical cols that are not required 

df_final = subset(df_final, select=-c(Cabin))
df_final = subset(df_final, select=-c(Ticket))
df_final = subset(df_final, select=-c(Name))
df_final = subset(df_final, select=-c(Embarked))

# split the dataset back into training data and test data
trainData <- df_final[1:891,]
test <- df_final[892:1309,]

# using svm model to predict
#model <- glm(Survived ~.,family=binomial(link='logit'),data=train)
library(caret)
library(e1071)
#model = train(Survived~., data=trainData, method="svmRadial", preProc=c("BoxCox"), trControl=trainControl((method = "boot")))
model <- svm(Survived ~ ., data = trainData)

summary(model)

# getting the results from the predicted model

fitted.results <- predict(model,newdata=test,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)

fitted.results = as.vector(fitted.results)

fitted.results = data.frame(Survived= fitted.results)

testData = read.csv("test.csv", header = T)

myResults = cbind(testData, fitted.results)

# writing the results back into the file
write.csv(myResults,file = "myResults6.csv")


misClasificError <- mean(fitted.results != test$Survived)
print(paste('Accuracy',1-misClasificError))

library(gplots)
library(ROCR)
p <- predict(model, newdata=test, type="response")
pr <- prediction(p, test$Survived)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

