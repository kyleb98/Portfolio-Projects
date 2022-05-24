################################################################################ Netflix Case Study ############################################################################################

library(tidyverse)
library(magrittr)
library(DT)
library(factoextra)
library(plyr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggcorrplot)
library(corrplot)
library(MASS)
library(DataExplorer)
library(leaps)
library(ISLR)
library(ISwR)
library(lattice)
library(class)
library(caret)
library(glmnet)
library(readr)
library(stringr)
library(factoextra)
library(ggplot2)
library(dplyr)
library(magrittr)
library(purrr)
library(cluster)

#Reading the data in and EDA


data=read.csv("Netflix.csv",h=T,stringsAsFactors=T)
data 

#This line of code makes all the empty cells to 0 which will come in handy for LDA
data[is.na(data)]=0

#create_report(data)

#Part D: Among all the Classification methods, which one is the more reliable technique for classifying series vs movies

#First we will use logistic regression, then we will check other methods to compare the error rate

glm1=glm(Series.or.Movie~Hidden.Gem.Score+IMDb.Score+Rotten.Tomatoes.Score+Awards.Received+Awards.Nominated.For+IMDb.Votes,data=data,family="binomial")
glm1
summary(glm1)
attach(data)
coef(glm1)
summary(glm1)$coef
glm.probs=predict(glm1,type="response")
glm.probs[1:10]
glm.pred=rep("Movie",7740)
glm.pred[glm.probs>.5]="Series"
table(glm.pred,Series.or.Movie)
mean(glm.pred==Series.or.Movie)

#The classification rate comes out to be .7663 by computing it manually.

#LDA Method

attach(data)
dataindex=sample(nrow(data),ceiling(nrow(data)*.5),replace=F)
train=data[dataindex,]
test=data[-dataindex,]

dim(train)
dim(test)

lda.fit=lda(Series.or.Movie~Hidden.Gem.Score+IMDb.Score+Rotten.Tomatoes.Score+Awards.Received+Awards.Nominated.For+IMDb.Votes,data=data,subet=train) 
lda.fit
lda.pred=predict(lda.fit,data)
lda.class=lda.pred$class
table(lda.class,Series.or.Movie)
mean(lda.class==Series.or.Movie)

#The classification comes out to be .7890

qda.fit=qda(Series.or.Movie~Hidden.Gem.Score+IMDb.Score+Rotten.Tomatoes.Score+Awards.Received+Awards.Nominated.For+IMDb.Votes,data=data,subet=train) #Same issue I ran into from the LDA approach, so I proceeded similarly
qda.fit
qda.class=predict(qda.fit,data)$class
table(qda.class,Series.or.Movie)
mean(qda.class==Series.or.Movie)

#The classification comes out to be .6052

#Part e

#Whole bunch of set up for the lasso and ridge regression

set.seed(1)
data.size=dim(data)[1]/2
train2=sample(1:dim(data)[1],data.size)
test2=-train2
data.train=data[train2,]
data.test=data[test2,]
train.mat=model.matrix(Hidden.Gem.Score~IMDb.Score+Rotten.Tomatoes.Score+Awards.Received+Awards.Nominated.For+IMDb.Votes+Series.or.Movie,data=data.train)
test.mat=model.matrix(Hidden.Gem.Score~IMDb.Score+Rotten.Tomatoes.Score+Awards.Received+Awards.Nominated.For+IMDb.Votes+Series.or.Movie,data=data.test)

#Lasso regression

data$Series.or.Movie = as.numeric(data$Series.or.Movie=="Movie") #Change to binary dummy variables
set.seed(1)
grid=10^seq(4,-2,length=100)
mod.lasso=cv.glmnet(train.mat,data.train[,"Hidden.Gem.Score"],alpha=1,lambda=grid,thresh=1e-12)
lambda.best=mod.lasso$lambda.min
lambda.best
lasso.pred=predict(mod.lasso,newx=test.mat,s=lambda.best)
mean((data.test[,"Hidden.Gem.Score"]-lasso.pred)^2)
predict(mod.lasso,s=lambda.best,type="coefficients")
#From Lasso regression, we get the MSE of 3.462791

#Now we will Cross Validate Lasso with Ridge Regression
set.seed(1)
mod.ridge=cv.glmnet(train.mat,data.train[,"Hidden.Gem.Score"],alpha=0,lambda=grid,thresh=1e-12)
lambda.best=mod.ridge$lambda.min
lambda.best
ridge.pred=predict(mod.ridge,newx=test.mat,s=lambda.best)
mean((data.test[,"Hidden.Gem.Score"]-ridge.pred)^2)
predict(mod.ridge,s=lambda.best,type="coefficients")
#From Ridge Regression, we get the MSE of 3.461274

#In this case we can see that ridge regression produced the more accurate model for predicting hidden gem scores

#Part f

regfit.full2=regsubsets(Boxoffice~IMDb.Score+Rotten.Tomatoes.Score+Awards.Received+Awards.Nominated.For+IMDb.Votes+Hidden.Gem.Score,data=data,nvmax=6)
#These would cause me errors because vector size is too big.

reg.summary=summary(regfit.full2)
reg.summary
reg.summary$rsq
which.max(reg.summary$adjr2)
which.min(reg.summary$cp)
which.min(reg.summary$bic)
coef(regfit.full2,5)

#It seems to show that among the movies, the best predictors for boxoffice among the movies are imdb score, rotten tomatoes score
#awards nominated for, imdb votes, and hidden gem score.

#Part g

data$Series.or.Movie = as.numeric(data$Series.or.Movie=="Movie") #Change to binary dummy variables

set.seed(1)
data.size=dim(data)[1]/2
train3=sample(1:dim(data)[1],data.size)
test3=-train3
data.train2=data[train3,]
data.test2=data[test3,]
train.mat2=model.matrix(Boxoffice~Hidden.Gem.Score+IMDb.Score+Rotten.Tomatoes.Score+Awards.Received+Awards.Nominated.For+IMDb.Votes+Series.or.Movie,data=data.train2)
test.mat2=model.matrix(Boxoffice~Hidden.Gem.Score+IMDb.Score+Rotten.Tomatoes.Score+Awards.Received+Awards.Nominated.For+IMDb.Votes+Series.or.Movie,data=data.test2)

set.seed(1)
grid=10^seq(4,-2,length=100)
mod.lasso2=cv.glmnet(train.mat2,data.train2[,"Boxoffice"],alpha=1,lambda=grid,thresh=1e-12,family="binomial")

#Not too sure how to handle a response variable that has double factor, so I am just lacking the coding technique to address this problem
#But if all goes correctly, we would use lasso regression to make a prediction model for boxoffice in an ideal world. 


lambda.best2=mod.lasso2$lambda.min
lambda.best2
lasso.pred2=predict(mod.lasso2,newx=test.mat2,s=lambda.best)
mean((data.test[,"Boxoffice"]-lasso.pred2)^2)
predict(mod.lasso2,s=lambda.best,type="coefficients")

#Part h

set.seed(2)
complete=hclust(dist(data),method="complete")

#It seems to show that to perform hierarchical clustering, we must remove the NA's in the data set. 
#I'm not too sure how to address that bc I thought I already did, so again, my R coding falls short to answer this problem 
#Correctly, but will roll with the punches. It still produces a dendrogram though, but it is nearly impossible to interpret besides having 3 clusters

plot(complete)

cutree(complete,3)

sddata=scale(data)
completesd=hclust(dist(data),method="complete")
plot(completesd)

cutree(completesd,3)
table(cutree(complete,3),cutree(completesd,3))