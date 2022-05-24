##Question 1
data.grav=read.csv("antigravity.csv",h=T)
data.grav

##Need to find a way to make a model 
t = data.grav$t
P = data.grav$P
model=lm(P~t)
par(mfrow=c(2,2))
plot(model)
dev.off()
plot(data.grav$t,data.grav$P)
x.seq = seq(0,3,by=.01)
y.seq = predict(model,newdata=data.frame(t=x.seq))
lines(x.seq,y.seq,col=2,lwd=2)

##Linear model doesn't work so lets try what they hinted, which was a polynomial model
model1=lm(P~t + I(t^2) + I(t^3))
par(mfrow=c(2,2))
plot(model1)
dev.off()
plot(data.grav$t,data.grav$P)
x.seq = seq(0,3,by=.01)
y.seq = predict(model1,newdata=data.frame(t=x.seq))
lines(x.seq,y.seq,col=2,lwd=2)
y.seq1 = 4.8*x.seq^3 - 9.8*x.seq^2 + 5
lines(x.seq,y.seq1,col=4,lwd=2)
summary(model1)

##Hypothesis Testing
#Week 3/4 Material
#Want  to test Ho:b3=4.8, b2=-9.8,b1=0,b0=5
#z=b3-h0b3/(se

















##Question 2
data=read.csv("eqmag.csv",h=T)
data

install.packages("rpart")
install.packages("randomForest")
install.packages("adabag")
install.packages("ROCR")
install.packages("gbm")
library(rpart)
library(randomForest)
library(adabag)
library(ROCR)
library(gbm)

##Need to make a model for predicting EQ mag (Random Forest)

m=data$magnitude
fd=data$fault_depth
fa=data$fault_angle
fl=data$fault_length
v=data$volcano 
y=data$volcano

x1=data$magnitude
x2=data$fault_depth
x3=data$fault_angle
x4=data$fault_length

model.eq=data.frame(m,fd,fa,fl,v)
model.rf=randomForest(m~v+fd+fa+fl,data=model.eq,ntree=100,mtry=2,control=rpart.control(minsplit=10,cp=.05))
names(model.rf)
errors=m-model.rf$predicted
plot(model.rf$predicted,errors)
abline(0,0,col=2)

##R^2
SSE=sum(errors^2)
SST=sum((m-mean(m))^2)
R2=(SST-SSE)/(SST)

##Prediction
fd=1.6
fa=13
fl=67
v=1

Summer=data.frame(fd,fa,fl,v)
predict(model.rf,newdata=Summer)

