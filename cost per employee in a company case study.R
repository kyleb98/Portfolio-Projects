
###Alot of code is similar to our in-class dicsussion of Kernel Regression###

### Part 1

CpEdata = read.csv("Cost per employee.csv", h = T)
cost = CpEdata$cost
employee = CpEdata$employee

#Scatterplot
plot(CpEdata)

#Assumption: Data is not linear (kind of obvious from scatterplot)

model = lm(CpEdata$cost~CpEdata$employee)
summary(model)
plot(CpEdata$employee, CpEdata$cost)
par(mfrow = c(2,2))
plot(model)

#Method to use: Kernel Regression (told us in class)

x = CpEdata$employee
y = CpEdata$cost
plot(x, y, xlim = c(min(x)-10, max(x)+10), ylim = c(min(y)-30, max(y)+30))
lines(c(min(x)-15,max(x)+15),c(0,0),lty=2)

#Find optimal h

SSR.KR = function(h){
	x1 = x
	y1 = rep(0,length(x1))
	for(i in 1:length(x1)){
		y1[i] = sum((1/(sqrt(2*pi))) * exp(-.5*((x1[i]-x[-i])/h)^2) * y[-i])/
		sum((1/(sqrt(2*pi))) * exp(-.5*((x1[i]-x[-i])/h)^2))
	}
	res = y - y1
	SSR = sum(res^2)
	SSR
}

h = optim(2,SSR.KR)$par

#Kernel regression/smoothing

x1 = seq(min(x), max(x), by=(max(x)-min(x))/99)
y1 = rep(0,100)
for(i in 1:100){
	y1[i] = sum((1/(sqrt(2*pi))) * exp(-.5*((x1[i]-x)/h)^2) * y)/
	sum((1/(sqrt(2*pi))) * exp(-.5*((x1[i]-x)/h)^2))
}

lines(x1, y1, col = 2)

#Plot

h = 1.204687
plot(x, y, xlim=c(min(x)-10, max(x)+10), ylim=c(min(y)-30, max(y)+30))
x1 = seq(min(x), max(x), by=(max(x)-min(x))/99)
y1 = rep(0,100)
for(i in 1:100){
	y1[i] = sum((1/(sqrt(2*pi))) * exp(-.5*((x1[i]-x)/h)^2) * y)/
	sum((1/(sqrt(2*pi))) * exp(-.5*((x1[i]-x)/h)^2))
}

lines(x1, y1, col = 2)

### Part 2 (Confidence Interval)

# Strategy:
# 1: Fit original model
# 2: BS sample x values
# 3: Use og model to predict BS-yhat
# 4: BS.y = BS.yhat + random residual from og model

#OG Residual
	
#y_55 = rep(0,1000)
#for(k in 1:1000) {
	
x1 = x
y1 = rep(0,length(x1))
	for(i in 1:length(x1)){
		y1[i] = sum((1/(sqrt(2*pi))) * exp(-.5*((x1[i]-x)/h)^2) * y)/
		sum((1/(sqrt(2*pi))) * exp(-.5*((x1[i]-x)/h)^2))
	res = y - y1
	}

#Recommended in OH's	
y_55 = rep(0,1000)
for(k in 1:1000) {
	
#Resample x
x.bootstrap = sample(x, length(x), replace = T)
#x.bootstrap

x1 = x.bootstrap 
yhat.bootstrap = rep(0,length(x1))
	for(i in 1:length(x1)){
		yhat.bootstrap[i] = sum((1/(sqrt(2*pi))) * exp(-.5*((x1[i]-x)/h)^2) * y)/
		sum((1/(sqrt(2*pi))) * exp(-.5*((x1[i]-x)/h)^2))
	}
	
y.bootstrap = yhat.bootstrap + sample(res, length(x), replace = T)
#y.bootstrap

x1 = 55
	for(i in 1:length(x1)){
		y_55[k] = sum((1/(sqrt(2*pi))) * exp(-.5*((x1-x.bootstrap)/h)^2) * y.bootstrap)/
		sum((1/(sqrt(2*pi))) * exp(-.5*((x1-x.bootstrap )/h)^2))
	}
}

#Graphs
#Saw through office hours
plot(x,y)
x1 = seq(min(x),max(x),by=(max(x)-min(x))/99)
y1 = rep(0,100)
for(i in 1:100){
	y1[i] = sum((1/(sqrt(2*pi))) * exp(-.5*((x1[i]-x)/h)^2) * y)/
	sum((1/(sqrt(2*pi))) * exp(-.5*((x1[i]-x)/h)^2))
}

lines(x1 ,y1 ,col = 2)

lower.ci = sort(y_55)[25]
upper.ci = sort(y_55)[975]
#lower.ci
#upper.ci

### Part 3 (Prediction Interval)

y_55_prediction = y_55 + sample(res, 1000, replace = T)
lower.p = sort(y_55_prediction)[25]
upper.p = sort(y_55_prediction)[975]
#lower.p
#upper.p

