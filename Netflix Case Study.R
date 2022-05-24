############################################################################################### Consumer Report Rating of Cereal ##################################################################################################################################

data2=read.csv("cereal.csv")
data2
names(data2)
attach(data2)
#Part 1

#I would say I agree with the consumer grading reports of the cereal because if I use all bran with extra fiber
#as a benchmark for all the other cereals, then we can see why it is the highest rated cereal health wise. 
#All bran with extra fiber has the lowest calories, no fat, low sodium, high fiber, high carbohydrates, zero sugars, 
#great source of potassium and two cups of it equates to one serving, so it is very nutrious and filling cereal to have for breakfasr

#Part 2 

regfit.full=regsubsets(rating~type+calories+protein+fat+sodium+fiber+carbo+sugars+potass+vitamins+shelf+weight+cups,data=data2,nvmax=16)
reg.summary=summary(regfit.full)
reg.summary
reg.summary$rsq
which.max(reg.summary$adjr2)
which.min(reg.summary$cp)
which.min(reg.summary$bic)
coef(regfit.full,9)

best.cereal.model=glm(rating~calories+protein+fat+sodium+fiber+carbo+sugars+potass+vitamins,data=data2)
best.cereal.model
summary(best.cereal.model)

#From our best subset selection, we can see that the best predictors for predicting rating includes: Calories, protein, fat, sodium,
#fiber, carbo, sugars, potass, and vitamins.

attach(data2)
data2[which.max(rating),"name"] 
data2[which.min(rating),"name"] 

#This shows that all bran with extra fiber is the highest rated cereal and cap'n'crunch is the lowest rated cereal

#I would say my fomulation is fairly consistent with the consumer reports because from the data, I can see that all bran with extra fiber has
#the best numbers for having the lowest calories, moderate protein, zero fat, low sodium, great fiber, moderate carbohydrates, zero sugar, good
#source of potassium and has some vitamins. Although it does not have the best in terms of all of those predictors for ratings, a 93 rating
#from consumer reports is nothing to scoff at, and if it did have the best in all areas it would be a 100 rating. 

#Part 3

attach(data2)
par(mfrow=c(3,3))
boxplot(calories~mfr,xlab="Manufacturer",ylab="Calories per Serving)",main="Calories in Cereal Based on Manufacturer")
boxplot(protein~mfr,xlab="Manufacturer",ylab="Protein (milligrams)",main="Protein in Cereal Based on Manufacturer")
boxplot(fat~mfr,xlab="Manufacturer",ylab="Fat (grams)",main="Fat in Cereal Based on Manufacturer")
boxplot(sodium~mfr,xlab="Manufacturer",ylab="Sodium (milligrams)",main="Sodium in Cereal Based on Manufacturer")
boxplot(fiber~mfr,xlab="Manufacturer",ylab="Fiber (grams)",main="Fiber in Cereal Based on Manufacturer")
boxplot(carbo~mfr,xlab="Manufacturer",ylab="Carbohydrates (grams)",main="Carbohydrates in Cereal Based on Manufacturer")
boxplot(sugars~mfr,xlab="Manufacturer",ylab="Sugar (grams)",main="Sugar Content in Cereal Based on Manufacturer")
boxplot(potass~mfr,xlab="Manufacturer",ylab="Potassium (milligrams)",main="Potassium in Cereal Based on Manufacturer")
boxplot(vitamins~mfr,xlab="Manufacturer",ylab="Vitamins (% of FDA recommended)",main="Vitamins in Cereal Based on Manufacturer")

boxplot(rating~mfr,xlab="Manufacturer",ylab="Rating",main="Ratings of Cereal Based on Manufacturer")

#Part 4
#Explain in report

#Part 5

#We would need to make linear models to see the effects of sugars and calories have on ratings

model1=lm(rating~sugars+calories,data=data2)
summary(model1)
plot(model1)
#We can see that both sugars and calories are statistically significant variables to tell the story of ratings, but it seems that sugars
#is the more statistically significant predictor than calories, but both are still solid predictors from our summary of model1

#Boxplots will help tell the story of how mfr and shelf impact ratings

boxplot(rating~mfr,xlab="Manufacturer",ylab="Ratings",main="Ratings of Cereal Brand Based on Manufacturer")
boxplot(rating~shelf,xlab="Shelf Number",ylab="Ratings",main="Ratings Based on Shelf Level")

#Looking at the boxplot for ratings to mfr, we can see that Nabisco generally has higher rated cereals than all the other brands on average
#but, from the boxplot alone, it simply just compares how each brand fares against each other.
#Looking at the boxplot for ratings to shelf level, we can see that on average, shelf one has the highest rated cereals on average, and 
#shelf three has the next highest average rated cereals. So what we can inference here is that shelf 1 tends to be the most bought 
#cereal because I assume kids could see it better and grab it for their parents than the ones on the second shelf, while shelf three 
#is more catered to adults and has higher rated cereals based on adults. Shelf 2 shows the typically they have the lowest rated cereal 
#averages compared to the other shelf levels.

#Part 6

#create_report(data2)

attach(data2)
par(mfrow=c(3,3))
boxplot(calories~shelf,xlab="Shelf Level",ylab="Calories per Serving)",main="Calories in Cereal Based on Shelf Level")
boxplot(protein~shelf,xlab="Shelf Level",ylab="Protein (milligrams)",main="Protein in Cereal Based on Shelf Level")
boxplot(fat~shelf,xlab="Shelf Level",ylab="Fat (grams)",main="Fat in Cereal Based on Shelf Level")
boxplot(sodium~shelf,xlab="Shelf Level",ylab="Sodium (milligrams)",main="Sodium in Cereal Based on Shelf Level")
boxplot(fiber~shelf,xlab="Shelf Level",ylab="Fiber (grams)",main="Fiber in Cereal Based on Shelf Level")
boxplot(carbo~shelf,xlab="Shelf Level",ylab="Carbohydrates (grams)",main="Carbohydrates in Cereal Based on Shelf Level")
boxplot(sugars~shelf,xlab="Shelf Level",ylab="Sugar (grams)",main="Sugar Content in Cereal Based on Shelf Level")
boxplot(potass~shelf,xlab="Shelf Level",ylab="Potassium (milligrams)",main="Potassium in Cereal Based on Shelf Level")
boxplot(vitamins~shelf,xlab="Shelf Level",ylab="Vitamins (% of FDA recommended)",main="Vitamins in Cereal Based on Shelf Level")

par(mfrow=c(3,3))
boxplot(calories~mfr,xlab="Manufacturer",ylab="Calories per Serving)",main="Calories in Cereal Based on Manufacturer")
boxplot(protein~mfr,xlab="Manufacturer",ylab="Protein (milligrams)",main="Protein in Cereal Based on Manufacturer")
boxplot(fat~mfr,xlab="Manufacturer",ylab="Fat (grams)",main="Fat in Cereal Based on Manufacturer")
boxplot(sodium~mfr,xlab="Manufacturer",ylab="Sodium (milligrams)",main="Sodium in Cereal Based on Manufacturer")
boxplot(fiber~mfr,xlab="Manufacturer",ylab="Fiber (grams)",main="Fiber in Cereal Based on Manufacturer")
boxplot(carbo~mfr,xlab="Manufacturer",ylab="Carbohydrates (grams)",main="Carbohydrates in Cereal Based on Manufacturer")
boxplot(sugars~mfr,xlab="Manufacturer",ylab="Sugar (grams)",main="Sugar Content in Cereal Based on Manufacturer")
boxplot(potass~mfr,xlab="Manufacturer",ylab="Potassium (milligrams)",main="Potassium in Cereal Based on Manufacturer")
boxplot(vitamins~mfr,xlab="Manufacturer",ylab="Vitamins (% of FDA recommended)",main="Vitamins in Cereal Based on Manufacturer")


#Part 7

newdata= data2[,-c(1:3)]
newdata=scale(newdata)
#summary(data)
apply(newdata,2,mean)

apply(newdata,2,sd)

k3=kmeans(newdata,centers=3,nstart=20)
p3=fviz_cluster(k3,geom="point",data=newdata)+ggtitle("k=3")
plot(p3)
#Yes, you can cluster/classify cereal


set.seed(2)
complete=hclust(dist(data2),method="complete")

#It seems to show that to perform hierarchical clustering, we must remove the NA's in the data set. 
#I'm not too sure how to address that bc I thought I already did, so again, my R coding falls short to answer this problem 
#Correctly, but will roll with the punches. It still produces a dendrogram though, but it is nearly impossible to interpret besides having 3 clusters

plot(complete)

cutree(complete,3)

sddata=scale(data)
completesd=hclust(dist(data2),method="complete")
plot(completesd)

cutree(completesd,3)
table(cutree(complete,3),cutree(completesd,3))

#Part 8

attach(data2)
par(mfrow=c(3,3))
boxplot(calories~shelf,xlab="Shelf Level",ylab="Calories per Serving)",main="Calories in Cereal Based on Shelf Level")
boxplot(protein~shelf,xlab="Shelf Level",ylab="Protein (milligrams)",main="Protein in Cereal Based on Shelf Level")
boxplot(fat~shelf,xlab="Shelf Level",ylab="Fat (grams)",main="Fat in Cereal Based on Shelf Level")
boxplot(sodium~shelf,xlab="Shelf Level",ylab="Sodium (milligrams)",main="Sodium in Cereal Based on Shelf Level")
boxplot(fiber~shelf,xlab="Shelf Level",ylab="Fiber (grams)",main="Fiber in Cereal Based on Shelf Level")
boxplot(carbo~shelf,xlab="Shelf Level",ylab="Carbohydrates (grams)",main="Carbohydrates in Cereal Based on Shelf Level")
boxplot(sugars~shelf,xlab="Shelf Level",ylab="Sugar (grams)",main="Sugar Content in Cereal Based on Shelf Level")
boxplot(potass~shelf,xlab="Shelf Level",ylab="Potassium (milligrams)",main="Potassium in Cereal Based on Shelf Level")
boxplot(vitamins~shelf,xlab="Shelf Level",ylab="Vitamins (% of FDA recommended)",main="Vitamins in Cereal Based on Shelf Level")

par(mfrow=c(3,3))
boxplot(calories~mfr,xlab="Manufacturer",ylab="Calories per Serving)",main="Calories in Cereal Based on Manufacturer")
boxplot(protein~mfr,xlab="Manufacturer",ylab="Protein (milligrams)",main="Protein in Cereal Based on Manufacturer")
boxplot(fat~mfr,xlab="Manufacturer",ylab="Fat (grams)",main="Fat in Cereal Based on Manufacturer")
boxplot(sodium~mfr,xlab="Manufacturer",ylab="Sodium (milligrams)",main="Sodium in Cereal Based on Manufacturer")
boxplot(fiber~mfr,xlab="Manufacturer",ylab="Fiber (grams)",main="Fiber in Cereal Based on Manufacturer")
boxplot(carbo~mfr,xlab="Manufacturer",ylab="Carbohydrates (grams)",main="Carbohydrates in Cereal Based on Manufacturer")
boxplot(sugars~mfr,xlab="Manufacturer",ylab="Sugar (grams)",main="Sugar Content in Cereal Based on Manufacturer")
boxplot(potass~mfr,xlab="Manufacturer",ylab="Potassium (milligrams)",main="Potassium in Cereal Based on Manufacturer")
boxplot(vitamins~mfr,xlab="Manufacturer",ylab="Vitamins (% of FDA recommended)",main="Vitamins in Cereal Based on Manufacturer")
boxplot(rating~mfr,xlab="Manufacturer",ylab="Rating",main="Ratings of Cereal Based on Manufacturer")


#There are definitely outliers in this data set
