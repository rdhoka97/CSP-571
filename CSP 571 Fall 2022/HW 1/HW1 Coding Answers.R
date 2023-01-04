#PROBLEM 1

#Load Libraries
library(ggplot2)

#Load Iris Dataset
iris <-data.frame(iris)

#BoxPlot
boxplot(iris,main="BoxPlot",xlab="Attributes",ylab="Values", col = "salmon")

#Inter Quartile Range Calculation:
print("IQR of SepalLength")
IQR(iris$Sepal.Length)
print("IQR of SepalWidth")
IQR(iris$Sepal.Width)
print("IQR of PetalLength")
IQR(iris$Petal.Length)
print("IQR of PetalWidth")
IQR(iris$Petal.Width)

#SD
print("Standard Deviation of SepalLength")
sd(iris$Sepal.Length)
print("Standard Deviation of SepalWidth")
sd(iris$Sepal.Width)
print("Standard Deviation of PetalLength")
sd(iris$Petal.Length)
print("Standard Deviation of PetalWidth")
sd(iris$Petal.Width)

#ggplot
ggplot(data = iris, aes(x = Species, y = Sepal.Length, fill = Species)) + geom_boxplot() + ggtitle("SepalLength")
ggplot(data = iris, aes(x = Species, y = Sepal.Width, fill = Species)) + geom_boxplot() + ggtitle("SepalWidth")
ggplot(data = iris, aes(x = Species, y = Petal.Length, fill = Species)) + geom_boxplot() + ggtitle("PetalLength")
ggplot(data = iris, aes(x = Species, y = Petal.Width, fill = Species)) + geom_boxplot() + ggtitle("PetalWidth")


#------------------------------------------------------------------------------------------------------------------


#PROBLEM 2

#Load Library
library(moments)

#Load Tree Sample Dataset
trees <-data.frame(trees)

#summary of Attributes
summary(trees)

#Histogram of Attributes
par(mfrow=c(3,1))
hist(trees$Girth,main="Histogram for Girth", xlab = "Girth", ylab = "Count",border="black", col="salmon",las=1)
hist(trees$Height,main="Histogram for Height", xlab = "Height", ylab = "Count",border="black", col="salmon",las=1)
hist(trees$Volume,main="Histogram for volume", xlab = "Volume", ylab = "Count",border="black", col="salmon",las=1)

print("Skewness of Girth:")
skewness(trees$Girth)
print("Skewness of Height:")
skewness(trees$Height)
print("Skewness of Volume:")
skewness(trees$Volume)



#-------------------------------------------------------------------------------------------------------------------


#PROBLEM 3

mpgData = read.table(url("https://archive.ics.uci.edu/ml/machine-learning-databases/auto-mpg/auto-mpg.data"),header = F, sep = "", stringsAsFactors = F)
mpgHeader = c("mpg","cylinders","displacement","horsepower","weight","acceleration","model year","origin","car name")
colnames(mpgData) = mpgHeader                           # setting up the headers
mpgData$horsepower = as.numeric(mpgData$horsepower)     # converting factors to numeric type
print("Mean before replacment:")
mean(mpgData$horsepower, na.rm = T)
mpg_median = median(mpgData$horsepower, na.rm = T)
mpgData$horsepower[is.na(mpgData$horsepower)] = mpg_median
print("Mean after replacment:")
mean(mpgData$horsepower, na.rm = T)



#-------------------------------------------------------------------------------------------------------------------


#PROBLEM 4

#Load Libraries
library(MASS)
library(ggplot2)
bostonData = data.frame(Boston)
attach(Boston)

#Display Summary
summary(bostonData)

#Fit a Regression Model
linearModel1 = lm(medv~lstat, data = bostonData)
summary(linearModel1)
coef(linearModel1)
cat("R-squared for Linear Model: ",summary(linearModel1)$r.sq)

#Plot Resulting Fit
plot(lstat,medv,col="lawngreen")
abline(linearModel1, col="red",lwd=2)
par(mfrow=c(2,2))
plot(linearModel1, col="lawngreen")

#plot of Fitted values Vs. Residuals
plot(linearModel1$fitted.values,linearModel1$residuals, col="lawngreen")
abline(linearModel1,col="red",lwd=2)

#Plot for Non-Linear Fit:
p1 <- ggplot(data=bostonData,aes(x=linearModel1$fitted,y=linearModel1$residuals)) +
  geom_point()+
  stat_smooth(col="red")+
  ggtitle("Non-Linear fit")
p1
coef(linearModel1)

#predictions for lstat values of 5, 10 and 15
test=data.frame(lstat=c(5,10,15))
predict(linearModel1,test,interval = "confidence")
predict(linearModel1,test,interval = "predict")

#Modified Plot
cat("Modifying the model to include lstat^2")
linearModel2=lm(medv~lstat + I(lstat^2))
summary(linearModel2)
coef(linearModel2)
cat("R-squared for Non-Linear Model: ",summary(linearModel2)$r.sq)
par(mfrow=c(2,2))
plot(linearModel2,col="lawngreen")

#plot for fitted values vs residual:
ggplot(data=bostonData,aes(x=linearModel2$fitted,y=linearModel2$residuals)) +
  geom_point(col="lawngreen")+
  stat_smooth(col="red")+
  ggtitle("Residual vs fitted values")

#plot for non-linear fit:
ggplot(data=bostonData,aes(x=lstat,y=medv)) +
  geom_point(col="lawngreen")+
  stat_smooth(formula = y ~ x + I(x^2),method="lm",col="red")+
  ggtitle("Non-Linear fit")

anova(linearModel1,linearModel2)
