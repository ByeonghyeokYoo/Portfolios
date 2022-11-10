##https://www.kaggle.com/datasets/yasserh/wine-quality-dataset/download?datasetVersionNumber=1

mydata<-read.csv("C:\\Users\\User\\OneDrive\\Desktop\\winequality-red.csv",header =T,sep = ",")
View(mydata)
head(mydata)
mydata1<-(mydata[,1:3])
mydata1
summary(mydata1)

## Plotting a histogram
## the data is normally distributed as show
fixed.acidity <- mydata$fixed.acidity
hist(fixed.acidity)

# plotting corr heatmap
# Load and install heatmaply package
## Colinearity between different features can be observed
install.packages("heatmaply")
library(heatmaply)
heatmaply_cor(x = cor(mydata1), xlab = "Features",
              ylab = "Features", k_col = 2, k_row = 2)


# Importing library
install.packages("tseries")
library(tseries)
## for first variable (volatile.acidity)
mydata2<-(mydata[,1:2])
mydata2
mydata2<-as.data.frame(mydata1)
wine.ts1<-ts(mydata2[,2], frequency=4)
#par(mfrow=c(3,1), mar=c(4,4,4,4)) ### Figure 4.5
plot(wine.ts1, type="l", xlab="volatile.acidity",ylab="fixed.acidity")
acf(mydata2[,2], 25, xlab="Lag", ylab="ACF", main="")
acf(mydata2[,2], 25, type="partial", xlab="Lag",ylab="Partial ACF", main="")

## for second  variable (citric.acid)
mydata3<-(mydata[,3])
mydata3
mydata3<-as.data.frame(mydata1)
wine.ts2<-ts(mydata3[,2], frequency=4)
#par(mfrow=c(3,1), mar=c(4,4,4,4)) ### Figure 4.5
plot(wine.ts2, type="l", xlab="citric.acid",ylab="fixed.acidity")
acf(mydata3[,2], 25, xlab="Lag", ylab="ACF", main="")
acf(mydata3[,2], 25, type="partial", xlab="Lag",ylab="Partial ACF", main="")


# Conduct the augmented Dickey-Fuller test
data1=mydata$volatile.acidity
data2=mydata$citric.acid
adf.test(data1)
## The test statistic and p-value come out to be equal to 0.51057 and 0.99 respectively. 
## Since the p-value is equal to or greater than 0.05,
## hence we would fail to reject the null hypothesis. 
## It implies that the time series is non-stationary. 
adf.test(data2)
## The test statistic and p-value come out to be equal to -1.321 and 0.831 respectively. 
## Since the p-value is equal to or greater than 0.05,
## hence we would fail to reject the null hypothesis. 
## It implies that the time series is non-stationary. 

#Difference the data
dwine1<-diff(mydata2[,2], lag=4)
#par(mfrow=c(3,1), mar=c(4,4,4,4)) ### Figure 4.6
plot(diff(wine.ts1, lag=4), type="l", xlab="volatile.acidity",ylab="Differenced Series")
acf(dwine1, 25, xlab="Lag", ylab="ACF", main="")
acf(dwine1, 25, type="partial", xlab="Lag",ylab="Partial ACF", main="")
## the series is stationary since the ACF cuts off at lag one
## ## and the PACF attenuates

#Difference the data
dwine2<-diff(mydata3[,2], lag=4)
#par(mfrow=c(3,1), mar=c(4,4,4,4)) ### Figure 4.6
plot(diff(wine.ts2, lag=4), type="l", xlab="citric.acid",ylab="Differenced Series")
acf(dwine2, 25, xlab="Lag", ylab="ACF", main="")
acf(dwine2, 25, type="partial", xlab="Lag",ylab="Partial ACF", main="")
## the series is stationary since the ACF cuts off at lag one
## and the PACF attenuates

#Fitting the model
dwine1.aic<-matrix(0,5,5)
dwine1.aic
for (i in 0:4) for (j in 0:4) {
  dwine1.fit<-arima(dwine1, order=c(i,0,j))
  dwine1.aic[i+1,j+1]<-dwine1.fit$aic
}

dwine1.fit<-arima(dwine1, order=c(3,0,4))
dwine1.fit

install.packages("dLagM")
library(dLagM)
data(mydata)
model.ardlDlm1 = ardlDlm(formula =fixed.acidity ~ volatile.acidity + citric.acid,
                         data = data.frame(mydata) , p = 2 , q = 1 )
summary(model.ardlDlm1)

## Provide a brief summary of your findings and state which model performs better.

## adjusted R2. is 0.5175 which means that at least half of the response variable is explained by the predictor variables.
## This model has equally distributed residuals around zero.
## The errors of this model are within a small bandwidth.
## This fitted model therefore performs better than the first two models.
## From the finding we can see that an increase in volatile. acidity increase the fixed. acidity positively
## also Increase in citric.acid increase the fixed.acidity 
## they should both be controlled at an average level.
