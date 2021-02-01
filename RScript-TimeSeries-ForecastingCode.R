TimeSeries<-as.data.frame(TimeSeriesCourseworkData19_20)
days<- seq(as.Date("2015-04-01"), as.Date("2019-03-31"), by = "day")
head(TimeSeries)
Patients<-ts(TimeSeries[,2],start = c(2015, as.numeric(format(days[1], "%j"))),
             frequency = 365)
head(Patients)
Patients

#Maximum visits is 97 on 26th Dec 2017 and 1st Jan 2018
max(Patients)

#Minimum visits is 16 on 2nd Aug 2016
min(Patients)

#Mean number of visits is 46.72485
mean(Patients)

#Median number of visits is 45
median(Patients)

#Interquartile range of visits is 16
IQR(Patients)

#Standard Deviation 12.9109
sd(Patients)

#Interquartile Range
IQR(Patients)
summary(Patients)

frequency(Patients)

cycle(Patients)

#We investigate a couple of different months
PatientsDec15<-ts(TimeSeries[245:275,2])
plot(PatientsDec15)

PatientsDec16<-ts(ts(TimeSeries[611:641,2]))
plot(PatientsDec16)
#Aggregate by year
aggregatedtotalmean<-aggregate(Patients,FUN = mean)
aggregatedtotalsum<-aggregate(Patients)
plot(aggregatedtotalsum)
boxplot(Patients~cycle(Patients))

#Aggregate by week
Patientsweekly<-ts(TimeSeries[,2],frequency = 7)
cycle(Patientsweekly)
weeks<-seq(as.Date("2015-04-01"), as.Date("2019-03-31"), by = "week")
weeks
Patients<-ts(TimeSeries[,2])

aggregatedweeklymean<-aggregate(Patientsweekly,FUN=mean)
aggregatedweeklysum<-aggregate(Patientsweekly)
plot(aggregatedweeklymean)
boxplot(Patients~cycle(Patientsweekly))
cycle(Patientsweekly)
#Aggregate by month
Patientsmonthly<-ts(TimeSeries[,2],frequency = 12)

aggregatedmonthlymean<-aggregate(Patientsmonthly,FUN=mean)
aggregatedmonthlysum<-aggregate(Patientsmonthly)
boxplot(Patientsmonthly~cycle(Patientsmonthly))
cycle(Patientsmonthly)
#Plot data, including mean and median
plot(TimeSeriesCourseworkData19_20,type="l",main="Number of patients against time")
abline(h=mean(Patients),col="red",lwd=5,lty=3)
abline(h=median(Patients),col="blue",lwd=5,lty=3)
legend("top", legend=c("Mean=46.72", "Median=45"),
       col=c("red", "blue"), lty=3, cex=0.5)

#General plot of data
plot(TimeSeriesCourseworkData19_20,type="l",main="Number of patients against time")

#Create a seasonal plot using ggplot
install.packages("ggplot2")

install.packages("fpp2")
library(fpp2)

#Seasonal plot for entire dataset
seasonplot(Patients,type="l")

seasonplot(Patientsweekly,type="p")

seasonplot(Patientsmonthly,type="p")

ggseasonplot(Patients,type="l")

#7 Day moving average
Patients7MA<-ma(Patients,7)
plot(Patients7MA,type="l",main="Moving Average Order 7 Plot of Patients",ylab="Patients Number",col="purple")

#Monthly moving average
Patients30MA<-ma(Patients,30)
plot(Patients30MA,type="l",main="Moving Average Order 30 Plot of Patients",ylab="Patients Number",col="darkgreen")

seasonplot(Patients30MA,,type="l")

#3 month moving average
Patients90MA<-ma(Patients,90)
plot(Patients90MA,type="l",main="Moving Average Order 90 Plot of Patients",ylab="Patients Number",col="darkblue")

decompose(Patients, type=c("additive"))
plot(decompose(Patients),main="Additive Decomposition of Patient Number Time Series")

#Examining stationarity
library(tseries)
adf.test(Patients)#low p-value suggests that data is stationary


#Plotting naive model
library(forecast)
naive(Patients,h=7)
plot(Patients)

NavPat<-naive(Patients,h=10)

NavPat$

PatientsTrainingts <-ts(TimeSeries[1:1023,2],start = c(2015),
                                  frequency = 365)

PatientsTest<-ts(TimeSeries[1024:1461,2],start = c(2018),
             frequency = 365)

plot(PatientsTrainingts)
plot(PatientsTest)

naive.Patients <- naive(PatientsTrainingts, h=445)
naive.Patients
naivepatientsfigures<-as.vector(naive.Patients$mean)
snaive.Patients <- snaive(PatientsTrainingts, h=445)
snaivepatientsfigures<-as.vector(snaive.Patients$mean)

PatientsNaive<-c(TimeSeries[1:1023,2],naivepatientsfigures)
head(PatientsNaive)
tail(PatientsNaive)


PatientsNaivets<-ts(PatientsNaive,start = c(2015, as.numeric(format(days[1], "%j"))),
                  frequency = 365)
plot(PatientsNaivets[1000:1468],col="red",type="l")
plot(Patients)
plot(Patients[1000:1468],type="l")
lines(PatientsNaivets[1000:1468],col="red")
lines(PatientsNaivets,col="red")

PatientsSNaivets<-c(TimeSeries[1:1023,2],snaivepatientsfigures)
head(PatientsSNaive)
tail(PatientsSNaive)
PatientsSNaivets<-ts(PatientsSNaive,start = c(2015, as.numeric(format(days[1], "%j"))),
                    frequency = 365)
plot(PatientsSNaivets,col="red")
lines(Patients)

plot(Patients[1000:1468],type="l")
lines(PatientsSNaivets[1000:1468],col="red")

naive.Patients.longterm <- naive(PatientsTrainingts, h=1000)
naivepatientslongtermfigures<-as.vector(naive.Patients.longterm$mean)
snaive.Patients.longterm <- snaive(PatientsTrainingts, h=1000)
snaivepatientslongtermfigures<-as.vector(snaive.Patients.longterm$mean)

PatientsNaiveLongTerm<-c(TimeSeries[1:1023,2],naivepatientslongtermfigures)
PatientsNaiveLongTermts<-ts(PatientsNaiveLongTerm,start = c(2015, as.numeric(format(days[1], "%j"))),
                    frequency = 365)

PatientsSNaiveLongTerm<-c(TimeSeries[1:1023,2],snaivepatientslongtermfigures)
PatientsSNaiveLongTermts<-ts(PatientsSNaiveLongTerm,start = c(2015, as.numeric(format(days[1], "%j"))),
                            frequency = 365)

plot(PatientsNaiveLongTermts)
lines(Patients,col="green")
plot(PatientsSNaiveLongTermts)
lines(Patients,col="blue")



#SES method
ses.Patients <- ses(PatientsTrainingts,h=445)
ses.Patients
sespatientsfigures<-as.vector(ses.Patients$mean)

Patientsses<-c(TimeSeries[1:1023,2],sespatientsfigures)
Patientssests<-ts(Patientsses,start = c(2015, as.numeric(format(days[1], "%j"))),
                     frequency = 365)

plot(Patientssests)
lines(Patients,col="pink")

ses.Patients.longterm <- ses(PatientsTrainingts, h=1000)
sespatientslongtermfigures<-as.vector(ses.Patients.longterm$mean)
PatientsSesLongTerm<-c(TimeSeries[1:1023,2],sespatientslongtermfigures)
PatientsSesLongTermts<-ts(PatientsSesLongTerm,start = c(2015, as.numeric(format(days[1], "%j"))),
                             frequency = 365)
plot(PatientsSesLongTermts)
lines(Patients,col="pink")


#Holt linear method
holt.Patients<-holt(PatientsTrainingts,h=445)
holt.Patients
holtpatientsfigures<-as.vector(holt.Patients$mean)
Patientsholt<-c(TimeSeries[1:1023,2],holtpatientsfigures)
Patientsholtts<-ts(Patientsholt,start = c(2015, as.numeric(format(days[1], "%j"))),
                  frequency = 365)
plot(Patientsholtts)
lines(Patients,col="orange")

holt.Patients.longterm<-holt(PatientsTrainingts,h=1000,damped = TRUE)
holt.Patients.longterm
holtpatientslongtermfigures<-as.vector(holt.Patients.longterm$mean)
PatientsholtLongTerm<-c(TimeSeries[1:1023,2],holtpatientslongtermfigures)
PatientsholtLongTermts<-ts(PatientsholtLongTerm,start = c(2015, as.numeric(format(days[1], "%j"))),
                          frequency = 365)
plot(PatientsholtLongTermts)
lines(Patients,col="purple")

#Holt Winters method
HoltWinters(PatientsTrainingts)
plot(HoltWinters(PatientsTrainingts))

holtwinters.Patients<-HoltWinters(PatientsTrainingts,seasonal = "additive")
holtwinterspatientsfigures<-as.vector(holtwinters.Patients$fitted)
Patientsholtwinters<-c(TimeSeries[1:1023,2],holtwinterspatientsfigures[1:445])
Patientsholtwintersts<-ts(Patientsholtwinters,start = c(2015, as.numeric(format(days[1], "%j"))),
                   frequency = 365)
plot(Patientsholtwintersts)
lines(Patients,col="brown")

PatientsholtwintersLongTerm<-c(TimeSeries[1:1023,2],holtwinterspatientsfigures[1:800])
PatientsholtwintersLongTermts<-ts(PatientsholtwintersLongTerm,start = c(2015, as.numeric(format(days[1], "%j"))),
                           frequency = 365)
plot(PatientsholtwintersLongTermts)
lines(Patients,col="purple")

#Linear regression model y=B0+B1X+eps
PatientsTrainingPoints<-TimeSeries[1:1023,2]
head(PatientsTrainingPoints)
simple.fit = lm(PatientsTrainingPoints ~ c(1:length(PatientsTrainingPoints)))
summary(simple.fit)
abline(simple.fit,col="blue",lwd=2)


plot(PatientsTrainingPoints,main="Plot of Linear Model on Training Set",ylab="Patients",type="l")
abline(simple.fit,col="green",lwd=3)
legend("topleft", legend=c("Linear Regression Estimator"),
       col="green", lty=1, cex=0.5)


plot(Patients,main="Plot of Linear Model on Entire Dataset")
abline(simple.fit,col="purple",lwd=3)
summary(simple.fit)
fitted(simple.fit)

#Multiple linear regression and boxplots

head(TimeSeriesCourseworkData19_20_for_multiple_linear_regression)

simplereg<-lm(data=TimeSeriesCourseworkData19_20_for_multiple_linear_regression,`Number of patients`~Day)
plot(lm(data=TimeSeriesCourseworkData19_20_for_multiple_linear_regression,`Number of patients`~Day))

plot(Patients)

lm(data=TimeSeriesCourseworkData19_20_for_multiple_linear_regression,`Number of patients`~Month)
summary(lm(data=TimeSeriesCourseworkData19_20_for_multiple_linear_regression,`Number of patients`~Day+Month+Year))

kmeans(TimeSeriesCourseworkData19_20_for_multiple_linear_regression[,-1],4)
(kmeans(TimeSeriesCourseworkData19_20_for_multiple_linear_regression[,-1],4))

boxplot(data=TimeSeriesCourseworkData19_20_for_multiple_linear_regression_names_for_year_boxplot,`Number of patients`~Year,xlab="Year",ylab="Patient Numbers",main="Boxplot of Patient Numbers against Year",col="red")
axis(1, at=1:12, labels=month.abb[1:12])


head(TimeSeriesCourseworkData19_20_for_multiple_linear_regression_names)
lm(TimeSeriesCourseworkData19_20_for_multiple_linear_regression_names[,-1])
summary(lm(TimeSeriesCourseworkData19_20_for_multiple_linear_regression_names[,-1]))

plot(Patients,type="l",main="Plot of Linear Regression Estimator on our Time Series")
head(TimeSeriesCourseworkData19_20_for_multiple_linear_regression_names_for_year_boxplot,main="Plot of linear regression estimator on our dataset")
simplereg<-lm(data=TimeSeriesCourseworkData19_20_for_multiple_linear_regression_names_for_year_boxplot,`Number of patients`~Year)
abline(simplereg,col="red",lwd=2)
legend("topleft", legend="Linear Regression Line",
       col="red",lty=1, cex=0.6)


#Multiple linear regression
multreg<-lm(TimeSeriesCourseworkData19_20_for_multiple_linear_regression_names[,-1])
plot(Patients)
abline(multreg,col="red",lwd=2)


#ACFs and PACFs
Patientsacf<-acf(Patients,500)
Patientsacf

Patientspacf<-acf(Patients,50,type="partial")
Patientspacf

#Trying out an ARIMA model
acf(Patients,lag.max = 100)
Patientsar1<-arima(Patients,order=c(1,0,0))

pacf(Patients,lag.ax=20)

tsdiag(Patientsar1)
auto.arima(Patients,seasonal=TRUE)
arima()

Patientsar12i1ma3<-arima(Patients,order=c(12,1,3))
tsdiag(arima(Patients,order=c(12,1,3)))

library(forecast)
predictionarima<-forecast(Patientsar12i1ma3,h=10)
predictionarima
plot(predictionarima)

Patientsar12i1ma3$model
predictionarima$residuals
sum(predictionarima$residuals)









#Naive and SNaive on training and test
PatientsTrain<-subset(Patients,end=1023)
head(PatientsTrain)
tail(PatientsTrain)

NaiveForTestPatients<-naive(PatientsTrain,445,level=FALSE)
plot(NaiveForTestPatients,xlab="Year",ylab="Patient Numbers",main="Naive Method on Test Patients")

accuracy(NaiveForTestPatients)

NaiveEntirePatients<-naive(Patients)
predict(NaiveForTestPatients,h=10)
predict(NaiveEntirePatients,h=10)




SNaiveForTestPatients<-snaive(PatientsTrain,445,level=90)
plot(SNaiveForTestPatients,xlab="Year",ylab="Patient Numbers",main="Seasonal Naive Method")

accuracy(SNaiveForTestPatients)

SNaiveEntirePatients<-snaive(Patients)
predict(SNaiveEntirePatients,h=10)


#SES

SESForTestPatients<-ses(PatientsTrain,445,level = FALSE)
plot(SESForTestPatients,xlab="Year",ylab="Patient Numbers",main="SES (Single Exponential Smoothing) Method")

accuracy(SESForTestPatients)

SESEntirePatients<-ses(Patients)
predict(SESEntirePatients,h=10)


#Holt Linear Method

HoltForPatients<-holt(Patients,445,level = FALSE)
plot(HoltForPatients,xlab="Year",ylab="Patient Numbers",main="Holt Linear Method")   

HoltForTestPatients<-holt(PatientsTrain,445,level = FALSE)
plot(HoltForTestPatients,xlab="Year",ylab="Patient Numbers",main="Holt Linear Method")           

accuracy(HoltForTestPatients)

HoltEntirePatients<-holt(Patients)
predict(HoltEntirePatients,h=10)


#Holt Winters Method - NEED TO CHANGE YEARS IN AXIS
PatientsForHoltWinters<-ts(TimeSeries[,2],start = c(2015, as.numeric(format(days[1], "%j"))),
                           frequency = 24)
PatientsForHoltWintersTraining<-subset(PatientsForHoltWinters,end=1023)

HoltWintersForTestPatients<-hw(PatientsForHoltWintersTraining,445,level = FALSE)
plot(HoltWintersForTestPatients,xlab="Year",ylab="Patient Numbers",main="Holt Winters Method")

accuracy(HoltWintersForTestPatients)

HoltWintersEntirePatients<-hw(PatientsForHoltWinters)
predict(HoltWintersEntirePatients,h=10)


#Linear Regression - TRAINING SET PLOT NEEDS NEW AXIS

simple.fit = lm(Patients ~ c(1:length(Patients)))

plot(PatientsTrainingPoints,main="Plot of Linear Regression Model on Training Set",ylab="Patient Numbers",type="l")
abline(simple.fit,col="blue",lwd=3)
legend("topleft", legend=c("Linear Regression Estimator"),
       col="blue", lty=1, cex=0.45)

plot(Patients,main="Plot of Linear Model on Entire dataset",ylab="Patients",type="l")
abline(simple.fit,col="green",lwd=3)
legend("topleft", legend=c("Linear Regression Estimator"),
       col="green", lty=1, cex=0.5)


accuracy(simple.fit)
predict(simple.fit$fitted.values,h=30)


#Multiple Linear Regression
multreg<-lm(TimeSeriesCourseworkData19_20_for_multiple_linear_regression_names[,-1])
summary(multreg)
plot(Patients)
abline(multreg,col="red",lwd=2)

accuracy(multreg)
predict(multreg$fitted.values,h=30)


#ACFs and PACFs
Patientsacf<-acf(Patients,500)
Patientsacf

Patientspacf<-acf(Patients,50,type="partial")
Patientspacf

#ARIMA Models ARIMA(1,0,0)

acf(Patients,lag.max = 100)
Patientsar1<-arima(Patients,order=c(1,0,0))

pacf(Patients,lag.ax=20)

tsdiag(Patientsar1)
predict(Patientsar1$arma,h=30)
accuracy(Patientsar1)


auto.arima(Patients,seasonal=TRUE)

#Recommended ARIMA(5,1,3)

Patientsar5i1ma3<-arima(Patients,order=c(5,1,3))
tsdiag(Patientsar5i1ma3)
predict(Patientsar5i1ma3$arma,h=30)
accuracy(Patientsar5i1ma3)

#Experimental ARIMA(12,1,3) 

Patientsar12i1ma3<-arima(Patients,order=c(12,1,3))
tsdiag(Patientsar12i1ma3)
predict(Patientsar12i1ma3$arma,h=30)
accuracy(Patientsar12i1ma3)

#ARIMA with Multiple Linear Regression
arimapredictedvalues<-predict(Patientsar12i1ma3$arma,h=10)
arimapredictedvalues$fitted
arimawithmultdayregression<-c(54.40819,42.93066,44.8693,56.27423,51.79238,51.002834,52.0592,54.40821,42.93492,44.87871)
