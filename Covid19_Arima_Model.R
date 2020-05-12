#----------Predicting confirmed cases in USA for next 120 days using Arima Model-------------#

setwd("/Users/elijaheniola/documents/dataset")

#Importing libraries
library(data.table)
library(tseries)
library(forecast)


covidConfirmed<-read.csv("covid19_confirmed_global.csv")
covidDeaths<-read.csv("covid19_deaths_global.csv")
covidRecovered<-read.csv("covid19_recovered_global.csv")

#Preparing US confirmed cases dataset

covidConfirmed <- covidConfirmed[c(-1,-2,-3, -4)]
covidConfirmed

covidConfirmedUS <- covidConfirmed[226,]
covidConfirmedUS


confirmed <- transpose(covidConfirmedUS)
confirmed

#rownames(confirmed) <- colnames(covidConfirmedUS)
colnames(confirmed) <- rownames(covidConfirmedUS)
confirmed

colnames(confirmed)[colnames(confirmed) == "226"] <- "confirmedCases"
confirmed

summary(confirmed)


#converting our dataset to a time series format
confirmed<-ts(confirmed)
confirmed

plot.ts(confirmed, xlab="Time in days")

class(confirmed)
frequency(confirmed)
summary(confirmed)
plot(confirmed, xlab="Time in days")
abline(reg=lm(confirmed~time(confirmed)))
cycle(confirmed)
plot(aggregate(confirmed,FUN=mean), xlab="Time in days")

boxplot(confirmed~cycle(confirmed))

# Determining that our time series is stationary enough to do any form of time series modelling 
# Augmented Dickey Fullers Test
adf.test(diff(log(confirmed)), alternative="stationary", k=1)

acf(log(confirmed))

acf(diff(log(confirmed)))

pacf(diff(log(confirmed)))

(fit <- arima(log(confirmed), c(0, 1, 1),seasonal = list(order = c(0, 1, 1), period = 1)))

# Prediction using manual p,d,q values
pred <- predict(fit, n.ahead = 120*1)
ts.plot(confirmed,2.718^pred$pred, log = "y", lty = c(1,3), ylab="Confirmed Cases", xlab="Time in days")

# Using auto arima to get best values for p, d, q
ourModelConfirmed <- auto.arima(confirmed)
ourModelConfirmed

auto.arima(confirmed, ic="aic", trace=TRUE)

#adf.test(myModel)
plot.ts(ourModelConfirmed$residuals)

acf(ts(ourModelConfirmed$residuals), main = 'ACF Residual')
pacf(ts(ourModelConfirmed$residuals), main = 'PACF Residual')

# Using the model to forecast for the next 120 days
ourForecast<-forecast(ourModelConfirmed, level=c(95), h=120*1)
plot(ourForecast, ylab="Confirmed Cases", xlab="Time in days")

# Testing our model
Box.test(ourModelConfirmed$residuals, lag=5, type="Ljung-Box")
Box.test(ourModelConfirmed$residuals, lag=10, type="Ljung-Box")
Box.test(ourModelConfirmed$residuals, lag=15, type="Ljung-Box")

#---------------------------------------------------------------------------

#Preparing US deaths cases dataset

covidDeaths <- covidDeaths[c(-1,-2,-3)]
covidDeaths

covidDeathsUS <- covidDeaths[226,]
covidDeathsUS

deaths <- transpose(covidDeathsUS)
deaths

#rownames(confirmed) <- colnames(covidConfirmedUS)
colnames(deaths) <- rownames(covidDeathsUS)
deaths

colnames(deaths)[colnames(deaths) == "226"] <- "deathCases"
deaths
summary(deaths)

#converting our dataset to a time series format
deaths<-ts(deaths)
deaths

plot.ts(deaths)

class(deaths)
frequency(deaths)
summary(deaths)
plot(deaths)
abline(reg=lm(deaths~time(deaths)))
cycle(deaths)
plot(aggregate(deaths,FUN=mean))

boxplot(deaths~cycle(deaths))

# Determining that our time series is stationary enough to do any form of time series modelling 
# Augmented Dickey Fullers Test
adf.test(diff(log(deaths)), alternative="stationary", k=1)

acf(log(deaths))

# Using auto arima to get best values for p, d, q
ourModelDeaths <- auto.arima(deaths)
ourModelDeaths

auto.arima(ourModelDeaths, ic="aic", trace=TRUE)

plot.ts(ourModelDeaths$residuals)

acf(ts(ourModelDeaths$residuals), main = 'ACF Residual')
pacf(ts(ourModelDeaths$residuals), main = 'PACF Residual')

# Using the model to forecast for the next 120 days
ourForecast<-forecast(ourModelDeaths, level=c(95), h=120*1)
plot(ourForecast,ylab="Death Cases", xlab="Time in days" )

# Testing our model
Box.test(ourModelDeaths$residuals, lag=5, type="Ljung-Box")
Box.test(ourModelDeaths$residuals, lag=10, type="Ljung-Box")
Box.test(ourModelDeaths$residuals, lag=15, type="Ljung-Box")


#-------------------------------------------------------------------

#Preparing US recovered cases dataset

covidRecovered <- covidRecovered[c(-1,-2,-3)]
covidRecovered

covidRecoveredUS <- covidRecovered[226,]
covidRecoveredUS


recovered <- transpose(covidRecoveredUS)
recovered

#rownames(recovered) <- colnames(covidRecoveredUS)
colnames(recovered) <- rownames(covidRecoveredUS)
recovered

colnames(recovered)[colnames(recovered) == "226"] <- "recoveredCases"
recovered
summary(recovered)

#converting our dataset to a time series format
recovered<-ts(recovered)
recovered

plot.ts(recovered)


class(recovered)
frequency(recovered)
summary(recovered)
plot(recovered)
abline(reg=lm(recovered~time(recovered)))
cycle(recovered)
plot(aggregate(recovered,FUN=mean))

boxplot(deaths~cycle(recovered))


# Determining that our time series is stationary enough to do any form of time series modelling 
# Augmented Dickey Fullers Test
adf.test(diff(log(recovered)), alternative="stationary", k=1)

acf(log(recovered))

# Using auto arima to get best values for p, d, q
ourModelRecovered<- auto.arima(recovered)
ourModelRecovered

auto.arima(ourModelRecovered, ic="aic", trace=TRUE)

plot.ts(ourModelRecovered$residuals)

acf(ts(ourModelRecovered$residuals), main = 'ACF Residual')
pacf(ts(ourModelDeaths$residuals), main = 'PACF Residual')

# Using the model to forecast for the next 120 days
ourForecastRecov<-forecast(ourModelDeaths, level=c(95), h=120*1)
plot(ourForecastRecov, ylab="Confirmed Cases", xlab="Time in days")

# Testing our model
Box.test(ourModelRecovered$residuals, lag=5, type="Ljung-Box")
Box.test(ourModelRecovered$residuals, lag=10, type="Ljung-Box")
Box.test(ourModelRecovered$residuals, lag=15, type="Ljung-Box")


#Binding the datasets for confirmed cases, deaths and recovered cases
ncol(confirmed)
ncol(deaths)
ncol(recovered)
confirmed_Deaths <- cbind(confirmed,deaths)
confirmed_Deaths
confirmed_Deaths_Recovered <- cbind(confirmed_Deaths,recovered)
confirmed_Deaths_Recovered


#-----Predicting confirmed, deaths and recovered cases for the top 13 countries using Arima Model-----#

setwd("C:/datamining_classExamples")

# importing libraries
library(car)
library(dplyr)
library(plyr)
library(tseries)
library(zoo)
library(forecast )

# importing data of confirmed, recovered and death time series for all countries.
# The appropriate paths has been provided where the dataset resides
confirmed=read.csv('covid19_confirmed_global.csv',
                   header = F,stringsAsFactors = F)
death=read.csv('covid19_deaths_global.csv',header=F,stringsAsFactors = F)
recovered=read.csv('covid19_recovered_global.csv',header=F,stringsAsFactors = F)

# looking at the first five rows of the confirmed data set
head(confirmed)

# seeing that the V2 and V3 columns are longitude and latitude which are not useful so
# making latitude and longitude as null. 
# this implies that those columns are being removed from all three datasets
confirmed$V2<-NULL
confirmed$V3<-NULL
recovered$V2<-NULL
recovered$V3<-NULL
death$V2<-NULL
death$V3<-NULL

# The first row contains dates. Removing first row of names and date after noting the start and end
# This has been done for all the three datasets
confirmed=confirmed[2:nrow(confirmed),]
recovered=recovered[2:nrow(recovered),]
death=death[2:nrow(death),]


# The code below converts to numeric the values of columns starting from column 2 to the last colum
for (j in 2:ncol(confirmed)){ # select from column 2 onwards
  confirmed[,j]=as.numeric(confirmed[,j]) # convert to numeric
  recovered[,j]=as.numeric(recovered[,j]) # convert to numeric
  death[,j]=as.numeric(death[,j]) # convert to numeric
}

# V1 is the country name. So for each country name, the subset of the data has been selected below and 
# the columns for each day have been summed for that subset.
# This gives the data for that day for that country as an aggregate
# This has been done for all the three datasets below.
data_confirmed=ddply(confirmed,'V1',function(x){colSums(x[,2:ncol(x)],na.rm = T)})
data_recovered=ddply(recovered,'V1',function(x){colSums(x[,2:ncol(x)],na.rm = T)})
data_death=ddply(death,'V1',function(x){colSums(x[,2:ncol(x)],na.rm = T)})


# getting data for US
data_US_confirmed=data_confirmed%>% filter(V1=='US') # V1 is country, so US is being selected
data_US_death=data_death%>% filter(V1=='US')# V1 is country, so US is being selected
data_US_recovered=data_recovered%>% filter(V1=='US')# V1 is country, so US is being selected

# getting data for India
data_India_confirmed=data_confirmed%>% filter(V1=='India')# V1 is country, so India is being selected
data_India_death=data_death%>% filter(V1=='India')# V1 is country, so India is being selected
data_India_recovered=data_recovered%>% filter(V1=='India')# V1 is country, so India is being selected

# getting data for Canada
data_Canada_confirmed=data_confirmed%>% filter(V1=='Canada') # V1 is country, so Canada is being selected
data_Canada_death=data_death%>% filter(V1=='Canada') # V1 is country, so Canada is being selected
data_Canada_recovered=data_recovered%>% filter(V1=='Canada') # V1 is country, so Canada is being selected

# data for China
data_China_confirmed=data_confirmed%>% filter(V1=='China') # V1 is country, so China is being selected
data_China_death=data_death%>% filter(V1=='China')# V1 is country, so China is being selected
data_China_recovered=data_recovered%>% filter(V1=='China')# V1 is country, so China is being selected

# data for france
data_france_confirmed=data_confirmed%>% filter(V1=='France')# V1 is country, so France is being selected
data_france_death=data_death%>% filter(V1=='France')# V1 is country, so France is being selected
data_france_recovered=data_recovered%>% filter(V1=='France')# V1 is country, so France is being selected

# data for Italy
data_Italy_confirmed=data_confirmed%>% filter(V1=='Italy')# V1 is country, so Italy is being selected
data_Italy_death=data_death%>% filter(V1=='Italy')# V1 is country, so Italy is being selected
data_Italy_recovered=data_recovered%>% filter(V1=='Italy')# V1 is country, so Italy is being selected

# data for Japan
data_Japan_confirmed=data_confirmed%>% filter(V1=='Japan')# V1 is country, so Japan is being selected
data_Japan_death=data_death%>% filter(V1=='Japan')# V1 is country, so Japan is being selected
data_Japan_recovered=data_recovered%>% filter(V1=='Japan')# V1 is country, so Japan is being selected

# Data for South Korea
data_Skorea_confirmed=data_confirmed%>% filter(V1=="Korea, South")# V1 is country, so "Korea, South" is being selected
data_Skorea_death=data_death%>% filter(V1=="Korea, South")# V1 is country, so "Korea, South" is being selected
data_Skorea_recovered=data_recovered%>% filter(V1=="Korea, South")# V1 is country, so "Korea, South" is being selected

# Data for Spain
data_Spain_confirmed=data_confirmed%>% filter(V1=="Spain")# V1 is country, so Spain is being selected
data_Spain_death=data_death%>% filter(V1=="Spain")# V1 is country, so Spain is being selected
data_Spain_recovered=data_recovered%>% filter(V1=="Spain")# V1 is country, so Spain is being selected

# Data for "United Arab Emirates"
data_UAE_confirmed=data_confirmed%>% filter(V1=="United Arab Emirates")# V1 is country, so UAE is being selected
data_UAE_death=data_death%>% filter(V1=="United Arab Emirates")# V1 is country, so UAE is being selected
data_UAE_recovered=data_recovered%>% filter(V1=="United Arab Emirates")# V1 is country, so UAE is being selected

# Data for UK
data_UK_confirmed=data_confirmed%>% filter(V1=="United Kingdom")# V1 is country, so UK is being selected
data_UK_death=data_death%>% filter(V1=="United Kingdom")# V1 is country, so UK is being selected
data_UK_recovered=data_recovered%>% filter(V1=="United Kingdom")# V1 is country, so UK is being selected


# Summing all the columns from second column onwards to get data each day for the world as an aggregate
# and creating a time series starting 22-01-2020 and ending in 07-04-2020
# This has been done using the zoo package

confirmed_world=zoo(as.vector(colSums(data_confirmed[,2:ncol(data_confirmed)])),
                    seq(from =as.Date("22-01-2020",format='%d-%m-%Y') ,
                        to =as.Date("07-04-2020",format='%d-%m-%Y') ,by=1 )) # confirmed in world
recovered_world=zoo(as.vector(colSums(data_recovered[,2:ncol(data_recovered)])),
                    seq(from =as.Date("22-01-2020",format='%d-%m-%Y') ,
                        to =as.Date("07-04-2020",format='%d-%m-%Y') ,by=1 ))#recovered in world
death_world=zoo(as.vector(colSums(data_death[,2:ncol(data_death)])),
                seq(from =as.Date("22-01-2020",format='%d-%m-%Y') ,
                    to =as.Date("07-04-2020",format='%d-%m-%Y') ,by=1 )) #death in the world

# calculating a seris of dates from 22-01 to 07-04 
dates=seq(from =as.Date("22-01-2020",format='%d-%m-%Y') ,
          to =as.Date("07-04-2020",format='%d-%m-%Y') ,by=1 )


par(mfrow=c(1,3)) # to plot three plots in same plot
# plotting confirmed cases in US
plot(y=data_US_confirmed[,2:ncol(data_US_confirmed)],type='o',
     x=dates,cex=0.4,main='Confirmed cases : US',xlab='Date',ylab ='confirmed cases')
# plotting recovered cases in US
plot(y=data_US_recovered[,2:ncol(data_US_recovered)],type='o',
     x=dates,cex=0.4,main='Recovered cases : US',xlab='Date',ylab ='recovered cases')
# plotting dead cases in US
plot(y=data_US_death[,2:ncol(data_US_death)],type='o',
     x=dates,cex=0.4,main='Dead cases: US',xlab='Date',ylab ='dead cases')

par(mfrow=c(1,3)) # to plot three plots in same plot
# plotting confirmed cases in India
plot(y=data_India_confirmed[,2:ncol(data_India_confirmed)],type='o',
     x=dates,cex=0.4,main='Confirmed cases : India',xlab='Date',ylab ='confirmed cases')
# plotting recovered cases in India
plot(y=data_India_recovered[,2:ncol(data_India_recovered)],type='o',
     x=dates,cex=0.4,main='Recovered cases : India',xlab='Date',ylab ='recovered cases')
# plotting dead cases in India
plot(y=data_India_death[,2:ncol(data_India_death)],type='o',
     x=dates,cex=0.4,main='Dead cases: India',xlab='Date',ylab ='dead cases')

par(mfrow=c(1,3)) # to plot three plots in same plot
# plotting confirmed cases in Canada
plot(y=data_Canada_confirmed[,2:ncol(data_Canada_confirmed)],type='o',
     x=dates,cex=0.4,main='Confirmed cases : Canada',xlab='Date',ylab ='confirmed cases')
# plotting recovered cases in Canada
plot(y=data_Canada_recovered[,2:ncol(data_Canada_recovered)],type='o',
     x=dates,cex=0.4,main='Recovered cases : Canada',xlab='Date',ylab ='recovered cases')
# plotting dead cases in Canada
plot(y=data_Canada_death[,2:ncol(data_Canada_death)],type='o',
     x=dates,cex=0.4,main='Dead cases : Canada',xlab='Date',ylab ='dead cases')

par(mfrow=c(1,3)) # to plot three plots in same plot
# plotting confirmed cases in China
plot(y=data_China_confirmed[,2:ncol(data_China_confirmed)],type='o',
     x=dates,cex=0.4,main='Confirmed cases : China',xlab='Date',ylab ='confirmed cases')
# plotting recovered cases in China
plot(y=data_China_recovered[,2:ncol(data_China_recovered)],type='o',
     x=dates,cex=0.4,main='Recovered cases : China',xlab='Date',ylab ='recovered cases')
# plotting dead cases in China
plot(y=data_China_death[,2:ncol(data_China_death)],type='o',
     x=dates,cex=0.4,main='Dead cases : China',xlab='Date',ylab ='dead cases')

par(mfrow=c(1,3)) # to plot three plots in same plot
# plotting confirmed cases in France
plot(y=data_france_confirmed[,2:ncol(data_france_confirmed)],type='o',
     x=dates,cex=0.4,main='Confirmed cases : France',xlab='Date',ylab ='confirmed cases')
# plotting recovered cases in France
plot(y=data_france_recovered[,2:ncol(data_france_recovered)],type='o',
     x=dates,cex=0.4,main='Recovered cases : France',xlab='Date',ylab ='recovered cases')
# plotting dead cases in France
plot(y=data_france_death[,2:ncol(data_france_death)],type='o',
     x=dates,cex=0.4,main='Dead cases : France',xlab='Date',ylab ='dead cases')

par(mfrow=c(1,3)) # to plot three plots in same plot
# plotting confirmed cases in Italy
plot(y=data_Italy_confirmed[,2:ncol(data_Italy_confirmed)],type='o',
     x=dates,cex=0.4,main='Confirmed cases : Italy',xlab='Date',ylab ='confirmed cases')
# plotting recovered cases in Italy
plot(y=data_Italy_recovered[,2:ncol(data_Italy_recovered)],type='o',
     x=dates,cex=0.4,main='Recovered cases : Italy',xlab='Date',ylab ='recovered cases')
# plotting dead cases in Italy
plot(y=data_Italy_death[,2:ncol(data_Italy_death)],type='o',
     x=dates,cex=0.4,main='Dead cases : Italy',xlab='Date',ylab ='dead cases')

par(mfrow=c(1,3)) # to plot three plots in same plot
# plotting confirmed cases in Japan
plot(y=data_Japan_confirmed[,2:ncol(data_Japan_confirmed)],type='o',
     x=dates,cex=0.4,main='Confirmed cases: Japan',xlab='Date',ylab ='confirmed cases')
# plotting recovered cases in Japan
plot(y=data_Japan_recovered[,2:ncol(data_Japan_recovered)],type='o',
     x=dates,cex=0.4,main='Recovered cases : Japan',xlab='Date',ylab ='recovered cases')
# plotting dead cases in Japan
plot(y=data_Japan_death[,2:ncol(data_Japan_death)],type='o',
     x=dates,cex=0.4,main='Dead cases : Japan',xlab='Date',ylab ='dead cases')

par(mfrow=c(1,3)) # to plot three plots in same plot
# plotting confirmed cases in South Korea
plot(y=data_Skorea_confirmed[,2:ncol(data_Skorea_confirmed)],type='o',
     x=dates,cex=0.4,main='Confirmed cases : S. Korea',xlab='Date',ylab ='confirmed cases')
# plotting recovered cases in South Korea
plot(y=data_Skorea_recovered[,2:ncol(data_Skorea_recovered)],type='o',
     x=dates,cex=0.4,main='Recovered cases : S. Korea',xlab='Date',ylab ='recovered cases')
# plotting dead cases in South Korea
plot(y=data_Skorea_death[,2:ncol(data_Skorea_death)],type='o',
     x=dates,cex=0.4,main='Dead cases : S. Korea',xlab='Date',ylab ='dead cases')

par(mfrow=c(1,3)) # to plot three plots in same plot
# plotting confirmed cases in Spain
plot(y=data_Spain_confirmed[,2:ncol(data_Spain_confirmed)],type='o',
     x=dates,cex=0.4,main='Confirmed cases : Spain',xlab='Date',ylab ='confirmed cases')
# plotting recovered cases in Spain
plot(y=data_Spain_recovered[,2:ncol(data_Spain_recovered)],type='o',
     x=dates,cex=0.4,main='Recovered cases : Spain',xlab='Date',ylab ='recovered cases')
# plotting dead cases in Spain
plot(y=data_Spain_death[,2:ncol(data_Spain_death)],type='o',
     x=dates,cex=0.4,main='Dead cases : Spain',xlab='Date',ylab ='dead cases')

par(mfrow=c(1,3)) # to plot three plots in same plot
# plotting confirmed cases in UAE
plot(y=data_UAE_confirmed[,2:ncol(data_UAE_confirmed)],type='o',
     x=dates,cex=0.4,main='Confirmed cases : UAE',xlab='Date',ylab ='confirmed cases')
# plotting recovered cases in UAE
plot(y=data_UAE_recovered[,2:ncol(data_UAE_recovered)],type='o',
     x=dates,cex=0.4,main='Recovered cases: UAE',xlab='Date',ylab ='recovered cases')
# plotting dead cases in UAE
plot(y=data_UAE_death[,2:ncol(data_UAE_death)],type='o',
     x=dates,cex=0.4,main='Dead cases: UAE',xlab='Date',ylab ='dead cases')

par(mfrow=c(1,3)) # to plot three plots in same plot
# plotting confirmed cases in Uk
plot(y=data_UK_confirmed[,2:ncol(data_UK_confirmed)],type='o',
     x=dates,cex=0.4,main='Confirmed cases: UK',xlab='Date',ylab ='confirmed cases')
# plotting recovered cases in US
plot(y=data_UK_recovered[,2:ncol(data_UK_confirmed)],type='o',
     x=dates,cex=0.4,main='Recovered cases : UK',xlab='Date',ylab ='recovered cases')
# plotting dead cases in UK
plot(y=data_UK_death[,2:ncol(data_UK_confirmed)],type='o',
     x=dates,cex=0.4,main='Dead cases : UK',xlab='Date',ylab ='dead cases')


# seeing that all the values show exponential pattern, the models will be fit on their logarithms
# function to make plots
plot_value<-function(values,name_tag){
  par(mfrow=c(2,2))
  (plot(values,type='o',cex=0.4,main=paste('Plot of frequency of ',name_tag),xlab='Date',ylab =j))
  values=log(values) # converting to logarithm
  #Plot of log of time series
  (plot(values,type='o',cex=0.4,main=paste('Plot of log frequency of ',name_tag),xlab='Date',ylab =j))
  # ACF and PACF plots
  (acf(values,main=paste('ACF of log frequency of ',name_tag),lag.max = 60))
  pacf(values,main=paste('PACF of log frequency of ',name_tag))
}

# function to fit ARIMA model and return diagnostics
fit_model<-function(values,order_arima){
  # converting to log values
  values=log(values)
  # fitting an arima log time series
  mod<-arima(values,order = order_arima)
  print(mod)
  # plotting the model in a 2x2 window
  par(mfrow=c(2,2))
  # plotting residuals from the model
  plot(mod$residuals,ylab='residuals',xaxt='n',main='plot of residuals')
  # giving x axis labels i.e. the date values
  axis(1, at=seq(1,length(mod$residuals),12.5) , las=2, cex.axis=0.6,
       labels=seq(as.Date("22-01-2020",format='%d-%m-%Y'), as.Date("22-01-2020",format='%d-%m-%Y')+length(mod$residuals)-1, length.out=7) )
  
  
  # plotting the forecasted 30 days ahead value
  plot(forecast(mod,h = 30),xaxt='n')
  # giving x axis labels i.e. the date values
  axis(1, at=seq(1,length(mod$residuals)+30,17.5) , las=2, cex.axis=0.6,
       labels=seq(as.Date("22-01-2020",format='%d-%m-%Y'), as.Date("22-01-2020",format='%d-%m-%Y')+length(mod$residuals)+30-1, length.out=7) )
  
  acf(mod$residuals,main='ACF of residuals',ylab='residuals') # ACF of residuals
  pacf(mod$residuals,main='PACF of residuals',ylab='residuals') # PACF of residuals
  
  #Durbin Watson test for stationarity check
  print('Durbin Watson statistic is :')
  print(durbinWatsonTest(as.vector(mod$residuals)))
}


## drag the plot window since it has large margins we would not be able to see the plots untill we increased it to great extent. The key is to increase the plot window beforehand and then run the lines of code.

# plot for confirmed cases in world
plot_value(confirmed_world,'confirmed world')
# fitting the ARIMA model of order (1,1,1) and getting diagnostics 
fit_model(confirmed_world,c(1,1,1)) 
# where first 1 indicates that it sets the lage value of 
#1 for auto regressions,uses a difference of 1 to make time series stationary
# and uses moving model of 1

# plot for recovered cases in world
plot_value(recovered_world,'recovered world')
# fitting the ARIMA model of order (1,1,4) and getting diagnostics 
fit_model(recovered_world,c(1,1,4))

# plot for death cases in world
plot_value(death_world,'recovered world')
# fitting the ARIMA model of order (1,1,3) and getting diagnostics 
fit_model(death_world,c(1,1,3))




