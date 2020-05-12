setwd("D:/COVID19-WORLDWIDE")

#Importing libraries
library(dplyr)
library(DataExplorer)
library(data.table)
library(e1071)
library(corrplot)
library(RColorBrewer)
# ---------------------------Covid-19 Confirmed Cases----------------------------------------------------------

#Read Confirmed cases
confirmed <- read.csv("covid19_confirmed_global.csv", header = TRUE)
confirmed

#Replacing spaces within Country names by underscore to calculate COR properly
confirmed$Country <- gsub(" ", "_", confirmed$Country)
confirmed$Country

#Removing Lat and Long from data frame
confirmed <- within(confirmed, rm("Lat", "Long"))
confirmed

#number columns in confirmed data frame
number_of_cols_confirmed <- ncol(confirmed)

#After removing col Lat and Long, we have dates from range 2 to x and categorizing by country
confirmed <- aggregate(confirmed[,2:number_of_cols_confirmed], by=list(Category=confirmed$Country ), FUN=sum)

#Exported to excel to verify per country per row
write.csv(confirmed,"covid19_confirmed_global_unique_records_for_each_country.csv")

#Explore the overall structure of available data
plot_str(confirmed[,2:number_of_cols_confirmed])

#Statistical overview of the data w.r.t. Dates for all countries.
summary(confirmed[,2:number_of_cols_confirmed])

#Transpose the confirmed data to analyze the linear model
confirmed <- transpose(confirmed)
confirmed

#Converting Dataframe into data table
require(data.table)
confirmed <- setDT(confirmed)
confirmed

#Writing the data table in text file with sep=" ", col.names=FALSE, 
#quote=FALSE, row.names=FALSE to form proper structure for linear modelling
write.table(confirmed, "confirmed.txt", sep=" ", col.names=FALSE, quote=FALSE, row.names=FALSE)

#Reading the structures table from confirmed.txt which was created above 
confirmed <- read.table("confirmed.txt",header=TRUE, na.strings = " ")
confirmed

#Density plot of number of confirmed cases for US, China, Italy, france and India
par(mfrow=c(2, 3))
plot(density(confirmed$US), main="Confirmed : Density Plot: US", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(confirmed$US), 2))) 
polygon(density(confirmed$US), col="red")
plot(density(confirmed$China), main="Confirmed : Density Plot: China", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(confirmed$China), 2))) 
polygon(density(confirmed$China), col="red")
plot(density(confirmed$Italy), main="Confirmed : Density Plot: Italy", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(confirmed$Italy), 2))) 
polygon(density(confirmed$Italy), col="red")
plot(density(confirmed$France), main="Confirmed : Density Plot: France", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(confirmed$France), 2))) 
polygon(density(confirmed$France), col="red")
plot(density(confirmed$India), main="Confirmed : Density Plot: India", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(confirmed$France), 2))) 
polygon(density(confirmed$India), col="red")

#Correlation matrix for confirmed data
par(mfrow=c(1, 1))
confirmed_cor <- cor(confirmed[, c(177,37,85,62,79)])
corrplot(confirmed_cor, type="upper", order="hclust",
         col=brewer.pal(n=number_of_cols_confirmed, name="RdYlBu"))

#Data is very big so writing in text file to analyze
write.table(confirmed_cor,"confirmed_cor.txt")

#Adding the day column in data table to plot the confirmed cases agains days
confirmed["Day"] <- seq(1:nrow(confirmed))

par(mfrow=c(1, 4))
# Sample linear model for US and China
lm_confirmed_USChina <- lm(US ~ China, data=confirmed)

#Print linear model for US and China
lm_confirmed_USChina

#Summary for linear model for US and China
summary(lm_confirmed_USChina)

#Residual data of linear model for US and China 
res_confirmed_USChina<-residuals(lm_confirmed_USChina)
res_confirmed_USChina

#Fitted data of linear model for US and China 
fits_confirmed_USChina <-fitted(lm_confirmed_USChina)
fits_confirmed_USChina

#Residual vs. Fitted plot of linear model for US and China 
plot(res_confirmed_USChina ~ fits_confirmed_USChina)

# Sample linear model for US and Italy
lm_confirmed_USItaly<- lm(US ~ Italy, data=confirmed)

#Print linear model for US and Italy
lm_confirmed_USItaly

#Summary for linear model for US and Italy
summary(lm_confirmed_USItaly)

#Residual data of linear model for US and Italy 
res_confirmed_USItaly <-residuals(lm_confirmed_USItaly)
res_confirmed_USItaly

#Fitted data of linear model for US and Italy 
fits_confirmed_USItaly<-fitted(lm_confirmed_USItaly)
fits_confirmed_USItaly

#Residual vs. Fitted plot of linear model for US and Italy 
plot(res_confirmed_USItaly ~ fits_confirmed_USItaly)

# Sample linear model for US and France
lm_confirmed_USFrance<- lm(US ~ France, data=confirmed)

#Print linear model for US and France
lm_confirmed_USFrance

#Summary for linear model for US and France
summary(lm_confirmed_USFrance)

#Residual data of linear model for US and France 
res_confirmed_USFrance <-residuals(lm_confirmed_USFrance)
res_confirmed_USFrance

#Fitted data of linear model for US and France 
fits_confirmed_USFrance<-fitted(lm_confirmed_USFrance)
fits_confirmed_USFrance

#Residual vs. Fitted plot of linear model for US and France 
plot(res_confirmed_USFrance ~ fits_confirmed_USFrance)


# Sample linear model for US and India
lm_confirmed_USIndia <- lm(US ~ India, data=confirmed)

#Print linear model for US and India
lm_confirmed_USIndia

#Summary for linear model for US and India
summary(lm_confirmed_USIndia)

#Residual data of linear model for US and India 
res_confirmed_USIndia <-residuals(lm_confirmed_USIndia)
res_confirmed_USIndia

#Fitted data of linear model for US and India 
fits_confirmed_USIndia<-fitted(lm_confirmed_USIndia)
fits_confirmed_USIndia

#Residual vs. Fitted plot of linear model for US and India 
plot(res_confirmed_USIndia ~ fits_confirmed_USIndia)

par(mfrow=c(1, 3))
#Plot for US, China, Italy, france and India represents the graph for the number of cases w. r. t. passing days
plot(confirmed$Day,confirmed$US, ylim=c(0,100000),xlim=c(confirmed$Day[1],confirmed$Day[nrow(confirmed)]),
     type="l",col="red", xlab = "Days", ylab = "Number of confirmed Cases")
lines(confirmed$Day,confirmed$China ,col="green")
lines(confirmed$Day,confirmed$Italy ,col="blue")
lines(confirmed$Day,confirmed$France ,col="black")
lines(confirmed$Day,confirmed$India ,col="orange")
legend( "topleft", c("US", "China", "Italy", "France", "India"), 
        text.col=c("red", "green", "blue", "black", "orange"))

#Converting the confimed data into time series with ts fuction and frequency is equal to 10
confirmed_ts <- ts(runif(confirmed[,2:ncol(confirmed)], 1, ncol(confirmed[,2:ncol(confirmed)])), frequency=10)

#Decompose a confirmed time series into random, seasonal, trend and observed
confirmed_decompose <- decompose(confirmed_ts)

#plot the decomposed confirmed time series for random, seasonal, trend and observed
plot(confirmed_decompose)

# ---------------------------Covid-19 Death Cases----------------------------------------------------------

#Read deaths cases
deaths <- read.csv("covid19_deaths_global.csv", header = TRUE)
deaths

#Replacing spaces within Country names by underscore to calculate COR properly
deaths$Country <- gsub(" ", "_", deaths$Country)
deaths$Country

#Removing Lat and Long from data frame
deaths <- within(deaths, rm("Lat", "Long"))
deaths

number_of_cols_deaths <- ncol(deaths)

#After removing col Lat and Long, we have dates from range 2 to x and categorizing by country
deaths <- aggregate(deaths[,2:number_of_cols_deaths], by=list(Category=deaths$Country ), FUN=sum)

#Exported to excel 
write.csv(deaths,"covid19_deaths_global_unique_records_for_each_country.csv")

#Explore the overall structure of available data
#library(DataExplorer)
plot_str(deaths[,2:number_of_cols_deaths])

#Statistical overview of the data w.r.t. Dates for all countries.
summary(deaths[,2:number_of_cols_deaths])

#Transpose the deaths data to analyze the linear model
#library(data.table)
deaths <- transpose(deaths)
deaths

#Converting Dataframe into data table
require(data.table)
deaths <- setDT(deaths)
deaths

#Writing the data table in text file with sep=" ", col.names=FALSE, 
#quote=FALSE, row.names=FALSE to form proper structure for linear modelling
write.table(deaths, "deaths.txt", sep=" ", col.names=FALSE, quote=FALSE, row.names=FALSE)

#Reading the structures table from deaths.txt which was created above 
deaths <- read.table("deaths.txt",header=TRUE, na.strings = " ")
deaths

#Density plot of number of death cases for US, China, Italy, france and India
par(mfrow=c(2, 3)) 
plot(density(deaths$US), main="Deaths : Density Plot: US", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(deaths$US), 2))) 
polygon(density(deaths$US), col="red")
plot(density(deaths$China), main="Deaths : Density Plot: China", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(deaths$China), 2))) 
polygon(density(deaths$China), col="red")
plot(density(deaths$Italy), main="Deaths : Density Plot: Italy", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(deaths$Italy), 2))) 
polygon(density(deaths$Italy), col="red")
plot(density(deaths$France), main="Deaths : Density Plot: France", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(deaths$France), 2))) 
polygon(density(deaths$France), col="red")
plot(density(deaths$India), main="Deaths : Density Plot: India", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(deaths$France), 2))) 
polygon(density(deaths$India), col="red")

#Correlation matrix for deaths data
par(mfrow=c(1, 4))
deaths_cor <- cor(deaths[, c(177,37,85,62,79)])
corrplot(deaths_cor, type="upper", order="hclust",
         col=brewer.pal(n=number_of_cols_deaths, name="RdYlBu"))


#Data is very big so writing in text file to analyze
write.table(deaths_cor,"deaths_cor.txt")

#Adding the day column in data table to plot the deaths cases agains days
deaths["Day"] <- seq(1:nrow(deaths))

par(mfrow=c(1, 4))
# Sample linear model for US and China
lm_deaths_USChina <- lm(US ~ China, data=deaths)

#Print linear model for US and China
lm_deaths_USChina

#Summary for linear model for US and China
summary(lm_deaths_USChina)

#Residual data of linear model for US and China 
res_deaths_USChina<-residuals(lm_deaths_USChina)
res_deaths_USChina

#Fitted data of linear model for US and China 
fits_deaths_USChina <-fitted(lm_deaths_USChina)
fits_deaths_USChina

#Residual vs. Fitted plot of linear model for US and China 
plot(res_deaths_USChina ~ fits_deaths_USChina)

# Sample linear model for US and Italy
lm_deaths_USItaly<- lm(US ~ Italy, data=deaths)

#Print linear model for US and Italy
lm_deaths_USItaly

#Summary for linear model for US and Italy
summary(lm_deaths_USItaly)

#Residual data of linear model for US and Italy 
res_deaths_USItaly <-residuals(lm_deaths_USItaly)
res_deaths_USItaly

#Fitted data of linear model for US and Italy 
fits_deaths_USItaly<-fitted(lm_deaths_USItaly)
fits_deaths_USItaly

#Residual vs. Fitted plot of linear model for US and Italy 
plot(res_deaths_USItaly ~ fits_deaths_USItaly)

# Sample linear model for US and France
lm_deaths_USFrance<- lm(US ~ France, data=deaths)

#Print linear model for US and France
lm_deaths_USFrance

#Summary for linear model for US and France
summary(lm_deaths_USFrance)

#Residual data of linear model for US and France 
res_deaths_USFrance <-residuals(lm_deaths_USFrance)
res_deaths_USFrance

#Fitted data of linear model for US and France 
fits_deaths_USFrance<-fitted(lm_deaths_USFrance)
fits_deaths_USFrance

#Residual vs. Fitted plot of linear model for US and France 
plot(res_deaths_USFrance ~ fits_deaths_USFrance)


# Sample linear model for US and India
lm_deaths_USIndia<- lm(US ~ India, data=deaths)

#Print linear model for US and India
lm_deaths_USIndia

#Summary for linear model for US and India
summary(lm_deaths_USIndia)

#Residual data of linear model for US and India 
res_deaths_USIndia <-residuals(lm_deaths_USIndia)
res_deaths_USIndia

#Fitted data of linear model for US and India 
fits_deaths_USIndia<-fitted(lm_deaths_USIndia)
fits_deaths_USIndia

#Residual vs. Fitted plot of linear model for US and India 
plot(res_deaths_USIndia ~ fits_deaths_USIndia)

par(mfrow=c(1, 1))
#Plot for US, China, Italy, france and India represents the graph for the number of cases w. r. t. passing days
plot(deaths$Day,deaths$US, ylim=c(0,10000),xlim=c(deaths$Day[1],deaths$Day[nrow(deaths)]),
     type="l",col="red", xlab = "Days", ylab = "Number of deaths Cases")
lines(deaths$Day,deaths$China ,col="green")
lines(deaths$Day,deaths$Italy ,col="blue")
lines(deaths$Day,deaths$France ,col="black")
lines(deaths$Day,deaths$India ,col="orange")
legend( "topleft", c("US", "China", "Italy", "France", "India"), 
        text.col=c("red", "green", "blue", "black", "orange"))

#Converting the deaths data into time series with ts fuction and frequency is equal to 10
deaths_ts <- ts(runif(deaths[,2:ncol(deaths)], 1, ncol(deaths[,2:ncol(deaths)])), frequency=10)

#Decompose a deaths time series into random, seasonal, trend and observed
deaths_decompose <- decompose(deaths_ts)

#plot the decomposed deaths time series for random, seasonal, trend and observed
plot(deaths_decompose)

# ---------------------------Covid-19 Recovered Cases----------------------------------------------------------


#Read recovered cases
recovered <- read.csv("covid19_recovered_global.csv", header = TRUE)
recovered

#Replacing spaces within Country names by underscore to calculate COR properly
recovered$Country <- gsub(" ", "_", recovered$Country)
recovered$Country

#Removing Lat and Long from data frame
recovered <- within(recovered, rm("Lat", "Long"))
recovered

number_of_cols_recovered <- ncol(recovered)

#After removing col Lat and Long, we have dates from range 2 to x and categorizing by country
recovered <- aggregate(recovered[,2:number_of_cols_recovered], by=list(Category=recovered$Country ), FUN=sum)

#Exported to excel 
write.csv(recovered,"covid19_recovered_global_unique_records_for_each_country.csv")

#Explore the overall structure of available data
#library(DataExplorer)
plot_str(recovered[,2:number_of_cols_recovered])

#Statistical overview of the data w.r.t. Dates for all countries.
summary(recovered[,2:number_of_cols_recovered])

#Transpose the recovered data to analyze the linear model
#library(data.table)
recovered <- transpose(recovered)
recovered

#Converting Dataframe into data table
require(data.table)
recovered <- setDT(recovered)
recovered

#Writing the data table in text file with sep=" ", col.names=FALSE, 
#quote=FALSE, row.names=FALSE to form proper structure for linear modelling
write.table(recovered, "recovered.txt", sep=" ", col.names=FALSE, quote=FALSE, row.names=FALSE)

#Reading the structures table from recovered.txt which was created above 
recovered <- read.table("recovered.txt",header=TRUE, na.strings = " ")
recovered

#Density plot of number of recovered cases for US, China, Italy, france and India
par(mfrow=c(2, 3)) 
plot(density(recovered$US), main="Recovered : Density Plot: US", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(recovered$US), 2))) 
polygon(density(recovered$US), col="red")
plot(density(recovered$China), main="Recovered : Density Plot: China", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(recovered$China), 2))) 
polygon(density(recovered$China), col="red")
plot(density(recovered$Italy), main="Recovered : Density Plot: Italy", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(recovered$Italy), 2))) 
polygon(density(recovered$Italy), col="red")
plot(density(recovered$France), main="Recovered : Density Plot: France", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(recovered$France), 2))) 
polygon(density(recovered$France), col="red")
plot(density(recovered$India), main="Recovered : Density Plot: India", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(recovered$France), 2))) 
polygon(density(recovered$India), col="red")

#Correlation matrix for recovered data
par(mfrow=c(1, 4))
recovered_cor <- cor(recovered[, c(177,37,85,62,79)])
corrplot(recovered_cor, type="upper", order="hclust",
         col=brewer.pal(n=number_of_cols_recovered, name="RdYlBu"))


#Data is so big so writing in text file to analyze
write.table(recovered_cor,"recovered_cor.txt")

#Adding the day column in data table to plot the recovered cases agains days
recovered["Day"] <- seq(1:nrow(recovered))

par(mfrow=c(1, 4))
# Sample linear model for US and China
lm_recovered_USChina <- lm(US ~ China, data=recovered)

#Print linear model for US and China
lm_recovered_USChina

#Summary for linear model for US and China
summary(lm_recovered_USChina)

#Residual data of linear model for US and China 
res_recovered_USChina<-residuals(lm_recovered_USChina)
res_recovered_USChina

#Fitted data of linear model for US and China 
fits_recovered_USChina <-fitted(lm_recovered_USChina)
fits_recovered_USChina

#Residual vs. Fitted plot of linear model for US and China 
plot(res_recovered_USChina ~ fits_recovered_USChina)

# Sample linear model for US and Italy
lm_recovered_USItaly<- lm(US ~ Italy, data=recovered)

#Print linear model for US and Italy
lm_recovered_USItaly

#Summary for linear model for US and Italy
summary(lm_recovered_USItaly)

#Residual data of linear model for US and Italy 
res_recovered_USItaly <-residuals(lm_recovered_USItaly)
res_recovered_USItaly

#Fitted data of linear model for US and Italy 
fits_recovered_USItaly<-fitted(lm_recovered_USItaly)
fits_recovered_USItaly

#Residual vs. Fitted plot of linear model for US and Italy 
plot(res_recovered_USItaly ~ fits_recovered_USItaly)

# Sample linear model for US and France
lm_recovered_USFrance<- lm(US ~ France, data=recovered)

#Print linear model for US and France
lm_recovered_USFrance

#Summary for linear model for US and France
summary(lm_recovered_USFrance)

#Residual data of linear model for US and France 
res_recovered_USFrance <-residuals(lm_recovered_USFrance)
res_recovered_USFrance

#Fitted data of linear model for US and France 
fits_recovered_USFrance<-fitted(lm_recovered_USFrance)
fits_recovered_USFrance

#Residual vs. Fitted plot of linear model for US and France 
plot(res_recovered_USFrance ~ fits_recovered_USFrance)


# Sample linear model for US and India
lm_recovered_USIndia<- lm(US ~ India, data=recovered)

#Print linear model for US and India
lm_recovered_USIndia

#Summary for linear model for US and India
summary(lm_recovered_USIndia)

#Residual data of linear model for US and India 
res_recovered_USIndia <-residuals(lm_recovered_USIndia)
res_recovered_USIndia

#Fitted data of linear model for US and India 
fits_recovered_USIndia<-fitted(lm_recovered_USIndia)
fits_recovered_USIndia

#Residual vs. Fitted plot of linear model for US and India 
plot(res_recovered_USIndia ~ fits_recovered_USIndia)

par(mfrow=c(1, 1))
#Plot for US, China, Italy, france and India represents the graph for the number of cases w. r. t. passing days
plot(recovered$Day,recovered$US, ylim=c(0,100000),xlim=c(recovered$Day[1],recovered$Day[nrow(recovered)]),
     type="l",col="red", xlab = "Days", ylab = "Number of recovered Cases")
lines(recovered$Day,recovered$China ,col="green")
lines(recovered$Day,recovered$Italy ,col="blue")
lines(recovered$Day,recovered$France ,col="black")
lines(recovered$Day,recovered$India ,col="orange")
legend( "topleft", c("US", "China", "Italy", "France", "India"), 
        text.col=c("red", "green", "blue", "black", "orange"))

#Converting the deaths data into time series with ts fuction and frequency is equal to 10
recovered_ts <- ts(runif(recovered[,2:ncol(recovered)], 1, ncol(recovered[,2:ncol(recovered)])), frequency=10)

#Decompose a recovered time series into random, seasonal, trend and observed
recovered_decompose <- decompose(recovered_ts)

#plot the decomposed recovered time series for random, seasonal, trend and observed
plot(recovered_decompose)

