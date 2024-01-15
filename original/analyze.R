library("ggplot2")
library(scales)
library(dplyr)
source("../sort.data.frame.R")
#analyze data


data <- read.csv("dataWithMarkAll.csv")
data$CurrentFiscal <- as.Date(data$CurrentFiscal)
data$date <- as.Date(data$date)
#calculate monthly yield, NII-prevNII/dividend
#data$newFiscal <- format(data$date,"Y")==format(lag(data$date),"Y")
data <- data %>%
  group_by(Ticker) %>%
  arrange(date) %>%
  mutate(diff = NII - lag(NII, default = first(NII)))


#determine if a new fiscal year has passed.  If so this will change the calculation for the NII change, as it is FY cummulative
data <- data %>%
  group_by(Ticker) %>%
  arrange(date) %>%
  mutate(newFY = format(CurrentFiscal,"%Y")!=format(lag(CurrentFiscal, default = first(CurrentFiscal)),"%Y"))

#now correct those diff with a newFiscalYear to be just the current NII
data[data$newFY,]$diff <- data[data$newFY,]$NII


#True rolling average:
data$Rolling1Mon <- data$diff/data$MonthlyDistribution
data$yield1Month <- (data$diff*12)/data$Mark
data$yield <- 12 * data$MonthlyDistribution / data$Mark
data[data$Rolling1Mon<0,"Rolling1Mon"] <- NA
#subset only
PCIetc <- c("PCK","PNI","PGP","PDO")
#PCIetc <- c("PCK")
#PCIetc <- c("PGP","PDO","PNI","PCK")
#onlyPCI <- c("PCI")
dataSubset <- data[data$Ticker %in% PCIetc,]
#dataSubset <- data
#remove remove remove!!!!!!!!!
#dataSubset[dataSubset$Ticker=="PDO",]$Rolling3Mon <- 1
dataSubset[dataSubset$Ticker=="PDO",]$Rolling6Mon <- 1
dataP <- dataSubset

#plot UNII as percentage of Dividend
#UNII
ggplot(dataP, aes(x=date, y=UNII/MonthlyDistribution, group=Ticker)) + geom_line(aes(colour=Ticker)) + ggtitle("UNII extra months")


#Total 1 month yield by nii
ggplot(dataP, aes(x=date, y=Rolling1Mon, group=Ticker)) + geom_line(aes(colour=Ticker))+
  scale_y_continuous(labels = function(x) paste0(x * 100, '%')) + ggtitle("Div Coverage based on NII")

 #3Mon
ggplot(dataP, aes(x=date, y=Rolling3Mon, group=Ticker)) + geom_line(aes(colour=Ticker))+
  scale_y_continuous(labels = function(x) paste0(x * 100, '%'))+ ggtitle("Rolling3Mon")
#6M
ggplot(dataP, aes(x=date, y=Rolling6Mon, group=Ticker)) + geom_line(aes(colour=Ticker))+
  scale_y_continuous(labels = function(x) paste0(x * 100, '%'))+ ggtitle("Rolling6Mon")
###########NII
ggplot(dataP, aes(x=date, y=NII, group=Ticker)) + geom_line(aes(colour=Ticker))+ ggtitle("NII")
#sinle month yield
ggplot(dataP, aes(x=date, y=yield1Month, group=Ticker)) + geom_line(aes(colour=Ticker))+
  scale_y_continuous(labels = function(x) paste0(x * 100, '%'))+ ggtitle("1 Month Yield using NII change")
#Yield
ggplot(dataP, aes(x=date, y=yield, group=Ticker)) + geom_line(aes(colour=Ticker))+
  scale_y_continuous(labels = function(x) paste0(x * 100, '%'))+ ggtitle("Yield Actual")

unii <- as.data.frame(subset(dataP, select=c(Ticker, UNII, yield1Month)))
unii
# #not that useful
# data <- as.data.frame(data)
# hightestYield <- sort.data.frame(data, by=~yield,decrease=T)
# lowestYield <- sort.data.frame(data, by=~yield,decrease=F)
# highest1Mon <- sort.data.frame(data, by=~rolling1Mon,decrease=T)




