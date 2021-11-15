#install.packages("readxl")
###########################
# Ok, so this is an attempt to automate reading the pimco CEF UNII data and graphing it
# It download the spreadsheet and either reads and processes the whole thing, or just the last entry. 
# It should work either way, basically it calulates the date from the spreadsheet number,  1 is June, 6 is Nov, etc.
# Jan will need to be adjusted and as well see what they do next year 
# The output is dataConverted.csv
# Then use analyze.R to plot
#NOTE: seems like the web site will include the last 6 months only.  So adjust
########################

#library(quantmod)
library(readxl)
#packageVersion("readxl")
library(httr)
library(tseries)
library(lubridate)
library(dplyr)
library(shiny)
library(ggplot2)
library(shinyWidgets)

cefTickers <- list("**Muni NY-CA**","PNF","PNI","PYN","PCQ","PCK","PZC",
                   "**Municipal***","PMF","PML","PMX",
                   "***Mortgage***","PCI","PDI","PKO","PDO",
                   "**Corporation*","PCM","PTY","PFL","PFN","PHK",
                   "*****Misc*****","PGP","RCS","PCM","NRGX")
graphChoices <- list("yield","yieldNII", "NII", "roll6", "roll3","roll1","UNII","discount")

options(warn= -1)
theMonths <- c("January","February","March","April","May","June","July")
last5Months <- c("August","September","October","November","December")
#,"PZC"
chosenTickers <- c("PCQ","PCK","PMF","PML","PMX","PNF","PNI","PYN","PCM","PTY","PCN","PCI","PDI","PHK","PKO","PFL","PFN","RCS","PGP","PDO")
chosenFundNames <- c("PCM Fund", "New York Municipal Income Fund III", "New York Municipal Income Fund II", "New York Municipal Income Fund", "Municipal Income Fund III", "Municipal Income Fund II", "Municipal Income Fund", "Income Strategy Fund II", "Income Strategy Fund", "Income Opportunity Fund", "High Income Fund", "Global StocksPLUS® & Income Fund", "Energy and Tactical Credit Opportunities Fund", "Dynamic Income Opportunities Fund", "Dynamic Income Fund", "Dynamic Credit and Mortgage Income Fund", "Corporate & Income Strategy Fund", "Corporate & Income Opportunity Fund", "California Municipal Income Fund III", "California Municipal Income Fund II", "California Municipal Income Fund")

getDiscount <- function(){
  df <- read.csv("PimcoFunds.csv")
  df <- data.frame(df$Symbol,df$Premium...Discount..)
  names(df) <- c("Ticker","discount")
  dfDiscount <- df[df$Ticker!="",]
  return(dfDiscount)
}

getNAV <- function(){
  df <- read.csv("PimcoFunds.csv",stringsAsFactors=FALSE,strip.white=T,blank.lines.skip = T)
  df <- data.frame(df$Symbol,df$Daily)
  names(df) <- c("Ticker","NAV")
  df <- df[df$Ticker!="",]
  df$NAV <- as.numeric(as.character(df$NAV))
  return(df)
}


getPimcoData <- function(chosenTickers) {
  #https://stackoverflow.com/questions/41368628/read-excel-file-from-a-url-using-the-readxl-package

  urlFromPimco <- "https://www.pimco.com/handlers/displaydocument.ashx?c=72201Y101&wd=UNII%20Report&fn=UNII%20Website%20File.xlsx&id=rc%2bNQHWWEucpe9Pl8GR4ZuchIBLSukAip55SJ6LR8CvmbW1h2ScLKkOI8wUGjjRVlqtvi4F2FCctG85Wa0WChwV9Lr8dxP8l9utktFaxMqbNE%2fu%2fmFZlP0en3TVZekc0GzTgcCj1yX2gpNL1AZPRb5eQpeObnFEmUNlpVN3DAYZ%2fCssLsoXU1FoLc793R4xHAmyp5THHNfyhxlsD%2fW8Opbmz04NLRE67WEWXOV0pe7cybRxYhZW585hPm3qR7xciSwUdtbMgrGhXmRzLL7Wl7588zkPbqsJjbKG953mv6J8ylrComp7qNhTVsS3tineeO4CmeqqcvbnzENMOKXLeQu1SoejRODtCrcOXbJyn1HEtHPyGI%2fZvbM7dtLOGdEhQGzVX9wd7xtNk9vUATKjZnUheCFYLOZcG9ENqjMVY4zwhm1ZzsR3bTIXOCIeVKtsA"
  GET(urlFromPimco, write_disk(tf <- tempfile()))
  sheetMonths <- excel_sheets(tf)
  numberOfSheets <- length(sheetMonths)
  #set the names from Pimco to these
  n <- c("FundName", "Ticker", "CurrentFiscal", "NII", "UNII", "MonthlyDistribution", "Rolling3Mon", "Rolling6Mon", "FiscalYear")
  data <- data.frame()
  
  currentYear <- as.numeric(format(Sys.Date(), format="%Y"))
  pastYear <- currentYear - 1
  currentMonth <- as.numeric(format(Sys.Date(), format="%m"))
  
  for (i in 1:numberOfSheets) {
    dfTibble <-  read_xlsx(tf, skip=10, sheet=i, col_names=as.character(n), col_types = c("text","text","date","numeric","numeric","numeric","numeric","numeric","date"))
    dfTibble <- dfTibble[dfTibble$Ticker %in% chosenTickers,]
    df <- data.frame(dfTibble)
    #check for months of last year
    theYear <- currentYear
    if ((currentMonth < 6) & (sheetMonths[i] %in% last5Months))
      theYear <- pastYear
    df$date <- as.Date(paste(sheetMonths[i],"-","15","-",as.character(theYear), sep=""), format = "%b-%d-%Y")
    
     for (j in 1:nrow(df)) { 
        marks <- get.hist.quote(as.character(df[j,"Ticker"]), start=df[j,"date"],quote=c("Open"), quiet=T)
      #  fundNames <- get.hist
        df[j,"Mark"] <- marks[1]
        df[j,"markLast"] <- marks[nrow(marks)]
        }
  data <- rbind(data, df)
}

#******************
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
data <- as.data.frame(data)

dfNAV <- getNAV()
data <- merge(data,dfNAV,by="Ticker")

data$discount <- (data$Mark-data$NAV)/data$NAV
return(data)
#######
#https://readxl.tidyverse.org/articles/articles/readxl-workflows.html
}

x <- getPimcoData(chosenTickers)
