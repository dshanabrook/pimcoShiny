#install.packages("readxl")
#install.packages("ggrepel")
###########################
# Ok, so this is an attempt to automate reading the pimco CEF UNII data and graphing it
# It download the spreadsheet and either reads and processes the whole thing, or just the last entry. 
# It should work either way, basically it calulates the date from the spreadsheet number,  1 is June, 6 is Nov, etc.
# Jan will need to be adjusted and as well see what they do next year 
# The output is dataConverted.csv
# Then use analyze.R to plot
#NOTE: seems like the web site will include the last 6 months only.  So adjust
########################
#gets the current NAV, discount.  
#download excel file from Pimco 
#Open with excel! or convert with https://www.zamzar.com then save as xsl
#move to PimcoFunds.xsl in shiny
#
#another method, which is not being used:
#open with numbers, export as .tsv
#move to shinyPimco and save as PimcoFunds.tsv (not sure about suffix)
#the read statment will start on the correct row, and read everyother row, so the fact that the 
#excel file is messed up won't matter.  
#you must have the correct number of funds, or it will be truncated
##############################
#install.packages("quantmod")
#library(quantmod)
library("readxl")
library("ggrepel")
#packageVersion("readxl")
library("httr")
library("tseries")
library("lubridate")
library("dplyr")
library("shiny")
library("ggplot2")
library("shinyWidgets")
pimcoCurrenttsv <- ("PimcoFunds.tsv")

cefTickers <- list("**Muni NY-CA**","PNF","PNI","PYN","PCQ","PCK","PZC",
                   "**Municipal***","PMF","PML","PMX",
                   "***Mortgage***","PDO","PDI","PAXS",
                   "**Corporation*","PCM","PTY","PFL","PFN","PHK",
                   "*****Misc*****","PGP","RCS","PCM","NRGX")
cefMuni2 <- list("PNF","PNI","PYN","PCQ","PCK","PZC")
cefMuni <- list("PMF","PML","PMX")
cefMort <- list("PDI","PDO","PAXS")
cefCorp <- list("PCM","PTY","PFL","PFN","PHK")
cefMisc <- list("PGP","RCS","PCM","NRGX")
graphChoices <- list("Yield","NII yield", "NII", "roll6", "roll3","roll1","UNII","discount/NII","discount/yield")
mostRecentData <- data.frame()

options(warn= -1)

theMonths <- c("January","February","March","April","May","June","July","August","September","October","November","December")
newFormatMonths <- c("October","November","December", "January","February")
lastYearsMonths <- c("November","December")
  
chosenTickers <- c("PCQ","PCK","PMF","PML","PMX","PNF","PNI","PYN","PCM","PTY","PCN","PHK","PKO","PFL","PFN","RCS","PGP","PDO")
testTickers <- c( "PDO")
chosenFundNames <- c("PCM Fund", "New York Municipal Income Fund III", "New York Municipal Income Fund II", "New York Municipal Income Fund", "Municipal Income Fund III", "Municipal Income Fund II", "Municipal Income Fund", "Income Strategy Fund II", "Income Strategy Fund", "Income Opportunity Fund", "High Income Fund", "Global StocksPLUS® & Income Fund", "Energy and Tactical Credit Opportunities Fund", "Dynamic Income Opportunities Fund", "Dynamic Income Fund", "Dynamic Credit and Mortgage Income Fund", "Corporate & Income Strategy Fund", "Corporate & Income Opportunity Fund", "California Municipal Income Fund III", "California Municipal Income Fund II", "California Municipal Income Fund")

getNAV <- function(){
  #4 is nav, 8 is discount
  xlFile <- "PimcoFunds.xls"
  ############
  # skip changed to 2.  unclear why, maybe back to 4 next time
  #############
  x <- read_excel(xlFile,skip=1,n_max=46,col_names=F,
                  .name_repair = "unique_quiet")
  names(x) <- c("Name","Symbol","Share Class","Daily NAV $","Daily Change $","Market Price $","Market Price Daily Change $","Premium / Discount %","Distribution Rate % (NAV)","Market Price Distribution Rate","Distribution Rate % (NAV)","30-Day SEC Yield %","30-Day SEC Yield % (Unsubsidized)","7-Day SEC Yield**","Latest Distribution ($ / Share)","Distribution (YTD)","Latest Dividend Distribution Payable Date","Latest Dividend Distribution Record Date","Latest Dividend Distribution Ex-Dividend Date")
  #remove na rows
  x <- x[is.na(x[,2])==0,]
  write.csv(x[,1:10],"PimcoFunds.csv",row.names=F)
  #desired columns, c("Ticker","NAV","discount","NAVdistribution","MarkDistribution")
  xx <- x[,c(2,4,8,9,11)]
  names(xx) <- c("Ticker","NAV","discount","NAVdistribution","MarkDistribution")
  
  xx$discount <- as.numeric(xx$discount)
  xx$NAVdistribution <- as.numeric(xx$NAVdistribution)
  xx$MarkDistribution <- as.numeric(xx$MarkDistribution)

  return(xx)
}

getPimcoData <- function(chosenTickers=cefMort) {
  #https://stackoverflow.com/questions/41368628/read-excel-file-from-a-url-using-the-readxl-package
  
  urlFromPimco <- "https://www.pimco.com/handlers/displaydocument.ashx?c=72201Y101&wd=UNII%20Report&fn=UNII%20Website%20File.xlsx&id=rc%2bNQHWWEucpe9Pl8GR4ZuchIBLSukAip55SJ6LR8CvmbW1h2ScLKkOI8wUGjjRVlqtvi4F2FCctG85Wa0WChwV9Lr8dxP8l9utktFaxMqbNE%2fu%2fmFZlP0en3TVZekc0GzTgcCj1yX2gpNL1AZPRb5eQpeObnFEmUNlpVN3DAYZ%2fCssLsoXU1FoLc793R4xHAmyp5THHNfyhxlsD%2fW8Opbmz04NLRE67WEWXOV0pe7cybRxYhZW585hPm3qR7xciSwUdtbMgrGhXmRzLL7Wl7588zkPbqsJjbKG953mv6J8ylrComp7qNhTVsS3tineeO4CmeqqcvbnzENMOKXLeQu1SoejRODtCrcOXbJyn1HEtHPyGI%2fZvbM7dtLOGdEhQGzVX9wd7xtNk9vUATKjZnUheCFYLOZcG9ENqjMVY4zwhm1ZzsR3bTIXOCIeVKtsA"
  GET(urlFromPimco, write_disk(tf <- tempfile()))
  sheetMonths <- excel_sheets(tf)
  numberOfSheets <- length(sheetMonths) -1 
  #set the names from Pimco to these
 # n <- c("FundName", "Ticker", "CurrentFiscal", "NII", "UNII", "MonthlyDistribution", "Rolling3Mon", "Rolling6Mon", "FiscalYear")
 # n2 <-c("FundName", "Ticker", "CurrentFiscal", "NII", "UNII", "Month3NII","MonthlyDistribution", "Rolling3Mon", "Rolling6Mon", "FiscalYear")

  data <- data.frame()
 
  currentYear <- as.numeric(format(Sys.Date(), format="%Y"))
  currentMonth <- as.numeric(format(Sys.Date(), format="%m"))
  sheetMonths <- excel_sheets(tf)
  
  for (i in 1:numberOfSheets) {
    
    dfTibble <-  read_xlsx(tf, skip=9, sheet=i)
    dfTibble <- dfTibble[dfTibble$Ticker %in% chosenTickers,]
    df <- data.frame(dfTibble)
    #table names changed.  
    names(df)[names(df) == 'Current Fiscal\r\nYear End']  <- 'CurrentFiscal'
    names(df)[names(df) == 'Estimated Net Investment Income (NII)(1) \r\nFiscal Year \r\nto Date']  <- 'NII'
    names(df)[names(df) == 'Estimated Undistributed \r\nNet Investment \r\nIncome -\r\nUNII/Estimated (ROC)(1)'] <- 'UNII'
    names(df)[names(df) == 'Monthly Distribution\r\nper Common\r\nShare(2)'] <- 'MonthlyDistribution'
    names(df)[names(df) == '3 Month Rolling Coverage Ratio(3)'] <- 'Rolling3Mon'
    names(df)[names(df) == '6 Month Rolling Coverage \r\nRatio (3)'] <- 'Rolling6Mon'
    names(df)[names(df) == 'Fiscal Year \r\nto Date Distribution Coverage\r\nRatio(4)'] <- 'FiscalYear'

    names(df)[names(df) == 'Current.Fiscal..Year.End']  <- 'CurrentFiscal'
    names(df)[names(df) == 'Estimated.Net.Investment.Income..NII..1....Fiscal.Year...to.Date']  <- 'NII'
    names(df)[names(df) == 'Estimated.Fiscal.Year.to.Date.Net.Investment.Income..NII..1.']  <- 'NII'
    names(df)[names(df) == 'Estimated.3..Month.Average...Net.Investment...Income..NII.']  <- 'NII'
    names(df)[names(df) == 'Estimated.Undistributed...Net.Investment...Income....UNII.Estimated..ROC..1.'] <- 'UNII'
    names(df)[names(df) == 'Monthly.Distribution..per.Common..Share.2.'] <- 'MonthlyDistribution'
    names(df)[names(df) == 'X3.Month.Rolling.Coverage.Ratio.3.'] <- 'Rolling3Mon'
    names(df)[names(df) == 'X6.Month.Rolling.Coverage...Ratio..3.'] <- 'Rolling6Mon'
    names(df)[names(df) == 'Fiscal.Year...to.Date.Distribution.Coverage..Ratio.4.'] <- 'FiscalYear'
#extra column added at some point,
   df = df[,!(names(df) %in% c("...11"))]
   df = df[,!(names(df) %in% c("NII.1"))]
   
   
    sheetMonthsNumber <- match(sheetMonths, month.name)
    monthsMultiYear <- max(sheetMonthsNumber) < length(sheetMonths)
 #   print(paste (sheetMonths[i], sheetMonthsNumber[i], monthsMultiYear))
    theYear <- currentYear
    if (monthsMultiYear){
      if (sheetMonthsNumber[i] > length(sheetMonths))
      theYear <- currentYear -1
    }
    
    df$date <- as.Date(paste(sheetMonths[i],"-","15","-",as.character(theYear), sep=""), format = "%b-%d-%Y")
  
    for (j in 1:nrow(df)) { 
      marks <- get.hist.quote(as.character(df[j,"Ticker"]), start=df[j,"date"],quote=c("Open"), quiet=T)
      df[j,"Mark"] <- marks[1]
    }
    data <- rbind(data, df)
  }
  
  #******************
  data$CurrentFiscal <- as.Date(data$CurrentFiscal)
  data$date <- as.Date(data$date)
  data$MonthlyDistribution <- as.numeric(data$MonthlyDistribution)
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
  
  #special Dividends
#  data[data$Ticker=="PDO","MonthlyDistribution"] <- data[data$Ticker=="PDO","MonthlyDistribution"] + (specialPDO/12)
  
  #yield calculated in two ways
  #yield is based on actual distribution / mark
  #yieldNII is the change in NII / mark
  data$Rolling1Mon <- data$diff/data$MonthlyDistribution
  data$yieldNII <- (data$diff*12)/data$Mark

  data$yield <- 12 * data$MonthlyDistribution / data$Mark
  data[data$Rolling1Mon<0,"Rolling1Mon"] <- NA
  data <- as.data.frame(data)
  
 dfNAV <- getNAV()
 data <- merge(data,dfNAV,by="Ticker")
 write.csv(data, "dataForDebugging.csv")
  return(data)
  #######
  #https://readxl.tidyverse.org/articles/articles/readxl-workflows.html
}
getMostRecent <- function(data){
  return(data[data$date==max(data$date),])
}