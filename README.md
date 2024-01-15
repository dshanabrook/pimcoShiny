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
