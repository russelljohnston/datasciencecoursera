#Question 1

#The American Community Survey distributes downloadable data about
#United States communities. Download the 2006 microdata survey about 
#housing for the state of Idaho using download.file() from here: 
#https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv 
#and load the data into R. The code book, describing the variable names is here: 
#https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FPUMSDataDict06.pdf 

#Apply strsplit() to split all the names of the data frame on the characters 
#"wgtp". What is the value of the 123 element of the resulting list?


fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
download.file(fileUrl,destfile="q4.csv",method="curl") 
data <- read.table("q4.csv",header=TRUE,sep = ",",quote="")

dataspl <- strsplit(names(data), split= "wgtp")
dataspl[[123]]

#---------------------------------------------------------------------------
#Question 2
# Load the Gross Domestic Product data for the 190 ranked countries in this data set: 
  
#  https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv 

#Remove the commas from the GDP numbers in millions of dollars and average them.
#What is the average? 

#Original data sources: http://data.worldbank.org/data-catalog/GDP-ranking-table


library(data.table)

fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
download.file(fileUrl,destfile="q3-31.csv",method="curl") 
GDP <- data.table(read.csv("q3-31.csv", skip=4, nrows=215))

#Removing unwated columns
GDP[,X.2:=NULL]
GDP[,X.5:=NULL]
GDP[,X.6:=NULL]
GDP[,X.7:=NULL]
GDP[,X.8:=NULL]
GDP[,X.9:=NULL]

#assign column names
names(GDP) <- c("CountryCode","rank","country","gdp")

GDP$gdpnum <- as.numeric(gsub(",", "", as.character(GDP$gdp)))

mean(GDP$gdpnum, na.rm = TRUE)

# answer
# 377652.4

#---------------------------------------------------------------------------
#Question 3

#In the data set from Question 2 what is a regular expression that would 
#allow you to count the number of countries whose name begins with "United"? 
#Assume that the variable with the country names in it is named countryNames.
#How many countries begin with United?

# tmp <- grep("^United",GDP$country)
subset(GDP, grepl("^United", GDP$country))

#---------------------------------------------------------------------------


#Question 4
#Load the Gross Domestic Product data for the 190 ranked countries in this data set: 

#  https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv 

#Load the educational data from this data set: 

#  https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv 

#Match the data based on the country shortcode. Of the countries for which the end of the fiscal year is available, how many end in June? 

#Original data sources: 
#  http://data.worldbank.org/data-catalog/GDP-ranking-table 
#http://data.worldbank.org/data-catalog/ed-stats


#reading educational data
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
download.file(fileUrl,destfile="q3-32.csv",method="curl") 
EDU <- data.table(read.csv("q3-32.csv",header = TRUE))
#match by countrycode
matched <- merge(GDP,EDU,all = TRUE,by='CountryCode')

matched <- apply(matched$Special.Notes,tolower)

grep("June",matched$Special.Notes)

sub <- subset(matched, grepl("end: june", tolower(matched$Special.Notes)))
       
nrow(sub)
#answer
#13

#---------------------------------------------------------------------------


#Question 5

#You can use the quanmod (http://www.quantmod.com/) package to get historical 
#stock prices for publicly traded companies on the NASDAQ and NYSE. 

#Use the following code to download data on Amazon's stock price and get the times
#the data was sampled.

library(quantmod)
library(timeDate)
amzn = getSymbols("AMZN",auto.assign=FALSE)
sampleTimes = index(amzn) 


sub <- subset(sampleTimes, grepl("2012",sampleTimes))
tab <- table(sub)
nrow(tab)

addmargins(table(year(sampleTimes), weekdays(sampleTimes)))




