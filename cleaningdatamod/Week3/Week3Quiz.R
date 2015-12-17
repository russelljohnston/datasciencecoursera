
#Question 1
#The American Community Survey distributes downloadable data 
#about United States communities. Download the 2006 microdata 
#survey about housing for the state of Idaho using download.file() 
#from here and load the data into R. 

#Create a logical vector that 
#identifies the households on greater than 10 acres who sold more 
#than $10,000 worth of agriculture products. Assign that logical 
#vector to the variable agricultureLogical. Apply the which() 
#function like this to identify the rows of the data frame where 
#the logical vector is TRUE. which(agricultureLogical) What are 
#the first 3 values that result?


fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
download.file(fileUrl,destfile="q3-1.csv",method="curl") 
data <- read.table("q3-1.csv",header=TRUE,sep = ",",quote="")

agricultureLogical <- which(with(data, ACR==3 & AGS == 6))

agricultureLogical[1:3]

#Answer
# [1] 125 238 262

#---------------------------------------------------------------------------

#Question 2
#Using the jpeg package read in the following picture of your 
#instructor into R 
#https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg 
#Use the parameter native=TRUE. What are the 30th and 80th quantiles 
#of the resulting data? (some Linux systems may produce an answer 
#638 different for the 30th quantile)
library(jpeg)


fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg"
download.file(fileUrl,destfile="q3-2.jpg",method="curl") 
img <- readJPEG("q3-2.jpg",native = TRUE)

quantile(img, c(.30, .80)) 

#answer
# -15259150 -10575416 

#---------------------------------------------------------------------------

#Question 3
#Load the Gross Domestic Product data for the 190 ranked countries in this data set: 
  
#   https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv 

#Load the educational data from this data set: 
#  https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv 

#Match the data based on the country shortcode. How many of the IDs match? 
#Sort the data frame in descending order by GDP rank (so United States is last). 
#What is the 13th country in the resulting data frame? 

# Original data sources: 
# neam 
# http://data.worldbank.org/data-catalog/ed-stats

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

#reading educational data
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
download.file(fileUrl,destfile="q3-32.csv",method="curl") 
EDU <- data.table(read.csv("q3-32.csv",header = TRUE))
#match by countrycode
matched <- merge(GDP,EDU,all = TRUE,by='CountryCode')
#total number of ranked ignoring NA values
sum(!is.na(unique(matched$rank)))

#answer
#189

#sort data in rank descending, and fetch 13th country
sortmatched <- matched[order(-rank)][13]

#answer
#st kitts & nevis

#---------------------------------------------------------------------------
#Question 4

#What is the average GDP ranking for the "High income: OECD" and 
#"High income: nonOECD" group?

matched[, mean(rank, na.rm = TRUE), by = Income.Group]

#answer
#1:                   NA 131.00000
#2: High income: nonOECD  91.91304
#3:           Low income 133.72973
#4:  Lower middle income 107.70370
#5:  Upper middle income  92.13333
#6:    High income: OECD  32.96667
#7:                            NaN

#---------------------------------------------------------------------------
#Question 5
#Cut the GDP ranking into 5 separate quantile groups. Make a table versus 
#Income.Group. How many countries are Lower middle income but among the 38 
#nations with highest GDP?

library(Hmisc)

matched$rankq = cut2(matched$rank, g = 5)
table(matched$Income.Group, matched$rankq)


#Answer
# 5

#                           [  1, 39) [ 39, 77) [ 77,115) [115,153) [153,190]
#High income: nonOECD         4         5         8         4         2
#High income: OECD           18        10         1         1         0
#Low income                   0         1         9        16        11
#Lower middle income          5        13        12         8        16
#Upper middle income         11         9         8         8         9