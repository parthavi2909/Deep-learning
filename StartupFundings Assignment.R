library(tidyverse)
library(lubridate)
library(plotly)
library(leaflet)
library(scales)
library(zoo)
library(glue)

getwd()

sf <- read_csv("C:\\Users\\megha\\OneDrive\\Desktop\\Deep Learning\\startup_funding.csv")

view(sf)

sf

#check NA availability for each column
colSums(is.na(sf))

#Delete unused column
sf <- sf[, -c(1,5,10)]
head(sf)

## Convert Date variable to Date format
sf$Date <- as.Date(sf$Date,format("%d/%m/%Y"))
## Confirm
str(sf)

## Make a new column for year
sf$year <- as.numeric(format(sf$Date,"%Y"))
## Year frequency table
yeartable <- table(sf$year )
yeartable
## Visualization using Bar chart
barplot(yeartable, xlab="Year", ylab = "No. of Startups which received funding", col="lightblue" )
head(sort(table(sf$StartupName), decreasing=TRUE))
industrytable <- head(sort(table(sf$IndustryVertical), decreasing=TRUE))
industrytable
## Visualization using Bar chart
barplot(industrytable, xlab="Industry", ylab = "No. of Startups which received funding", col="lightblue",cex.names = 0.7 )
##Subvertical vertical frequency table
subverticaltable <- head(sort(table(sf$SubVertical), decreasing=TRUE))
subverticaltable
## Visualization using Bar chart
barplot(subverticaltable, xlab="Subvertical", ylab = "No. of Startups which received funding", col="pink",cex.names = 1 )
##Location frequency table
locationtable <- head(sort(table(sf$CityLocation), decreasing=TRUE))
locationtable
## Visualization using Bar chart
barplot(locationtable, xlab="Location", ylab = "No. of Startups which received funding", col="green",cex.names = 1 )
##Investor frequency table
Investortable <- head(sort(table(sf$InvestorsName), decreasing=TRUE))
Investortable
## Visualization using Bar chart
barplot(Investortable, xlab="Investor Name", ylab = "No. of Startups which received funding", col="coral",cex.names = 0.5 )
##Investment Type frequency table
typetable <- head(sort(table(sf$InvestmentType), decreasing=TRUE))
typetable
## Visualization using Bar chart
barplot(typetable, xlab="Investment Type", ylab = "No. of Startups which received funding", col="aquamarine",cex.names = 0.5 )
## Convert factor from factor to numeric

sf$AmountInUSD<-as.numeric(as.character(sf$AmountInUSD))
sf$AmountInUSD[is.na(sf$AmountInUSD)] <- 0
disclosedfunding.df <- subset(sf,AmountInUSD >0)
library(psych)
describe(disclosedfunding.df$AmountInUSD)[,c(3:9)]
boxplot(disclosedfunding.df$AmountInUSD, horizontal = TRUE, ylim=c(0,20000000)  )
Location <- c("Bangalore","Mumbai","New Delhi","Gurgaon","Pune")
Funding <- c(sum(disclosedfunding.df$AmountInUSD[disclosedfunding.df$CityLocation=="Bangalore"]),sum(disclosedfunding.df$AmountInUSD[disclosedfunding.df$CityLocation=="Mumbai"]),sum(disclosedfunding.df$AmountInUSD[disclosedfunding.df$CityLocation=="New Delhi"]),sum(disclosedfunding.df$AmountInUSD[disclosedfunding.df$CityLocation=="Gurgaon"]),sum(disclosedfunding.df$AmountInUSD[disclosedfunding.df$CityLocation=="Pune"]))
frame <- data.frame(Location,Funding)
pie(frame$Funding,labels = frame$Location,col=cm.colors(6),main = "Funding(USD) Received in top 5 cities" )


p <- aggregate(AmountInUSD ~CityLocation , data=disclosedfunding.df ,mean)
p <- p[order(-p$AmountInUSD),]
## Only display for cities with maximum mean funding
head(p)
tail(p)
disclosedfunding.df$IndustryVertical <- as.numeric(disclosedfunding.df$IndustryVertical)
disclosedfunding.df$SubVertical <- as.numeric(disclosedfunding.df$SubVertical)
disclosedfunding.df$CityLocation <- as.numeric(disclosedfunding.df$CityLocation)
mat <- cor(disclosedfunding.df[,c(4,5,6,9)])
mat
library(corrplot)
corrplot(corr=cor(disclosedfunding.df[,c(4,5,6,9)]),method="ellipse")
tab <- table(disclosedfunding.df$CityLocation)
chisq.test(tab)
tab2 <- table(disclosedfunding.df$IndustryVertical )
chisq.test(tab2)
