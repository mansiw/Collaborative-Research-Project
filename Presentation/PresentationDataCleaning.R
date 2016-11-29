#### Initial Setup ####

## Libraries ##

library(stargazer)  # for summary statistics and regression tables
library(magrittr)  # for 'piping': more readable code
library(ggplot2)  # the ggplot2 package provides nice function for plotting
library(arm)  # for the sim() function to simulate model estimates
library(interplot)  # for plotting interactions
library(dplyr)  # for data manipulation
library(plm)
## Loading the Data ##

setwd('/Users/toridykes1/GitHub/Collaborative-Research-Project/Presentation')

d <- read.csv('Datasets/ESS1-6e01_1_F1.csv') # ESS Data on Political Engagement
e <- read.csv('Datasets/Under 25 unemp.csv', stringsAsFactors = F) # Eurostat Data on Unemployment

#### Cleaning Up the Data ####

## Limiting to Youth Data ##

df <- subset(d, d$agea <= 25 & d$agea >= 18) # Limit ESS data to indiivduals 18 - 25
eu <- subset(e, e$AGE == "Less than 25 years") # Limit unemployment data to individuals under 25

# Summary of the tables

summary(df$agea)
summary(eu$AGE)

## Filtering the Eurostat Unemployment Data for Relevant Figures and Years ##

eu <- subset(eu, eu$UNIT == "Percentage of active population") # Limit the to single statistic
eu <- subset(eu, eu$TIME == 2006 |eu$TIME == 2008 |eu$TIME == 2010 |eu$TIME == 2012| eu$TIME == 2014) # Limit to only years in which Eurostat survey occurred

## Checking unique countries in each dataset ##

unique(df$cntry)
unique(eu$GEO)

## Drop EU-related observations from unemployment data ##

dropnames <- c("Euro area (18 countries)", "Euro area (19 countries)", "European Union (28 countries)", "Euro area (EA11-2000, EA12-2006, EA13-2007, EA15-2008, EA16-2010, EA17-2013, EA18-2014, EA19)", "European Union (25 countries)", "European Union (27 countries)")

eu <- eu[! eu$GEO %in% dropnames, ]

## Change name of Germany ##

eu$GEO[eu$GEO=="Germany (until 1990 former territory of the FRG)"] <- "Germany"

## Removing Undesired Countries from ESS Dataset ##
temp <- with(df, which(df$cntry == "AT" | df$cntry == "HR" | df$cntry == "IS" | df$cntry == "LT" | df$cntry == "LU" | df$cntry == "RU" | df$cntry == "TR" | df$cntry == "UA" | df$cntry == "IL" | df$cntry == "CH", arr.ind=TRUE))
df <- df[-temp, ]

table(df$cntry) # check to see country names remaining


##Removing Undesired Countries from Unemployment Dataset ##

temp2 <- with(eu, which(eu$GEO == "Japan" | eu$GEO == "Turkey" | eu$GEO == "United States" | eu$GEO == "Austria" | eu$GEO == "Croatia" | eu$GEO == "United States" | eu$GEO == "Iceland" | eu$GEO == "Lithuania" | eu$GEO == "Luxembourg" | eu$GEO == "Latvia" | eu$GEO == "Romania" | eu$GEO == "Malta" | eu$GEO == "Switzerland",  arr.ind=TRUE))
eu <- eu[-temp2, ]


## Removing unwanted columns from ESS data ##

ESSVariables <-c("cntry", "essround", "polintr","trstprl", "trstplt","trstep","vote","contplt","wrkprty","wrkorg","badge","sgnptit","pbldmn","bctprd","clsprty","mmbprty","edulvla","uempla", "uempli","dsbld", "mbtru","mainact","wrkctra","gndr","stfdem","pdjobev")

ESSData <- df[ESSVariables]

table(ESSData$cntry)


## Condensing ESS Data ##

table(ESSData$polintr) # want to drop any response greater than 4

ESSData <- subset(ESSData, ESSData$polintr <= 4 & ESSData$trstprl <=10 & ESSData$trstplt <=10 & ESSData$trstep <=10 & ESSData$vote <=3 & ESSData$contplt <=2 & ESSData$wrkprty <= 2 & ESSData$badge <=2 & ESSData$sgnptit <= 2 & ESSData$pbldmn <= 2 & ESSData$bctprd <= 2 & ESSData$clsprty <=2 & ESSData$edulvla > 0 & ESSData$edulvla < 55 & ESSData$mbtru <= 3 & ESSData$pdjobev <= 2 & ESSData$gndr <= 2 )

table(ESSData$polintr)

ESSData$polintr[ESSData$polintr > 4] <- NA
ESSData$trstprl[ESSData$trstprl > 10] <- NA
ESSData$trstplt[ESSData$trstplt > 10] <- NA
ESSData$trstep[ESSData$trstep > 10] <- NA
ESSData$vote[ESSData$vote > 2] <- NA
ESSData$contplt[ESSData$contplt > 2] <- NA
ESSData$wrkprty[ESSData$wrkprty > 2] <- NA
ESSData$badge[ESSData$badge > 2] <- NA
ESSData$sgnptit[ESSData$sgnptit > 2] <- NA
ESSData$pbldmn[ESSData$pbldmn > 2] <- NA
ESSData$bctprd[ESSData$bctprd > 2] <- NA
ESSData$clsprty[ESSData$clsprty > 2] <- NA
ESSData$edulvla[ESSData$edulvla == 0] <- NA
ESSData$edulvla[ESSData$edulvla >= 55] <- NA
ESSData$mbtru[ESSData$mbtru > 3] <- NA
ESSData$pdjobev[ESSData$pdjobev > 2] <- NA
ESSData$gndr[ESSData$gndr > 2] <- NA


table(ESSData$edulvla)


## Add year value for ESS Rounds ##

ESSData$TIME[ESSData$essround == 1] <- 2002
ESSData$TIME[ESSData$essround == 2] <- 2004
ESSData$TIME[ESSData$essround == 3] <- 2006
ESSData$TIME[ESSData$essround == 4] <- 2008
ESSData$TIME[ESSData$essround == 5] <- 2010
ESSData$TIME[ESSData$essround == 6] <- 2012

## Add country code to unemployment data country names for merging ##

library(countrycode)
#countrycode("Austria", "country.name", "iso2c")

eu$cntry <- countrycode(eu$GEO, "country.name", "iso2c")

## Creating Dummy Variable ##

# If uempla or uempli = 1, then code as 1, else 0

ESSData$unempdummy <- 0

# ESSData$unempdummy <- ESSData$uempla + ESSData$uempli

ESSData$unempdummy <- ifelse(ESSData$uempla ==1 | ESSData$uempli ==1, 1, 0)
# if answer to either of the unemployment variables is 1, code as 1

table(ESSData$unempdummy) # to verify only 0 or 1 in that variable

## Group Data ##

GroupedESS <- group_by(ESSData, cntry, TIME) # Group the ESS Data by country and year

MeansESS <- summarize(GroupedESS, avgpolintr = mean(polintr), avgtrstprl = mean(trstprl), avgtrstplt = mean(trstplt), avgtrstep = mean(trstep), avgvote = mean(vote), avgcontplt = mean(contplt), avgwrkprty = mean(wrkprty), avgwrkorg = mean(wrkorg), avgbadge=mean(badge), avgsgnptit = mean(sgnptit), avgpbldmn = mean(pbldmn), avgbctprd = mean(bctprd), avgclsprty = mean(clsprty), avgmmbprty = mean(mmbprty), avgedulvla = mean(edulvla), avguempla = mean(uempla), avguempli = mean(uempli), avgdsbld = mean(dsbld), avgmbtru = mean(mbtru)) 
# create variables containing the means for each country and year

#### Merge Datasets ####

YouthData <- merge(MeansESS, eu, by = c("cntry", "TIME"), all = T)

write.csv(YouthData, file = "YouthData.csv")

YouthData$Value <- as.numeric(as.character(YouthData$Value)) # Ensure all the unemployment value are numerics 

testvect <- c()

testvect <- names(d[1:125])

class(testvect)

is.atomic(testvect)
is.vector(testvect)

#dtest <- d[!eg2011$ID %in% bg2011missingFromBeg, ]


#### Building a Model ####

## Need to:
# - Subtract the value of 2002 from 2004; 2004 from 2006, etc. 
# - Assign these values to deltaavgpolintr 

# Create delta column for avg political interest
YouthData$deltaavgpolintr <- unlist(by(YouthData$avgpolintr, YouthData$cntry, function(x){c(NA,diff(x))}))

# Create delta column for unemployment 
YouthData$deltavalue <- unlist(by(YouthData$Value, YouthData$cntry, function(x){c(NA,diff(x))}))

# Create delta for voting
YouthData$deltaavgvote <- unlist(by(YouthData$avgvote, YouthData$cntry, function(x){c(NA,diff(x))}))

# Quickie regression
m1 <- lm(deltaavgpolintr ~ deltavalue, data = YouthData) # regress change in polintr on change in uenmp

summary(m1)

m2 <- lm(deltaavgvote ~ deltavalue, data = YouthData) # regress change in vote on change in unemp

summary(m2)

m3 <- plm(deltaavgvote ~ deltavalue, data = YouthData, model = "within") # FE model, vote & unemp

summary(m3)

m4 <- lm(deltaavgvote ~ deltavalue + factor(cntry) - 1, data = YouthData) # create dummies for each country

summary(m4)

# regress individual level employment data on did you vote
# could do this at a country level as well 
# scatterplots to visualize

unique(ESSData$cntry)

## Test Regression With Individual Unemployment Data ##

m5 <- lm(polintr ~ unempdummy, data = ESSData)

summary(m5)

m6 <- lm(pbldmn ~ unempdummy, data = ESSData)

summary(m6)