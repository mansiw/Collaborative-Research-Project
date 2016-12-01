---
  title: "Untitled"
author: "Mansi"
date: "Saturday, November 19, 2016"
output: pdf_document
---
  
  ## Libraries ##
  
  
  library(stargazer)  # for summary statistics and regression tables
library(magrittr)  # for 'piping': more readable code
library(ggplot2)  # the ggplot2 package provides nice function for plotting
library(arm)  # for the sim() function to simulate model estimates
library(interplot)  # for plotting interactions
library(dplyr)  # for data manipulation

library(foreign)

## Reading 2014 dataset into an object
x <- read.dta("C:/Users/hp/Documents/GitHub/Collaborative-Research-Project/ESS7e02.dta")

## Subsetting x to keep only the youth
x <- subset(x, x$agea <= 25 & x$agea >= 18)

## Capturing the variable names of d into a vector and subsetting
df_vars <- c()
df_vars <- names(ESSData)

x_vars <- names(x)

## Checking for common variables

common <- intersect(x_vars, df_vars)
common

remain <- setdiff(df_vars, x_vars)
remain ## Shows variables in the dataset until 2012 but not in 2014

## Dropping irrelevant variables
x <- x[ ,common]

## Removing Undesired Countries from ESS Dataset ##
temp <- with(x, which(x$cntry == "AT" | x$cntry == "LT" | x$cntry == "IL" | x$cntry == "CH", arr.ind=TRUE))

x <- x[-temp, ]

## Removing unwanted columns from x using the vector ESSVariables created in Data cleaning
temp2 <- intersect(ESSVariables, common) #Since common refers to variables in x

x2 <- x[temp2]


## Coding variables that require manual effort ##

table(x2$polintr)
levels(x2$polintr) <- c("1", "2", "3", "4", "7", "8", "9")

table(x2$vote)
levels(x2$vote) <- c("1", "2", "3","7", "8", "9")

table(x2$contplt)
levels(x2$contplt) <- c("1", "2", "7", "8", "9")

table(x2$wrkprty)
levels(x2$wrkprty) <- c("1", "2", "7", "8", "9")

table(x2$wrkorg)
levels(x2$wrkorg) <- c("1", "2", "7", "8", "9")

table(x2$badge)
levels(x2$badge) <- c("1", "2", "7", "8", "9")

table(x2$sgnptit)
levels(x2$sgnptit) <- c("1", "2", "7", "8", "9")

table(x2$pbldmn)
levels(x2$pbldmn) <- c("1", "2", "7", "8", "9")

table(x2$bctprd)
levels(x2$bctprd) <- c("1", "2", "7", "8", "9")

table(x2$clsprty)
levels(x2$clsprty) <- c("1", "2", "7", "8", "9")

table(x2$uempla)
levels(x2$uempla) <- c("0", "1")

table(x2$uempli)
levels(x2$uempli) <- c("0", "1")

table(x2$dsbld)
levels(x2$dsbld) <- c("0", "1")

table(x2$mbtru)
levels(x2$mbtru) <- c("1", "2", "3", "7", "8", "9")

table(x2$wrkctra)
levels(x2$wrkctra) <- c("1", "2", "3", "6", "7", "8", "9")

table(x2$trstprl) ## shows that we need to code this manually
levels(x2$trstprl) <- c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "77", "88", "99")
table(x2$trstprl) ## shows the levels as we need them


levels(x2$trstplt) <- c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "77", "88", "99")
table(x2$trstplt)

levels(x2$trstep) <- c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "77", "88", "99")

levels(x2$eisced) <- c("0", "1", "2", "3", "4", "5", "6", "7", "55", "77", "88", "99")

levels(x2$gndr) <- c("1", "2", "9")


levels(x2$pdjobev) <- c("1", "2", "6", "7", "8", "9")

levels(x2$mainact) <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "66", "77", "88", "99")




## Changing class of variables as needed for merging
## x2$essround <- as.integer(x2$essround)
## x2$polintr <- as.integer(x2$polintr)
## x2$trstprl <- as.integer(x2$trstprl)
## x2$trstplt <- as.integer(x2$trstplt)
## x2$trstep <- as.integer(x2$trstep)

## As there are only 25 variables common between these two, those are the ones we can need to merge the data. The one variable that exists in only until 2012 cannot be included in the command below otherwise it doesn't work

combined <- merge(ESSData, x2, by = c("cntry", "essround", "polintr","trstprl", "trstplt","trstep","vote","contplt","wrkprty","wrkorg","badge","sgnptit","pbldmn", "bctprd","clsprty","eisced","uempla", "uempli","dsbld", "mbtru","mainact","wrkctra","gndr","stfdem","pdjobev"), all = T)



combined$polintr[combined$polintr > 4] <- NA
combined$trstprl[combined$trstprl > 10] <- NA
combined$trstplt[combined$trstplt > 10] <- NA
combined$trstep[combined$trstep > 10] <- NA
combined$vote[combined$vote > 2] <- NA
combined$contplt[combined$contplt > 2] <- NA
combined$wrkprty[combined$wrkprty > 2] <- NA
combined$badge[combined$badge > 2] <- NA
combined$sgnptit[combined$sgnptit > 2] <- NA
combined$pbldmn[combined$pbldmn > 2] <- NA
combined$bctprd[combined$bctprd > 2] <- NA
combined$clsprty[combined$clsprty > 2] <- NA
combined$eisced[combined$eisced == 0] <- NA
combined$eisced[combined$eisced >50] <- NA
combined$mbtru[combined$mbtru > 3] <- NA
combined$pdjobev[combined$pdjobev > 2] <- NA
combined$gndr[combined$gndr > 2] <- NA


combined$TIME[combined$essround == 1] <- 2002
combined$TIME[combined$essround == 2] <- 2004
combined$TIME[combined$essround == 3] <- 2006
combined$TIME[combined$essround == 4] <- 2008
combined$TIME[combined$essround == 5] <- 2010
combined$TIME[combined$essround == 6] <- 2012
combined$TIME[combined$essround == 7] <- 2014



## Unemployment dummy


combined$unempdummy <- 0

# ESSData$unempdummy <- ESSData$uempla + ESSData$uempli

combined$unempdummy <- ifelse(combined$uempla ==1 | combined$uempli ==1, 1, 0)
# if answer to either of the unemployment variables is 1, code as 1

table(combined$unempdummy) # to verify only 0 or 1 in that variable

GroupedESS <- group_by(combined, cntry, TIME) # Group the ESS Data by country and year

MeansESS <- summarize(GroupedESS, avgpolintr = mean(polintr), avgtrstprl = mean(trstprl), avgtrstplt = mean(trstplt), avgtrstep = mean(trstep), avgvote = mean(vote), avgcontplt = mean(contplt), avgwrkprty = mean(wrkprty), avgwrkorg = mean(wrkorg), avgbadge=mean(badge), avgsgnptit = mean(sgnptit), avgpbldmn = mean(pbldmn), avgbctprd = mean(bctprd), avgclsprty = mean(clsprty), avgmmbprty = mean(mmbprty), avgeisced = mean(eisced), avguempla = mean(uempla), avguempli = mean(uempli), avgdsbld = mean(dsbld), avgmbtru = mean(mbtru)) 
# create variables containing the means for each country and year
