
# Fill in a package name
library("SDSFoundations")

# Check if you already installed the package
any(grepl("SDSFoundations", installed.packages()))


# Read in csv files
df <- read.csv(file = 'Analysis Exercise Analyst.csv')
head(df)



#check null values
which(is.na(df))

# Number of rows in the dataset
nrow(df)

# Convert to lower case
df$country <- tolower(df$country)
df

#this would create a new dataframe yet we need a variable 
#df1$x1 <- 'z'

#df['active_cases'] = total cases-(deaths +recoveries)
active_cases_var = sum(df$cases)-(sum(df$deaths) + sum(df$recovered) )  

#the number of activate cases is 
active_cases_var

#the number of activate cases for each row is 

df$active_cases <- df[,3] - (df[,4]  + df[,5] )

#or use this
df['activeCase'] <- df['cases'] - (df['deaths'] + df['recovered'])

df

#e. Generate a new variable called month from the date variable (you should have 4 months)
# library to deal with date
library("lubridate")
any(grepl("lubridate", installed.packages()))

month(as.POSIXlt(some_date, format="%d/%m/%Y"))

#only get month
df[, "month"] <- month(as.POSIXlt(df$date, format="%m/%d/%Y"))
df

#check unique month
unique(df["month"])

#f. Compute mean, median and standard deviation for Kenya, Nigeria and Rwanda for each month

#function to return what they asked
getmean <- function(value){
  average <- mean(value)
  return(average)
}

getmedian <- function(value){
  median <- median(median)
  return(median)
}

getstd <- function(value){
  std <- sd(value)
  return(average)
}

#easier way 
# get countries and related records
df2 <- data.frame(df[df$country ==  "rwanda" | df$country ==  "kenya" | df$country ==  "nigeria", ] )
head(df2)

#pivot table
library(tidyverse)
library(here)

#mean, median and standard deviation, for total cases, deaths and recoveries
pivot <- df2 %>% select(country, cases, deaths, recovered)%>% 
  group_by(df2$month, df2$country )%>% summarise(Meancases= mean(cases),
                                                 Mediancases= median(cases),
                                                 Stdcases=sd(cases),
                                                 Meandeaths= mean(deaths),
                                                 Mediandeaths= median(deaths),
                                                 Stddeaths=sd(deaths),
                                                 Meanrecovered= mean(recovered),
                                                 Medianrecovered= median(recovered),
                                                 Stdrecovered=sd(recovered))
pivot


#g. Write a paragraph with any insights from the summary stats in f above.

#all the best





#QUESTION 2

#dataset called "nassCDS" from the package DAAG  
library("DAAG")

data(nassCDS)
head(nassCDS)

#check data dimension
dim(nassCDS)
#out put
#[1] 26217    15

#a. How many individuals used seatbelt?
levels(nassCDS$seatbelt)

table(nassCDS$seatbelt)
#none belted 
#7644  18573 


levels(nassCDS$dead)


#d. Produce a 2X2 table that shows the number of seatbelt users (belted/none) and accident&#39;s
#outcome (alive/dead)?
table(nassCDS$dead,nassCDS$seatbelt)



distrib <- xtabs(weight ~ dead + airbag + seatbelt + dvcat, data=nassCDS)
distrib
dim(distrib)

library(productplots)

str(distrib)

# Plotting seems to fails
PlotXTabs(weight ~ dead + airbag + seatbelt + dvcat, data=nassCDS)


# 2X2 table that shows the number of 
#seatbelt users (belted/none) and accidentsoutcome
tab <- xtabs(~ dead + seatbelt, data=nassCDS)
tab


#e.

tab1 <- as.data.frame(tab)
tab1$dead
tab1

#get only dead proportion 
deadprop <- tab1[tab1$dead ==  "dead", ]
deadprop

summary(tab)
summary(tab1)
summary(deadprop)

#test proportion using the initial tab from 
ttests <- function(value){
  result <- t.test(value, mu = 5)
  return(result)
}
ttests(tab)
#fisher.test(tab1)

