library(XML)
library(data.table)
library(dplyr)
library(tidyr)
library(plyr)
library(sqldf)

setwd("/Users/caseyiannone/Google Drive/Graduate_Coursework/Florida State University/Practicum/Analysis_Plots")

#Grab a list of state abbreviations from the web to compare to list of Ipeds abbriviations
State_ab <- "http://www.50states.com/abbreviations.htm"
State_ab.table <- readHTMLTable(State_ab, header=T, which=1,stringsAsFactors=F)
View(State_ab.table)

# Read in State Income data
State_Income <- read.csv("per_capita_income.csv",header=TRUE,sep=",")

#Build a table that has the state abbreviation/parse out the per capita income for each state
State_Person <- filter(State_Income, Description == "Per capita personal income (dollars)")
State_Person <- data.table(State_Person)
State_ab.table <- data.table(State_ab.table)
#Drop uneeded vars
State_Person[, c("GeoFips","Description", "LineCode", "X2015Q1","X2015Q2","X2015Q3") := NULL]

setnames(State_ab.table, "US State:","US_State:")

#Join datasets
State_Data <- right_join(State_Person, State_ab.table, by = "US_State:")
State_Data <- data.table(State_Data)
State_Data <- State_Data[-c(51:67), ]

write.csv(State_Data, "State_Data.csv")


