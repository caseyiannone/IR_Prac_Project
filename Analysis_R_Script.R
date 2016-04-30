library(data.table)
library(dplyr)
library(tidyr)
library(plyr)
library(sqldf)

#############################################################################################################################
#Combining IPEDS datasets into a single file

#PC Working directory
setwd("C:/Users/ciannone/Google Drive/Graduate_Coursework/Florida State University/Practicum/Analysis_Plots/IPEDS_Data")

#Mac
setwd("~/Google Drive/Graduate_Coursework/Florida State University/Practicum/Analysis_Plots/IPEDS_Data")

HD_2014 <- read.csv("hd2014.csv",header=TRUE,sep=",")
ADM_2014 <- read.csv("adm2014.csv",header=TRUE,sep=",")
EF_2014 <- read.csv("ef2014d.csv",header=TRUE,sep=",")


#Combine various institutional demographics into a single file
IPEDS_Data <- inner_join(HD_2014, ADM_2014, by="UNITID") 
IPEDS_Data <- inner_join(IPEDS_Data, EF_2014, by="UNITID")
IPEDS_Data <- data.table(IPEDS_Data)
#remove uneeded variables
IPEDS_Data <- IPEDS_Data[,c(3:4):= NULL]
IPEDS_Data <- IPEDS_Data[,c(4:19):= NULL]
IPEDS_Data <- select(IPEDS_Data,-contains("ADMCON"))
IPEDS_Data <- select(IPEDS_Data,-contains("X"))

#Some of these variables are inbetween others that I want to keep, so I thought it would be
#easier to just to drop by varaible name instead of index
#IPEDS_Data <- select(IPEDS_Data, -matches("ACT")) I think this drops all of the ACT variables
IPEDS_Data <- IPEDS_Data[,ACT := NULL]
IPEDS_Data <- select(IPEDS_Data, -matches("CBSA"))
IPEDS_Data <- select(IPEDS_Data, -matches("CBSATYPE"))
IPEDS_Data <- select(IPEDS_Data, -matches("CLOSEDAT"))
IPEDS_Data <- select(IPEDS_Data, -matches("CNGDSTCD"))
IPEDS_Data <- select(IPEDS_Data, -matches("COUNTYCD"))
IPEDS_Data <- select(IPEDS_Data, -matches("COUNTYNM"))
IPEDS_Data <- select(IPEDS_Data, -matches("CSA"))
IPEDS_Data <- select(IPEDS_Data, -matches("CYACTIVE"))
IPEDS_Data <- select(IPEDS_Data, -matches("DEATHYR"))
IPEDS_Data <- select(IPEDS_Data, -matches("DFRCGID"))
IPEDS_Data <- select(IPEDS_Data, -matches("DFRCUSCG"))
IPEDS_Data <- select(IPEDS_Data, -matches("F1SYSCOD"))
IPEDS_Data <- select(IPEDS_Data, -matches("F1SYSNAM"))
IPEDS_Data <- select(IPEDS_Data, -matches("F1SYSTYP"))
IPEDS_Data <- select(IPEDS_Data, -matches("HOSPITAL"))
IPEDS_Data <- select(IPEDS_Data, -matches("MEDICAL"))
IPEDS_Data <- select(IPEDS_Data, -matches("NECTA"))
IPEDS_Data <- select(IPEDS_Data, -matches("NEWID"))
IPEDS_Data <- select(IPEDS_Data, -matches("POSTSEC"))
IPEDS_Data <- select(IPEDS_Data, -matches("PSEFLAG"))
IPEDS_Data <- select(IPEDS_Data, -matches("PSET4FLG"))
IPEDS_Data <- select(IPEDS_Data, -matches("RPTMTH"))

#list of Vars
names(IPEDS_Data)


#read in college scorecard data
CSC_2011 <- read.csv("CSC_2011.csv",header=TRUE,sep=",")
CSC_2011 <- data.table(CSC_2011)

#Removing 2011 institutional demogrphic data, which will be replaced by 2013 data
CSC_2011 <- CSC_2011[,c(2:1637):= NULL] 

#select the treasury variables that are only avaiavle in 2011 to combine with institution demogrphic data in 2013

CSC_2011_2 <- select(CSC_2011, UNITID, mn_earn_wne_p10, mn_earn_wne_p6, mn_earn_wne_p8, md_earn_wne_p10, 
                     md_earn_wne_p6, md_earn_wne_p8, sd_earn_wne_p10, sd_earn_wne_p6, sd_earn_wne_p7, 
                     sd_earn_wne_p8, sd_earn_wne_p9)

#Combine IPEDS & Scorecard data
Full_DS <- inner_join(IPEDS_Data, CSC_2011_2, by="UNITID")
Full_DS <- data.table(Full_DS)
names(Full_DS)
#Remove more uneeded variabels
Full_DS <- Full_DS[,c(13:21):= NULL] 
Full_DS <- Full_DS[,c("sd_earn_wne_p7","sd_earn_wne_p9"):= NULL]

#change all nulls to NA, as there is a combination of Nulls and NAs used in the files
Full_DS[Full_DS == "NULL"] <- NA
Full_DS[Full_DS == "PrivacySuppressed"] <- NA

############################### Wrtie combined dataset to csv file
write.csv(Full_DS, "Final_Data.csv")


#Are there NAs present
is.na(Full_DS)
#Percentage of data that is missing
mean(is.na(Full_DS))

summary(Full_DS)

########################################################################################## Left off here!!!!!!!!!
# Need to decided how I shold handle NA valus for SAT/ACT
#Can use the full_data csv to perform analysis on from this point
# Replace NAs with the median by institutional type

dat$X <- with(Full_DS,ifelse(
  is.na(X),
  ave(coloumn with missing values,Carnegie class,FUN=function(x) mean(x,na.rm=TRUE)),
  X
))


#Remove rows with missing values in any of the coloumns
DT <- na.omit(Full_data2)
#THere is too much incomplete data with current data. Pull institution demogrphic data direct from ipeds for 2014

write.table(Full_data2, "Full_Data2.csv", sep=",")

#If you run functions like mean() or sum() on a vector containing NA or NaN, they will return NA and NaN, 
#which is generally unhelpful, though this will alert you to the presence of the bad value. Many of these 
#functions take the flag na.rm, which tells them to ignore these values.


####################################################################################################################################
#Drop columns with lots of missing data 
Full_data2 <- Full_data[,c("sd_earn_wne_p6","sd_earn_wne_p7", "sd_earn_wne_p9","sd_earn_wne_p8") := NULL]
summary(Full_data)
