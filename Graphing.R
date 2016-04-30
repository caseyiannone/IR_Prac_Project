library(data.table)
library(dplyr)
library(tidyr)
library(plyr)
library(sqldf)
library(ggvis)
library(ggplot2)
library(googleVis)
library(ggthemes)
library(googlesheets)
library(portfolio)
library(purrr)       
library(tidyr)       
library(lubridate)   # date manipulation
library(scales)      # pairs nicely with ggplot2 for plot label formatting
library(gridExtra)   # a helper for arranging individual ggplot objects
library(viridis)     # best. color. palette. evar.
library(knitr)  
library(ggmap)
library(reshape2)
library(choroplethr)
library(choroplethrMaps)
library(gridExtra)
library(RColorBrewer)

#############################################################################################################################
#PC Working directory
setwd("C:/Users/ciannone/Google Drive/Graduate_Coursework/Florida State University/Practicum/Analysis_Plots")

#Mac
setwd("~/Google Drive/Graduate_Coursework/Florida State University/Practicum/Analysis_Plots")

FD <- read.csv("Final_Data.csv",header=TRUE,sep=",")
SD <- read.csv("State_Data.csv",header=TRUE,sep=",")
Earnings <- read.csv("earnings.csv",header=TRUE,sep=",")

FD <- data.table(FD)

## Create Selectivity variable ADMSSN / APPLCN

FD <- mutate(FD, SELECT = ADMSSN/APPLCN)

#For discrete variables, the tick mark labels are taken directly from levels of the factor. However, 
#sometimes the factor levels have short names that aren't suitable for presentation.
#GGPlot changing factor labels
#bp + scale_x_discrete(breaks=c("ctrl", "trt1", "trt2"),
#labels=c("Control", "Treat 1", "Treat 2"))
      

#### Graph showing shape and color by public, private, private for-profit (USE) ####
labels <-c("1" ="Public", "2"="Private Not-For-Profit","3" ="Private For-Profit")
ggplot(FD,aes(SELECT,md_earn_wne_p10)) + 
  geom_point(aes(color=STUFACR, size = 2))+
  scale_colour_gradient(low = "#862633",high = "#EED484",space = "Lab",guide = "colourbar")+
  geom_smooth(method = "lm")+
  #facet_grid(CONTROL ~ ., labeller=labeller(CONTROL = labels))+
  geom_hline(aes(yintercept=mean(md_earn_wne_p10, na.rm=TRUE)),colour="#BB0000", linetype="dashed")+
  xlab("Institution Selectivitiy")+
  ylab("10 Year Median Income")+
  scale_y_continuous(breaks=seq(0,150000, by=10000),labels=dollar)+
  scale_x_continuous(breaks=seq(0.00,1.00, by=.05),labels = percent)+
  geom_text(aes(label=ifelse(md_earn_wne_p10 > 95000,as.character(INSTNM),'')
                ,vjust=0, hjust=-0.1,check_overlap=TRUE))+
  theme_minimal(base_family="Helvetica")


#### Graph showing shape and color by public, private, private for-profit (VERY INTERESTING) (USE)####
labels <-c("1" ="Public", "2"="Private Not-For-Profit","3" ="Private For-Profit")
ggplot(FD,aes(SELECT,md_earn_wne_p10)) + 
  geom_point(aes(color=STUFACR, size = 2))+
  scale_colour_gradient(low = "#862633",high = "#EED484",space = "Lab",guide = "colourbar")+
  geom_smooth(method = "lm")+
  facet_grid(CONTROL ~ ., labeller=labeller(CONTROL = labels))+
  geom_hline(aes(yintercept=mean(md_earn_wne_p10, na.rm=TRUE)),colour="#BB0000", linetype="dashed")+
  xlab("Institution Selectivitiy")+
  ylab("10 Year Median Income")+
  scale_y_continuous(breaks=seq(0,120000, by=25000),labels=dollar)+
  scale_x_continuous(breaks=seq(0.00,1.00, by=.05),labels = percent)+
  geom_text(aes(label=ifelse(md_earn_wne_p10 > 75000,as.character(INSTNM),'')
                ,vjust=0, hjust=-0.1,check_overlap=TRUE))+
  theme_bw(base_family="Helvetica")


###### Florida State Compared to all of Carnegie class USE ########
Carnegie_15 <- subset(FD[CARNEGIE == "15"])
Carnegie_15 <- mutate(Carnegie_15,name = INSTNM == "Florida State University" )
write.csv(Carnegie_15,"carnegis.csv")
ggplot(Carnegie_15,aes(SELECT,md_earn_wne_p10)) + 
  geom_point()+
  geom_smooth(method = "lm")+
  geom_hline(aes(yintercept=mean(md_earn_wne_p10, na.rm=TRUE)),colour="#BB0000", linetype="dashed")+
  xlab("Select")+
  ylab("10 Year Median Income")+
  geom_text(aes(label=ifelse(INSTNM=="Florida State University",as.character(INSTNM),'')
                ,vjust=0, hjust=-0.1))+
  scale_y_continuous(breaks=seq(0,150000, by=10000),labels=dollar)+
  scale_x_continuous(breaks=seq(0.00,1.00, by=.05),labels = percent)+
  geom_text(aes(label=ifelse(md_earn_wne_p10 > 70000,as.character(INSTNM),'')
                ,vjust=0, hjust=-0.1,check_overlap=TRUE))+
  theme_minimal(base_family="Helvetica")


#scale_colour_gradient(low = "#862633",high = "#EED484",space = "Lab",guide = "colourbar")+

#### Peer Comp  USE ####
Peer_Asp <- read.csv("peer_asp.csv",header=TRUE,sep=",")
write.csv(Peer_Asp,"peer_asp.csv")
ggplot(Peer_Asp,aes(reorder_size(INSTNM),md_earn_wne_p10, fill=factor(COMP),order=md_earn_wne_p10)) + 
  geom_bar(stat="identity")+
  #scale_fill_manual( values=c("#862633", "#EED484", "#8D744A"),name="Institutional Sector",
  #                  breaks=c("2","1","3" ),
  #                 labels=c("Private Not-for-Profit","Public", "Private for-Profit"))+
  coord_flip()+
  ylab("")+
  xlab("")+
  scale_y_continuous(breaks=seq(0,60000, by=10000),labels=dollar)+
  geom_hline(aes(yintercept=mean(md_earn_wne_p10, na.rm=TRUE)),colour="#BB0000", linetype="dashed")+
  theme_bw(base_family="Helvetica")

#### Earnings by State per capita & institutions USE #####
q








##Histogram of earnings overall #############
ggplot(FD,aes(md_earn_wne_p10)) + 
  geom_histogram(colour="black", fill="white")+
  geom_vline(aes(xintercept=mean(md_earn_wne_p10, na.rm=T)),   # Ignore NA values for mean
             color="red", linetype="dashed", size=1)+
  xlab("10 Year Median Income")+
  ylab("Number of Institutions")+
  theme_minimal()

### Histogram of Earnings by Sector ####
labels <-c("1" ="Public", "2"="Private Not-For-Profit","3" ="Private For-Profit")
ggplot(FD,aes(md_earn_wne_p10)) + 
  geom_histogram(color="black", fill="white")+
  geom_vline(aes(xintercept=mean(md_earn_wne_p10, na.rm=T)),   # Ignore NA values for mean
             color="red", linetype="dashed", size=1)+
  facet_grid(CONTROL ~ ., labeller=labeller(CONTROL = labels))+
  xlab("10 Year Median Income")+
  ylab("Number of Institutions")+
  theme_minimal()

##### Box Plot by Sector (GOOD) #####
ggplot(FD,aes(factor(CONTROL),md_earn_wne_p10, fill=factor(CONTROL))) + 
  geom_boxplot(outlier.colour = "red", outlier.shape = 1)+
  stat_summary(fun.y=mean, geom="point", shape=23, size=4)+
  geom_hline(aes(yintercept=mean(md_earn_wne_p10, na.rm=T)),   # Ignore NA values for mean
             color="red", linetype="dashed", size=1)+
  scale_fill_manual( values=c("#862633", "#EED484", "#8D744A"),name="Institutional Sector",
                      breaks=c("1", "2", "3"),
                      labels=c("Public", "Private Not-for-Profit", "Private for-Profit"))+
  ylab("10 Year Median Income")+
  xlab("Institutional Sector")+
  scale_y_continuous(breaks=seq(0,120000, by=25000),labels=dollar)+
  #scale_x_continuous(breaks=seq(0.00,1.00, by=.05),labels = percent)+
  geom_text(aes(label=ifelse(md_earn_wne_p10 > 75000,as.character(INSTNM),'')
                ,vjust=0, hjust=-0.1,check_overlap=TRUE))+
  theme_minimal(base_family="Helvetica")
  #annotate("text", x = 3.3, y = 42000, label = "Some text")+




##### Breakout Of Public Institutions Earnings #####
Single <- ddply(FD,~CARNEGIE,summarise,mean=mean(md_earn_wne_p10,na.rm = TRUE))


######  HeatMap ##########

row.names(Earnings) <- Earnings$State

Earnings$State <- NULL

Earnings_matrix <- data.matrix(Earnings)

earnings_heatmap <- heatmap(Earnings_matrix, Rowv=NA, Colv=NA, col = cm.colors(256), scale="column", margins=c(5,5))


US <- get_map(location = 'United States',zoom = 4,color = "color",source = "google",maptype = "hybrid")

ggmap(US)

ggmap(US) + geom_tile(FD, aes(x = LONGITUD, y = LATITUDE, alpha = !is.na(md_earn_wne_p10), fill = 'red'))


                      theme(axis.title.y = element_blank(), axis.title.x = element_blank())


theme(axis.title.y = element_blank(), axis.title.x = element_blank())
summary(FD$LATITUDE)


##### Graph showing shape by control and color by sector #####
ggplot(FD,aes(SELECT,md_earn_wne_p10, shape= factor(CONTROL))) + 
geom_point(aes(color = factor(SECTOR)))+
  #geom_abline(intercept = 51227, slope = -15631)+
  geom_smooth(aes(group=factor(CONTROL), method="lm"))+
theme_minimal()

#website to find this http://docs.ggplot2.org/0.9.3/geom_abline.html
coef(lm(md_earn_wne_p10 ~ SELECT, data = FD))

##################################



##### Avg 10yr $ ####
mean(FD$md_earn_wne_p10,na.rm = TRUE)


ggplot(FD, aes(SECTOR, y=total_bill, group=1)) +
  geom_line()
#here look here 4_25

FD %>% group_by(STABBR) %>% summarise(mean(md_earn_wne_p10,na.rm=TRUE)) -> Mean_earnings_state
FD %>% group_by(SECTOR) %>% summarise(mean(md_earn_wne_p10,na.rm=TRUE)) -> Mean_earnings_sector
FD %>% group_by(CARNEGIE) %>% summarise(mean(md_earn_wne_p10,na.rm=TRUE)) -> Mean_earnings_Car
FD %>% group_by(CCSIZSET) %>% summarise(mean(md_earn_wne_p10,na.rm=TRUE)) -> Mean_earnings_Size

FD %>% group_by(CCSIZSET) %>% mutate(mean(md_earn_wne_p10,na.rm=TRUE)) -> Test

mutate(FD quantile(FD$md_earn_wne_p10, probs = seq(0, 1, 0.25), na.rm = TRUE, names = TRUE, type = 7)

       
       
       
########## Top 10% of institutions by earnings #####
filter <- filter(FD, cume_dist(desc(md_earn_wne_p10)) < 0.1)
labels <-c("1" ="Public", "2"="Private Not-For-Profit","3" ="Private For-Profit")
ggplot(filter,aes(SELECT,md_earn_wne_p10)) + 
  geom_point(aes(color=STUFACR, size = 2))+
  scale_colour_gradient(low = "#862633",high = "#EED484",space = "Lab",guide = "colourbar")+
  geom_smooth(method = "lm")+
  #facet_grid(CONTROL ~ ., labeller=labeller(CONTROL = labels))+
  geom_hline(aes(yintercept=mean(md_earn_wne_p10, na.rm=TRUE)),colour="#BB0000", linetype="dashed")+
  xlab("Institution Selectivitiy")+
  ylab("10 Year Median Income")+
  theme_bw(base_family="Helvetica")

########## Top 10% of institutions by earnings By FACTSTUR #####
filter <- filter(FD, cume_dist(desc(md_earn_wne_p10)) < 0.1)
labels <-c("1" ="Public", "2"="Private Not-For-Profit","3" ="Private For-Profit")
ggplot(filter,aes(STUFACR,md_earn_wne_p10)) + 
  geom_point(aes(color=SELECT, size = 2))+
  scale_colour_gradient(low = "#862633",high = "#EED484",space = "Lab",guide = "colourbar")+
  geom_smooth(method = "lm")+
  #facet_grid(CONTROL ~ ., labeller=labeller(CONTROL = labels))+
  geom_hline(aes(yintercept=mean(md_earn_wne_p10, na.rm=TRUE)),colour="#BB0000", linetype="dashed")+
  xlab("Institution Selectivitiy")+
  ylab("10 Year Median Income")+
  theme_bw(base_family="Helvetica")

##### Peer and Aspiratoinal Institutions #####
ggplot(subset(FD[INSTNM %in% c("Alabama A & M University","Auburn University")]), aes(md_earn_wne_p10)) + 
  geom_histogram(binwidth = 5)
FD <- data.table(FD)
Peer_Asp <- subset(FD[INSTNM %in% c("Florida State University","Georgia State University","University of Georgia",
                                    "Indiana State University","Iowa State University","Kansas State University",
                                    "Michigan State University","University of Maryland-College Park",
                                    "Missouri State University-Springfield","Ohio State University-Main Campus")])
write.csv(Peer_Asp,"peer_asp.csv")
##### Not Using #####
labels <-c("1" ="Public", "2"="Private Not-For-Profit","3" ="Private For-Profit")
ggplot(Peer_Asp,aes(SELECT,md_earn_wne_p10)) + 
  geom_point(aes(color=STUFACR, size = 2))+
  scale_colour_gradient(low = "#862633",high = "#EED484",space = "Lab",guide = "colourbar")+
  geom_smooth(method = "lm")+
  facet_grid(CONTROL ~ ., labeller=labeller(CONTROL = labels))+
  geom_hline(aes(yintercept=mean(md_earn_wne_p10, na.rm=TRUE)),colour="#BB0000", linetype="dashed")+
  xlab("Select")+
  ylab("10 Year Median Income")+
  geom_text(label = Peer_Asp$INSTNM)+
  theme_bw(base_family="Helvetica")

## Graduate earning by peers barchart
labels <-c("1" ="Public", "2"="Private Not-For-Profit","3" ="Private For-Profit")
ggplot(Peer_Asp,aes(INSTNM,md_earn_wne_p10)) + 
  geom_bar(stat="identity", aes(fill=INSTNM)) +
  xlim("Indiana State University","Missouri State University-Springfield","Georgia State University",
       "Ohio State University-Main Campus","Kansas State University","Florida State University",
       "University of Georgia","Iowa State University","Michigan State University",
       "University of Maryland-College Park") +
  guides(fill=FALSE) +
  #scale_fill_gradientn(colours=RColorBrewer::brewer.pal("Blues"))+
  geom_hline(aes(yintercept=mean(md_earn_wne_p10, na.rm=TRUE)),colour="#BB0000", linetype="dashed")+
  xlab("")+
  ylab("10 Year Median Income")+
  theme_bw(base_family="Helvetica")+
  coord_flip()
  

###### top 10% Histogram ##### 
labels <-c("1" ="Public", "2"="Private Not-For-Profit","3" ="Private For-Profit")
ggplot(filter,aes(md_earn_wne_p10)) + 
  geom_histogram(color="black", fill="white")+
  geom_vline(aes(xintercept=mean(md_earn_wne_p10, na.rm=T)),   # Ignore NA values for mean
             color="red", linetype="dashed", size=1)+
  facet_grid(CONTROL ~ ., labeller=labeller(CONTROL = labels))+
  xlab("10 Year Median Income")+
  ylab("Number of Institutions")+
  theme_minimal()

###### top 10% boxplot ##### 
ggplot(filter,aes(factor(CONTROL),md_earn_wne_p10, fill=factor(CONTROL))) + 
  geom_boxplot(outlier.colour = "red", outlier.shape = 1)+
  stat_summary(fun.y=mean, geom="point", shape=23, size=4)+
  geom_hline(aes(yintercept=mean(md_earn_wne_p10, na.rm=T)),   # Ignore NA values for mean
             color="red", linetype="dashed", size=1)+
  scale_fill_manual( values=c("#862633", "#EED484", "#8D744A"),name="Institutional Sector",
                     breaks=c("1", "2", "3"),
                     labels=c("Public", "Private Not-for-Profit", "Private for-Profit"))+
  ylab("10 Year Median Income")+
  xlab("Institutional Sector")+
  #annotate("text", x = 3.3, y = 42000, label = "Some text")+
  theme_minimal()



##### top 10% barchart by sector ###### 
reorder_size <- function(x) {
  factor(x, levels = names(sort(table(x))))
}

ggplot(filter,aes(reorder_size(factor(CONTROL)), fill=factor(CONTROL))) + 
  geom_bar()+
  scale_fill_manual( values=c("#862633", "#EED484", "#8D744A"),name="Institutional Sector",
                     breaks=c("2","1","3" ),
                     labels=c("Private Not-for-Profit","Public", "Private for-Profit"))+
  coord_flip()+
  ylab("")+
  xlab("")+
  #annotate("text", x = 3.3, y = 42000, label = "Some text")+
  #scale_y_continuous(breaks=seq(0,120000, by=25000),labels=dollar)+
  #scale_x_continuous(breaks=seq(0.00,1.00, by=.05),labels = percent)+
  #geom_text(aes(label=ifelse(md_earn_wne_p10 > 75000,as.character(INSTNM),'')
   #             ,vjust=0, hjust=-0.1,check_overlap=TRUE))+
  theme_bw(base_family="Helvetica")

##### top 10% barchart by CARNEGIE Class ###### 
ggplot(filter,aes(reorder_size(factor(CARNEGIE)), fill=factor(CARNEGIE))) + 
  geom_bar()+
  coord_flip()+
  ylab("")+
  xlab("")+
  scale_fill_discrete(name="Institutional Sector",
                     breaks=c("15","21","31","-3","16","53","54","40","59","32","55","22"),
                     labels=c("Doctoral/Research Universities-Extensive","Master's Colleges and Universities" ,
                              "Baccalaureate Colleges-Liberal Arts","Not Applicable",
                              "Doctoral/Research Universities-Intensive","Specialized Institutions-Other separate health profession schools",
                              "Specialized Institutions-Schools of engineering and technology","Associate's Colleges",
                              "Specialized Institutions-Other specialized institutions","Baccalaureate Colleges-General",
                              "Specialized Institutions-Schools of business and management","Master's Colleges and Universities II"
))+
  theme_minimal(base_family="Helvetica")


####### Avaerage Selectivity by Carnegie Class #####
DT <- data.table(filter)
DT_2 <- data.table(FD)
DT_2[,.(SELECT.mean = mean(SELECT, na.rm=TRUE)),by=factor(CARNEGIE)]
DT_2[,.(SELECT.mean = mean(SELECT, na.rm=TRUE)),by=factor(CONTROL)]

DT[,.(SELECT.median = median(SELECT, na.rm=TRUE)),by=factor(CONTROL)]
DT_2[,.(SELECT.median = median(SELECT, na.rm=TRUE)),by=factor(CONTROL)]

#annotate("text", x = 3.3, y = 42000, label = "Some text")+

#scale_fill_brewer(palette="RdYlGn")+

# Color brewer palettes see choices display.brewer.all()

##### Institutional Earnings Percentiles ####
dfr$myrank<-rank(dfr$myvar) #for normal ranks, or 
FD$myrank<-rank(FD$md_earn_wne_p10)/length(FD$md_earn_wne_p10) #for percentile ranks.


