### LIBRARY CALL ###
library(tidyr)
library(dplyr)
library(data.table)
library(gdata)
library(rpart)
library(rpart.plot)	


#######################################################################################################

### READING DATA ###

#Set working directory
setwd("~/Dropbox/NCAIR_Pres/CL_Western_Carolina/NCAIR_ML_Example")

#Import dataset
Original_Student_Data <- read.xls("Original_Data.xls")

#Put dataset into a data table
DT <- data.table(Original_Student_Data)

#######################################################################################################

### CLEANING DATA ###

#Print List of Variables
names(DT)

#make ID key varible 
setkey(DT,ID)

#Remove Unneeded Data
DT$College <- NULL
DT$Degree.code <- NULL
DT$Program.name <- NULL
DT$Program.code <- NULL
DT$GRE.Analytical.Writing <- NULL
DT$GRE.Quantitative <- NULL
DT$GRE.Verbal <- NULL
DT$UG.GPA <- NULL
DT$ACT...Composite <- NULL
DT$Transfer.hours.applied.to.degree <- NULL
DT$Is.transfer.student <- NULL
DT$Total.degree.credit.hours.earned.at.institution <- NULL
DT$Division.level <- NULL
DT$Degree.level <- NULL
DT$Citizenship <- NULL
DT$Residence...county <- NULL
DT$Residence...state <- NULL
DT$Age <- NULL
DT$Career.level <- NULL
DT$Residence...country <- NULL
DT$Institutional.cumulative.GPA <- NULL
DT$Academic.year <- NULL
DT$Year <- NULL
DT$Admitted.this.term<- NULL
DT$Acceptance.status <- NULL
DT$Term <- NULL
DT$Enrolled.this.term <- NULL
DT$Enrollment.status <- NULL
DT$Semester <-NULL
DT$Applied.this.term <- NULL
DT$Applied.this.term <- NULL
DT$Admittance.status <- NULL

#list current factors
names(DT)

#Combine SAT read & SAT math into a single variable
DT <- mutate(DT, SAT = DT$SAT...Math + DT$SAT...Critical.reading)

#Remove old SAT
DT$SAT...Critical.reading <- NULL
DT$SAT...Math<- NULL

#Filter out incomplete data
DT <- subset(DT, SAT & HS.GPA !=""|NA)

#Create Fake Retention Factor
DT$fake_retention <- as.numeric(DT$Is.new.student.this.term == "Yes")
DT$Is.new.student.this.term <- NULL

#Check the makeup of the fake factor. Some NAs exist and will need to be removed
count(DT, fake_retention)

#Remove NAs
DT <- subset(DT, fake_retention !=""|NA)

#Remove Duplicate IDs
DT <- DT %>% distinct(ID)


#Coding Race
my_table <- table(DT$Race.or.ethnicity)
View(my_table)
DT$Race_Eth <- factor(DT$Race.or.ethnicity)
levels(DT$Race_Eth)


DT$Gender_Factor <- factor(DT$Gender)
levels(Gender_Factor)
is.factor(Gender_Factor)

#######################################################################################################

### CREATING MODEL ###
train_new <- DT[1:3032,]
test_new <- DT[3033:3790,]
test_new$fake_retention <- NULL

# train_new and test_new are available in the workspace
str(train_new)
str(test_new)

# Create a new model `my_tree`
my_tree <- rpart(fake_retention ~   SAT + HS.GPA + Gender_Factor + Race_Eth , data = train_new, method = "class", control=rpart.control(cp=0.0001))

summary(my_tree)

# Visualize your new decision tree
prp(my_tree, type = 4, extra = 100, faclen=0, under = TRUE, compress=FALSE, ycompress=FALSE)

# Make your prediction using `my_tree` and `test_new`
my_prediction <- predict(my_tree, test_new, type = "class")
head(my_prediction)

# Create a data frame with two columns: ID & Retained Survived contains your predictions
vector_studentID <- test_new$ID

my_solution <- data.frame(ID = vector_studentID, Retained = my_prediction)

head(my_solution)

# Write solution to a csv file with the name my_solution.csv
write.csv(my_solution, file = "my_solution.csv",row.names=FALSE)

















