setwd("C:/Competition")

library(tidyverse)
library(rpart)
library(data.table)
library(ggplot2)
library(dplyr)
library(datetime)
library(lubridate)
library(mlogit)
library(foreign)
library(nnet)
library(randomForest)
library(dummy)
library(caTools)
library(naniar)
library(mice)

rm(list=ls(all=TRUE))

data <- read.csv("utdallas_contest_data-1.csv")
#View(data)

set.seed(123)

data1 <- select(data,4,6,18,17,16,15,21,26,27,56,69,70,122,163,239,243,251,406,416,469,468,467,558,559,617:684)
#View(data1)

data1 <- data1[!is.na(data1$match),]
data1 <- data1[!(data1$match=="N"),]

# #Cleaning wc_num_items
# data1$wc_num_items <- factor(data1$wc_num_items, levels = c("A", "B","C"), labels = c(1,2,3))
# 
# as.numeric(data1$wc_num_items)
# sum(is.na(data1$wc_num_items))
# 
# data1$wc_num_items <- ifelse(is.na(data1$wc_num_items), ceiling(runif(20481,min = 1,max=3)) ,data1$wc_num_items)
# as.factor(data1$wc_num_items)
# 
# #Cleaning wc_low_ticket_num_items
# data1$wc_low_ticket_num_items <- factor(data1$wc_low_ticket_num_items, levels = c("A", "B","C"), labels = c(1,2,3))
# 
# as.numeric(data1$wc_low_ticket_num_items)
# sum(is.na(data1$wc_low_ticket_num_items))
# 
# data1$wc_low_ticket_num_items <- ifelse(is.na(data1$wc_low_ticket_num_items), ceiling(runif(20481,min = 1,max=3)) ,data1$wc_low_ticket_num_items)
# as.factor(data1$wc_low_ticket_num_items)
# 
# #Cleaning wc_low_ticket_amt_spent
# data1$wc_low_ticket_amt_spent <- factor(data1$wc_low_ticket_amt_spent, levels = c("A", "B","C"), labels = c(1,2,3))
# 
# as.numeric(data1$wc_low_ticket_amt_spent)
# sum(is.na(data1$wc_low_ticket_amt_spent))
# 
# data1$wc_low_ticket_amt_spent <- ifelse(is.na(data1$wc_low_ticket_amt_spent), ceiling(runif(20481,min = 1,max=3)) ,data1$wc_low_ticket_amt_spent)
# as.factor(data1$wc_low_ticket_amt_spent)
# 
# #View(data1)
# 
# #Cleaning other12_shopped_30_days
# data1$other12_times_shopped_30_days <- factor(data1$other12_times_shopped_30_days, levels = c("A","B","C","D","E","F","G","H","I"), labels = c(1,2,3,4,5,6,7,8,9))
# 
# as.numeric(data1$other12_times_shopped_30_days)
# sum(is.na(data1$other12_times_shopped_30_days))
# 
# data1$other12_times_shopped_30_days <- ifelse(is.na(data1$wc_low_ticket_amt_spent), ceiling(runif(20481,min = 1,max=3)) ,data1$wc_low_ticket_amt_spent)
# as.factor(data1$wc_low_ticket_amt_spent)
# 
# #Cleaning other13_times_shopped_30_days
# data1$other13_times_shopped_30_days <- factor(data1$other13_times_shopped_30_days, levels = c("A","B","C","D","E","F","G","H","I"), labels = c(1,2,3,4,5,6,7,8,9))
# 
# as.numeric(data1$other13_times_shopped_30_days)
# sum(is.na(data1$other13_times_shopped_30_days))
# 
# data1$other12_times_shopped_30_days <- ifelse(is.na(data1$wc_low_ticket_amt_spent), ceiling(runif(20481,min = 1,max=3)) ,data1$wc_low_ticket_amt_spent)
# as.factor(data1$wc_low_ticket_amt_spent)
# 
# #Cleaning other14_shopped_3_mos
# data1$wc_low_ticket_amt_spent <- factor(data1$wc_low_ticket_amt_spent, levels = c("A", "B","C"), labels = c(1,2,3))
# 
# as.numeric(data1$wc_low_ticket_amt_spent)
# sum(is.na(data1$wc_low_ticket_amt_spent))
# 
# data1$wc_low_ticket_amt_spent <- ifelse(is.na(data1$wc_low_ticket_amt_spent), ceiling(runif(20481,min = 1,max=3)) ,data1$wc_low_ticket_amt_spent)
# as.factor(data1$wc_low_ticket_amt_spent)
# 
# #Cleaning other15_shopped_3_mos
# data1$wc_low_ticket_amt_spent <- factor(data1$wc_low_ticket_amt_spent, levels = c("A", "B","C"), labels = c(1,2,3))
# 
# as.numeric(data1$wc_low_ticket_amt_spent)
# sum(is.na(data1$wc_low_ticket_amt_spent))
# 
# data1$wc_low_ticket_amt_spent <- ifelse(is.na(data1$wc_low_ticket_amt_spent), ceiling(runif(20481,min = 1,max=3)) ,data1$wc_low_ticket_amt_spent)
# as.factor(data1$wc_low_ticket_amt_spent)
# 
# #Cleaning other16_shopped_3_mos
# data1$wc_low_ticket_amt_spent <- factor(data1$wc_low_ticket_amt_spent, levels = c("A", "B","C"), labels = c(1,2,3))
# 
# as.numeric(data1$wc_low_ticket_amt_spent)
# sum(is.na(data1$wc_low_ticket_amt_spent))
# 
# data1$wc_low_ticket_amt_spent <- ifelse(is.na(data1$wc_low_ticket_amt_spent), ceiling(runif(20481,min = 1,max=3)) ,data1$wc_low_ticket_amt_spent)
# as.factor(data1$wc_low_ticket_amt_spent)
# 
# #Cleaning other17_shopped_3_mos
# data1$wc_low_ticket_amt_spent <- factor(data1$wc_low_ticket_amt_spent, levels = c("A", "B","C"), labels = c(1,2,3))
# 
# as.numeric(data1$wc_low_ticket_amt_spent)
# sum(is.na(data1$wc_low_ticket_amt_spent))
# 
# data1$wc_low_ticket_amt_spent <- ifelse(is.na(data1$wc_low_ticket_amt_spent), ceiling(runif(20481,min = 1,max=3)) ,data1$wc_low_ticket_amt_spent)
# as.factor(data1$wc_low_ticket_amt_spent)
# 
# #Cleaning other18_shopped_3_mos
# data1$wc_low_ticket_amt_spent <- factor(data1$wc_low_ticket_amt_spent, levels = c("A", "B","C"), labels = c(1,2,3))
# 
# as.numeric(data1$wc_low_ticket_amt_spent)
# sum(is.na(data1$wc_low_ticket_amt_spent))
# 
# data1$wc_low_ticket_amt_spent <- ifelse(is.na(data1$wc_low_ticket_amt_spent), ceiling(runif(20481,min = 1,max=3)) ,data1$wc_low_ticket_amt_spent)
# as.factor(data1$wc_low_ticket_amt_spent)
# 
# #Cleaning other19_shopped_3_mos
# data1$wc_low_ticket_amt_spent <- factor(data1$wc_low_ticket_amt_spent, levels = c("A", "B","C"), labels = c(1,2,3))
# 
# as.numeric(data1$wc_low_ticket_amt_spent)
# sum(is.na(data1$wc_low_ticket_amt_spent))
# 
# data1$wc_low_ticket_amt_spent <- ifelse(is.na(data1$wc_low_ticket_amt_spent), ceiling(runif(20481,min = 1,max=3)) ,data1$wc_low_ticket_amt_spent)
# as.factor(data1$wc_low_ticket_amt_spent)
# 
# #Cleaning other20_shopped_3_mos
# data1$wc_low_ticket_amt_spent <- factor(data1$wc_low_ticket_amt_spent, levels = c("A", "B","C"), labels = c(1,2,3))
# 
# as.numeric(data1$wc_low_ticket_amt_spent)
# sum(is.na(data1$wc_low_ticket_amt_spent))
# 
# data1$wc_low_ticket_amt_spent <- ifelse(is.na(data1$wc_low_ticket_amt_spent), ceiling(runif(20481,min = 1,max=3)) ,data1$wc_low_ticket_amt_spent)
# as.factor(data1$wc_low_ticket_amt_spent)
# 
# #Cleaning other21_shopped_3_mos
# data1$wc_low_ticket_amt_spent <- factor(data1$wc_low_ticket_amt_spent, levels = c("A", "B","C"), labels = c(1,2,3))
# 
# as.numeric(data1$wc_low_ticket_amt_spent)
# sum(is.na(data1$wc_low_ticket_amt_spent))
# 
# data1$wc_low_ticket_amt_spent <- ifelse(is.na(data1$wc_low_ticket_amt_spent), ceiling(runif(20481,min = 1,max=3)) ,data1$wc_low_ticket_amt_spent)
# as.factor(data1$wc_low_ticket_amt_spent)
# 
# #Cleaning other22_shopped_3_mos
# data1$wc_low_ticket_amt_spent <- factor(data1$wc_low_ticket_amt_spent, levels = c("A", "B","C"), labels = c(1,2,3))
# 
# as.numeric(data1$wc_low_ticket_amt_spent)
# sum(is.na(data1$wc_low_ticket_amt_spent))
# 
# data1$wc_low_ticket_amt_spent <- ifelse(is.na(data1$wc_low_ticket_amt_spent), ceiling(runif(20481,min = 1,max=3)) ,data1$wc_low_ticket_amt_spent)
# as.factor(data1$wc_low_ticket_amt_spent)
# 
# #Cleaning other23_shopped_3_mos
# data1$wc_low_ticket_amt_spent <- factor(data1$wc_low_ticket_amt_spent, levels = c("A", "B","C"), labels = c(1,2,3))
# 
# as.numeric(data1$wc_low_ticket_amt_spent)
# sum(is.na(data1$wc_low_ticket_amt_spent))
# 
# data1$wc_low_ticket_amt_spent <- ifelse(is.na(data1$wc_low_ticket_amt_spent), ceiling(runif(20481,min = 1,max=3)) ,data1$wc_low_ticket_amt_spent)
# as.factor(data1$wc_low_ticket_amt_spent)
# 
# #Cleaning other24_shopped_3_mos
# data1$wc_low_ticket_amt_spent <- factor(data1$wc_low_ticket_amt_spent, levels = c("A", "B","C"), labels = c(1,2,3))
# 
# as.numeric(data1$wc_low_ticket_amt_spent)
# sum(is.na(data1$wc_low_ticket_amt_spent))
# 
# data1$wc_low_ticket_amt_spent <- ifelse(is.na(data1$wc_low_ticket_amt_spent), ceiling(runif(20481,min = 1,max=3)) ,data1$wc_low_ticket_amt_spent)
# as.factor(data1$wc_low_ticket_amt_spent)
# 
# #Cleaning other25_shopped_3_mos
# data1$wc_low_ticket_amt_spent <- factor(data1$wc_low_ticket_amt_spent, levels = c("A", "B","C"), labels = c(1,2,3))
# 
# as.numeric(data1$wc_low_ticket_amt_spent)
# sum(is.na(data1$wc_low_ticket_amt_spent))
# 
# data1$wc_low_ticket_amt_spent <- ifelse(is.na(data1$wc_low_ticket_amt_spent), ceiling(runif(20481,min = 1,max=3)) ,data1$wc_low_ticket_amt_spent)
# as.factor(data1$wc_low_ticket_amt_spent)
# 
# #Cleaning other26_shopped_3_mos
# data1$wc_low_ticket_amt_spent <- factor(data1$wc_low_ticket_amt_spent, levels = c("A", "B","C"), labels = c(1,2,3))
# 
# as.numeric(data1$wc_low_ticket_amt_spent)
# sum(is.na(data1$wc_low_ticket_amt_spent))
# 
# data1$wc_low_ticket_amt_spent <- ifelse(is.na(data1$wc_low_ticket_amt_spent), ceiling(runif(20481,min = 1,max=3)) ,data1$wc_low_ticket_amt_spent)
# as.factor(data1$wc_low_ticket_amt_spent)


####################################


sum(is.na(data1$Country_of_Origin_U324574))


#Replace Presence of child columns NA values by "N"
# as.factor(data1$Presence_Children_90311)
# data1$Presence_Children_90311 <- ifelse(is.na(data1$Presence_Children_90311), "N" ,data1$Presence_Children_90311)
# data1$Presence_Children_90311
# data1$Presence_Children_90311 <- factor(data1$Presence_Children_90311, levels = c("2", "1"), labels = c("Y" , "N"))

#Replace Num of Child column NA values by 0
as.factor(data1$Num_Child_in_HH_90307)
data1$Num_Child_in_HH_90307 <- ifelse(is.na(data1$Num_Child_in_HH_90307), 0 ,data1$Num_Child_in_HH_90307)
data1$Num_Child_in_HH_90307

#Replace Num of adults by median or random values
# as.factor(data1$Num_Adults_HH_90306)
# data1$Num_Adults_HH_90306 <- ifelse(is.na(data1$Num_Adults_HH_90306), ceiling(runif(4928,min = 1,max=6)) ,data1$Num_Adults_HH_90306)
#as.character(data1$Religion_U324573)

#Replace mail order responder NA values by "U"
as.factor(data1$Mail_Order_Respnder_U324068)
data1$Mail_Order_Respnder_U324068 <- ifelse(is.na(data1$Mail_Order_Respnder_U324068), "U" ,data1$Mail_Order_Respnder_U324068)
data1$Mail_Order_Respnder_U324068
data1$Mail_Order_Respnder_U324068 <- factor(data1$Mail_Order_Respnder_U324068, levels = c("1", "2", "3"), labels = c("U","M", "Y"))
#as.character(data1$Mail_Order_Respnder_U324068)

#Replace mail order responder NA values by "U"
as.factor(data1$Mail_Order_Byr_U324069)
data1$Mail_Order_Byr_U324069 <- ifelse(is.na(data1$Mail_Order_Byr_U324069), "U" ,data1$Mail_Order_Byr_U324069)
data1$Mail_Order_Byr_U324069
data1$Mail_Order_Byr_U324069 <- factor(data1$Mail_Order_Byr_U324069, levels = c("1", "2", "3", "4"), labels = c("U","M", "P","Y"))
#as.character(data1$Mail_Order_Byr_U324069)

#replace survey online purchase
data1$Srvy_Online_Purchr_U324253
data1$Srvy_Online_Purchr_U324253 <- factor(data1$Srvy_Online_Purchr_U324253, levels = c("Y", ""), labels = c("Y","N"))
#as.character(data1$Srvy_Online_Purchr_U324253)

#replace Survey mail order clothing
data1$Srvy_Mail_Ord_Clthg_U324257
data1$Srvy_Mail_Ord_Clthg_U324257 <- factor(data1$Srvy_Mail_Ord_Clthg_U324257, levels = c("Y", ""), labels = c("Y","N"))
#as.character(data1$Srvy_Mail_Ord_Clthg_U324257)

#replace Survey mail order Plus size clothing
data1$Srvy_Mail_Ord_PlsSz_U324265
data1$Srvy_Mail_Ord_PlsSz_U324265 <- factor(data1$Srvy_Mail_Ord_PlsSz_U324265, levels = c("Y", ""), labels = c("Y","N"))
#as.character(data1$Srvy_Mail_Ord_PlsSz_U324265)

#replace buyer catalog apparel
data1$Byr_Cat_Apprl_Gen_U324430
data1$Byr_Cat_Apprl_Gen_U324430 <- factor(data1$Byr_Cat_Apprl_Gen_U324430, levels = c("Y", ""), labels = c("Y","N"))
#as.character(data1$Byr_Cat_Apprl_Gen_U324430)

#replace buyer catalog apparel
data1$Byr_Ret_Apprl_Gen_U324483
data1$Byr_Ret_Apprl_Gen_U324483 <- factor(data1$Byr_Ret_Apprl_Gen_U324483, levels = c("Y", ""), labels = c("Y","N"))
#as.character(data1$Byr_Ret_Apprl_Gen_U324483)
data1$Presence_Srvy_Data_U324177<-NULL

# replace religion unknown with X
as.factor(data1$Religion_U324573)
levels(data1$Religion_U324573) <- c(levels(data1$Religion_U324573), "X") 
data1$Religion_U324573[data1$Religion_U324573==""] <- "X"
(data1$Religion_U324573)

#Add NA in Race
addNA(data1$Race_Code_90308)
data1$Race_Code_90308[data1$Race_Code_90308==""] <- NA
data1$Race_Code_90308

#Add NA in Age Range
addNA(data1$Age_Range_U324055)
data1$Age_Range_U324055[data1$Age_Range_U324055==""] <- NA
data1$Age_Range_U324055

#Add NA in Occ
addNA(data1$Occ_1st_Indv_90317)
data1$Occ_1st_Indv_90317[data1$Occ_1st_Indv_90317==""] <- NA
data1$Occ_1st_Indv_90317

#Add NA in Voter
addNA(data1$Register_Voter_Prty_U324135)
data1$Register_Voter_Prty_U324135[data1$Register_Voter_Prty_U324135==""] <- NA
data1$Register_Voter_Prty_U324135

#making Edu factor
data1$Edu_1st_Indv_90318 <- factor(data1$Edu_1st_Indv_90318, levels = c("1", "2", "3"), labels = c("1","2","3"))
is.factor(data1$Edu_1st_Indv_90318)
addNA(data1$Edu_1st_Indv_90318)
data1$Edu_1st_Indv_90318[data1$Edu_1st_Indv_90318==""] <- NA
data1$Edu_1st_Indv_90318

#Add NA in number of adults
data1$Num_Adults_HH_90306 <- factor(data1$Num_Adults_HH_90306, levels = c("1", "2", "3","4","5","6"), labels = c("1", "2", "3","4","5","6"))
is.factor(data1$Num_Adults_HH_90306)
addNA(data1$Num_Adults_HH_90306)
data1$Num_Adults_HH_90306[data1$Num_Adults_HH_90306==""] <- NA
data1$Num_Adults_HH_90306

#Add NA in country of origin
data1$Country_of_Origin_U324574 <- factor(data1$Country_of_Origin_U324574, levels = c("1", "2", "3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20"), labels = c("1", "2", "3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20"))
is.factor(data1$Country_of_Origin_U324574)
addNA(data1$Country_of_Origin_U324574)
data1$Country_of_Origin_U324574[data1$Country_of_Origin_U324574==""] <- NA
data1$Country_of_Origin_U324574

X <- data1[,(1:23)]
X$Byr_Ret_Orders_U324481 <- NULL
X$Byr_Ret_Dollars_U324482 <- NULL
X$Byr_Ret_Apprl_Gen_U324483 <- NULL
X$Byr_Cat_Apprl_Gen_U324430 <- NULL
X$Byr_Cat_Total_Dollr_U324420 <- NULL
X$Srvy_Mail_Ord_PlsSz_U324265 <- NULL
X$Srvy_Online_Purchr_U324253 <- NULL
X$Srvy_Mail_Ord_Clthg_U324257 <- NULL
X$Mail_Order_Byr_U324069 <- NULL
X$Mail_Order_Respnder_U324068 <- NULL
X$match <- NULL

#Imputing the missing values using mice and pmm
tempdata <- mice(X, m=5, maxit = 5, meth = 'polr')
completedata <- complete(tempdata,1)


data1$Race_Code_90308 <- completedata$Race_Code_90308
data1$Num_Adults_HH_90306 <- completedata$Num_Adults_HH_90306
data1$Age_Range_U324055 <- completedata$Age_Range_U324055
data1$Occ_1st_Indv_90317 <- completedata$Occ_1st_Indv_90317
data1$Edu_1st_Indv_90318 <- completedata$Edu_1st_Indv_90318
data1$Register_Voter_Prty_U324135 <- completedata$Register_Voter_Prty_U324135  
data1$Country_of_Origin_U324574 <- completedata$Country_of_Origin_U324574

data1$Country_of_Origin_U324574

View(data1)

data1 <- data1[c(1,2,3,4,5,6,7,8,9,10,13,22,23,11,12,14,15,16,17,18,19,20,21,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91)]

write.csv(data1, "data1.csv")
