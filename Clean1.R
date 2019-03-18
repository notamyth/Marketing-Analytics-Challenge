setwd("C:\Amit\Competition\MAC\2018_UTD_Morris_Hite_Customer_Insights_Competition_Rules_final")

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

data <- read.csv("data1.csv")
#View(data)

set.seed(123)

#WC_num items
addNA(data$wc_num_items)
data$wc_num_items[data$wc_num_items==""] <- NA
data$wc_num_items
clean1 <- select(data,3:24,26)

tempdata <- mice(clean1, m=5, maxit = 5, meth = 'polyreg')
completedata <- complete(tempdata,1)
data$wc_num_items <- completedata$wc_num_items
#WC_big ticket_num item
addNA(data$wc_big_ticket_num_items)
data$wc_big_ticket_num_items[data$wc_big_ticket_num_items==""] <- NA
data$wc_big_ticket_num_items
clean1 <- select(data,3:27,28)

tempdata <- mice(clean1, m=5, maxit = 5, meth = 'polyreg')
completedata <- complete(tempdata,1)
data$wc_big_ticket_num_items <- completedata$wc_big_ticket_num_items

#wc_low ticket num items
addNA(data$wc_low_ticket_num_items)
data$wc_low_ticket_num_items[data$wc_low_ticket_num_items==""] <- NA
data$wc_low_ticket_num_items
clean1 <- select(data,3:29,30)

tempdata <- mice(clean1, m=5, maxit = 5, meth = 'polyreg')
completedata <- complete(tempdata,1)
data$wc_low_ticket_num_items <- completedata$wc_low_ticket_num_items

#wc_big ticket amt spent
addNA(data$wc_big_ticket_amt_spent)
data$wc_big_ticket_amt_spent[data$wc_big_ticket_amt_spent==""] <- NA
data$wc_big_ticket_amt_spent
clean1 <- select(data,3:31)

tempdata <- mice(clean1, m=5, maxit = 5, meth = 'polyreg')
completedata <- complete(tempdata,1)
data$wc_big_ticket_amt_spent <- completedata$wc_big_ticket_amt_spent

#wc_low ticket amt spent
addNA(data$wc_low_ticket_amt_spent)
data$wc_low_ticket_amt_spent[data$wc_low_ticket_amt_spent==""] <- NA
data$wc_low_ticket_amt_spent
clean1 <- select(data,3:32)#

tempdata <- mice(clean1, m=5, maxit = 5, meth = 'polyreg')
completedata <- complete(tempdata,1)
data$wc_low_ticket_amt_spent <- completedata$wc_low_ticket_amt_spent

# 
# #CLient 30 days
# addNA(data$client_times_shopped_30_days)
# data$client_times_shopped_30_days[data$client_times_shopped_30_days==""] <- NA
# data$client_times_shopped_30_days
# clean1 <- select(data,3:33)
# 
# tempdata <- mice(clean1, m=5, maxit = 5, meth = 'polyreg')
# completedata <- complete(tempdata,1)
# data$client_times_shopped_30_days <- completedata$client_times_shopped_30_days
# 
# #comp1 30 days
# addNA(data$comp1_times_shopped_30_days)
# data$comp1_times_shopped_30_days[data$comp1_times_shopped_30_days==""] <- NA
# data$comp1_times_shopped_30_days
# clean1 <- select(data,3:35)
# 
# tempdata <- mice(clean1, m=5, maxit = 5, meth = 'polyreg')
# completedata <- complete(tempdata,1)
# data$comp1_times_shopped_30_days <- completedata$comp1_times_shopped_30_days
# 
# #comp2 30 days
# addNA(data$comp2_times_shopped_30_days)
# data$comp2_times_shopped_30_days[data$comp2_times_shopped_30_days==""] <- NA
# data$comp2_times_shopped_30_days
# clean1 <- select(data,3:37)
# 
# tempdata <- mice(clean1, m=5, maxit = 5, meth = 'polyreg')
# completedata <- complete(tempdata,1)
# data$comp2_times_shopped_30_days <- completedata$comp2_times_shopped_30_days
# 
# #comp3 30 days
# addNA(data$comp3_times_shopped_30_days)
# data$comp3_times_shopped_30_days[data$comp3_times_shopped_30_days==""] <- NA
# data$comp3_times_shopped_30_days
# clean1 <- select(data,3:39)
# 
# tempdata <- mice(clean1, m=5, maxit = 5, meth = 'polyreg')
# completedata <- complete(tempdata,1)
# data$comp3_times_shopped_30_days <- completedata$comp3_times_shopped_30_days
# 
# #other1 30 days
# addNA(data$other1_times_shopped_30_days)
# data$comp3_times_shopped_30_days[data$comp3_times_shopped_30_days==""] <- NA
# data$comp3_times_shopped_30_days
# clean1 <- select(data,3:39)
# 
# tempdata <- mice(clean1, m=5, maxit = 5, meth = 'polyreg')
# completedata <- complete(tempdata,1)
# data$comp3_times_shopped_30_days <- completedata$comp3_times_shopped_30_days

write.csv(data, "clean1.csv")

