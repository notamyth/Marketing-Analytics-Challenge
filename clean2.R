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

data <- read.csv("clean1.csv")


#Replace client NA values by "A"
# addNA(data$client_times_shopped_30_days)
# data$client_times_shopped_30_days[data$client_times_shopped_30_days==""] <- NA
# data$client_times_shopped_30_days
# as.factor(data$client_times_shopped_30_days)
# data$client_times_shopped_30_days <- ifelse(is.na(data$client_times_shopped_30_days), "A" ,data$client_times_shopped_30_days)
# data$client_times_shopped_30_days
# data$client_times_shopped_30_days <- factor(data$client_times_shopped_30_days, levels = c("1", "2", "3","4","5","6","7","8","9"), labels = c("A","B", "C","D","E","F","G","H","I"))
#as.character(data1$Mail_Order_Respnder_U324068)


#Client
as.factor(data$client_times_shopped_30_days)
#levels(data$client_times_shopped_30_days) <- c(levels(data$client_times_shopped_30_days), "A") 
data$client_times_shopped_30_days[data$client_times_shopped_30_days==""] <- "A"
(data$client_times_shopped_30_days)

#comp1
as.factor(data$comp1_times_shopped_30_days)
data$comp1_times_shopped_30_days[data$comp1_times_shopped_30_days==""] <- "A"
(data$comp1_times_shopped_30_days)

#comp2
as.factor(data$comp2_times_shopped_30_days)
data$comp2_times_shopped_30_days[data$comp2_times_shopped_30_days==""] <- "A"
(data$comp2_times_shopped_30_days)

#comp3
as.factor(data$comp3_times_shopped_30_days)
data$comp3_times_shopped_30_days[data$comp3_times_shopped_30_days==""] <- "A"
(data$comp3_times_shopped_30_days)

#Other1
as.factor(data$other1_times_shopped_30_days)
data$other1_times_shopped_30_days[data$other1_times_shopped_30_days==""] <- "A"
(data$other1_times_shopped_30_days)

#Other2
as.factor(data$other2_times_shopped_30_days)
data$other2_times_shopped_30_days[data$other2_times_shopped_30_days==""] <- "A"
(data$other2_times_shopped_30_days)

#Other3
as.factor(data$other3_times_shopped_30_days)
data$other3_times_shopped_30_days[data$other3_times_shopped_30_days==""] <- "A"
(data$other3_times_shopped_30_days)

#other4
as.factor(data$other4_times_shopped_30_days)
data$other4_times_shopped_30_days[data$other4_times_shopped_30_days==""] <- "A"
(data$other4_times_shopped_30_days)

#Other5
as.factor(data$other5_times_shopped_30_days)
data$other5_times_shopped_30_days[data$other5_times_shopped_30_days==""] <- "A"
(data$other5_times_shopped_30_days)

#Other6
as.factor(data$other6_times_shopped_30_days)
data$other6_times_shopped_30_days[data$other6_times_shopped_30_days==""] <- "A"
(data$other6_times_shopped_30_days)

#Other7
as.factor(data$other7_times_shopped_30_days)
data$other7_times_shopped_30_days[data$other7_times_shopped_30_days==""] <- "A"
(data$other7_times_shopped_30_days)

#other8
as.factor(data$other8_times_shopped_30_days)
data$other8_times_shopped_30_days[data$other8_times_shopped_30_days==""] <- "A"
(data$other8_times_shopped_30_days)

#other9
as.factor(data$other9_times_shopped_30_days)
data$other9_times_shopped_30_days[data$other9_times_shopped_30_days==""] <- "A"
(data$other9_times_shopped_30_days)

#other10
as.factor(data$other10_times_shopped_30_days)
data$other10_times_shopped_30_days[data$other10_times_shopped_30_days==""] <- "A"
(data$other10_times_shopped_30_days)

#other11
as.factor(data$other11_times_shopped_30_days)
data$other11_times_shopped_30_days[data$other11_times_shopped_30_days==""] <- "A"
(data$other11_times_shopped_30_days)

#other12
as.factor(data$other12_times_shopped_30_days)
data$other12_times_shopped_30_days[data$other12_times_shopped_30_days==""] <- "A"
(data$other12_times_shopped_30_days)

#other13
as.factor(data$other13_times_shopped_30_days)
data$other13_times_shopped_30_days[data$other13_times_shopped_30_days==""] <- "A"
(data$other13_times_shopped_30_days)

#other14
as.factor(data$other14_times_shopped_30_days)
data$other14_times_shopped_30_days[data$other14_times_shopped_30_days==""] <- "A"
(data$other14_times_shopped_30_days)

#other15
as.factor(data$other15_times_shopped_30_days)
data$other15_times_shopped_30_days[data$other15_times_shopped_30_days==""] <- "A"
(data$other15_times_shopped_30_days)

#other16
as.factor(data$other16_times_shopped_30_days)
data$other16_times_shopped_30_days[data$other16_times_shopped_30_days==""] <- "A"
(data$other16_times_shopped_30_days)

#other17
as.factor(data$other17_times_shopped_30_days)
data$other17_times_shopped_30_days[data$other17_times_shopped_30_days==""] <- "A"
(data$other17_times_shopped_30_days)

#other18
as.factor(data$other18_times_shopped_30_days)
data$other18_times_shopped_30_days[data$other18_times_shopped_30_days==""] <- "A"
(data$other18_times_shopped_30_days)

#other19
as.factor(data$other19_times_shopped_30_days)
data$other19_times_shopped_30_days[data$other19_times_shopped_30_days==""] <- "A"
(data$other19_times_shopped_30_days)

#other20
as.factor(data$other20_times_shopped_30_days)
data$other20_times_shopped_30_days[data$other20_times_shopped_30_days==""] <- "A"
(data$other20_times_shopped_30_days)

#other21
as.factor(data$other21_times_shopped_30_days)
data$other21_times_shopped_30_days[data$other21_times_shopped_30_days==""] <- "A"
(data$other21_times_shopped_30_days)

#other22
as.factor(data$other22_times_shopped_30_days)
data$other22_times_shopped_30_days[data$other22_times_shopped_30_days==""] <- "A"
(data$other22_times_shopped_30_days)

#other23
as.factor(data$other23_times_shopped_30_days)
data$other23_times_shopped_30_days[data$other23_times_shopped_30_days==""] <- "A"
(data$other23_times_shopped_30_days)

#other24
as.factor(data$other24_times_shopped_30_days)
data$other24_times_shopped_30_days[data$other24_times_shopped_30_days==""] <- "A"
(data$other24_times_shopped_30_days)

#other25
as.factor(data$other25_times_shopped_30_days)
data$other25_times_shopped_30_days[data$other25_times_shopped_30_days==""] <- "A"
(data$other25_times_shopped_30_days)

#other26
as.factor(data$other26_times_shopped_30_days)
data$other26_times_shopped_30_days[data$other26_times_shopped_30_days==""] <- "A"
(data$other26_times_shopped_30_days)

write.csv(data, "clean2.csv")
