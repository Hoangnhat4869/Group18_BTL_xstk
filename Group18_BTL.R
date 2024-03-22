library("readr")
library("dplyr")
library("stringr")
library("ggplot2")
library("psych")
library("knitr")
library("nortest")
library("car")

CPUdata <- read.csv(".\\Intel_CPUs.csv", na.strings = c("", "N/A"))
CPUdata <- as.data.frame(data, stringsAsFactors = FALSE)
View(CPUdata)
CPUs_data = CPUdata[,c("Product_Collection","Vertical_Segment","Status","Launch_Date","Lithography","Recommended_Customer_Price","nb_of_Cores","nb_of_Threads","Processor_Base_Frequency","Cache","Instruction_Set","TDP","Max_Memory_Size","Max_nb_of_Memory_Channels","Max_Memory_Bandwidth")]
# Data cleaning
apply(is.na(CPUs_data), 2, sum)
test <- as.data.frame(CPUdata$Recommended_Customer_Price)
View(test)
