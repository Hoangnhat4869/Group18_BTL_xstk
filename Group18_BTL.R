## Created on 2023-03-22
## Import some libraries to use
## Remember to install them first
library("readr")
library("dplyr")
library("stringr")
library("ggplot2")
library("psych")
library("knitr")
library("nortest")
library("car")


# Load the data
CPUdata <- read.csv(".\\Intel_CPUs.csv", na.strings = c("", "N/A"), header = TRUE)
CPUdata <- as.data.frame(CPUdata, stringsAsFactors = FALSE)
View(CPUdata)


# Data cleaning
