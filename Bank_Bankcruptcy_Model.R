

rm(list = ls(all.names = all()))

setwd("E:\\Insofe_Materials\\CUTE_Dec9th\\Data")

bnk_data <- read.csv("bankdata.csv", header = T, sep = ",")
summary(bnk_data)
str(bnk_data)

# Get the NA's

pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(bnk_data,2,pMiss)

# dropping off Attr21,Attr27,Attr37,Attr60 for NA''s
drops <- c("Attr21","Attr27","Attr37","Attr60")
bnk_data <- bnk_data[ , !(names(bnk_data) %in% drops)]
colnames(bnk_data)

library(mice)
md.pattern(bnk_data)

methods(mice)

tempData <- mice(bnk_data,m=5,maxit=50,meth='pmm',seed=500)
summary(tempData)

new_bnk_data <- complete(tempData)

# Apply Caping if needed or after checking the summary

# Check VIF and AIC vals

# Build Model

# Accuracy 

