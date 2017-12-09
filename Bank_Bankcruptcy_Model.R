
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

library(DMwR)

bnk_data<-centralImputation(bnk_data)

apply(bnk_data,2,pMiss)
summary(bnk_data)

# library(mice)
# md.pattern(bnk_data)
# 
# methods(mice)
# 
# tempData <- mice(bnk_data,m=5,maxit=50,meth='pmm',seed=500)
# summary(tempData)
# 
# new_bnk_data <- complete(tempData)
# 
# 
# library(missForest)
# imputationResults <- missForest(bnk_data)
# dataMissForestImputed <- imputationResults$ximp

# Apply Caping if needed or after checking the summary
f_apply_cap <- function(col_name)
{
  x = col_name
  qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
  caps <- quantile(x, probs=c(.05, .95), na.rm = T)
  H <- 1.5 * IQR(x, na.rm = T)
  x[x < (qnt[1] - H)] <- caps[1]
  x[x > (qnt[2] + H)] <- caps[2]
  return <- x
# data$IV<-x
}

summary(bnk_data)
colnames(bnk_data)
temp_data <- subset(bnk_data, select = c(1:60))
str(temp_data)

temp_data <- data.frame(apply(temp_data,2,f_apply_cap))

str(temp_data)
summary(temp_data)

temp_data <- cbind(temp_data,bnk_data$target)
str(temp_data)

names(temp_data)[names(temp_data) == 'bnk_data$target'] <- 'target'
str(temp_data)

bnk_data_bkp <- bnk_data
bnk_data <- temp_data
str(bnk_data)
rm (temp_data)

library(caret)
# createDataPartition(y, times = 1, p = 0.75, list = FALSE)

library(caTools)
set.seed(12345)

sample = sample.split(bnk_data, SplitRatio = .70)

temp  = subset(bnk_data, sample == FALSE)

sample1 = sample.split(temp, SplitRatio = .70)
test = subset(temp, sample1 == TRUE)
valid =  subset(temp, sample1 == F)
train = subset(bnk_data, sample == TRUE)
class(valid)
View(valid)

# Check VIF and AIC vals

# Build Model
library(randomForest)
bnk_rf<- randomForest(target ~ ., data=train, keep.forest=TRUE, ntree=50)


print(bnk_rf)
bnk_rf$predicted
bnk_rf$importance

# plot (directly prints the important attributes)
varImpPlot(bnk_rf)

valid$predicted <- predict(bnk_rf,valid)
test$predicted <- predict(bnk_rf,test)

library(e1071)
library(caret)
# Accuracy 
confusionMatrix(data=valid$predicted,
                reference=valid$target)

# Validation Results
# Confusion Matrix and Statistics
# 
# Reference
# Prediction   No  Yes
# No  3952  189
# Yes   19   12
# 
# Accuracy : 0.9501          
# 95% CI : (0.9431, 0.9566)
# No Information Rate : 0.9518          
# P-Value [Acc > NIR] : 0.7088          
# 
# Kappa : 0.0918          
# Mcnemar's Test P-Value : <2e-16          
#                                           
#             Sensitivity : 0.9952          
#             Specificity : 0.0597          
#          Pos Pred Value : 0.9544          
#          Neg Pred Value : 0.3871          
#              Prevalence : 0.9518          
#          Detection Rate : 0.9473          
#    Detection Prevalence : 0.9926          
#       Balanced Accuracy : 0.5275          
#                                           
#        'Positive' Class : No  



confusionMatrix(data=test$predicted,
                reference=test$target)
# Test Data
# Confusion Matrix and Statistics
# 
# Reference
# Prediction   No  Yes
# No  8737  437
# Yes   33   15
# 
# Accuracy : 0.949           
# 95% CI : (0.9443, 0.9534)
# No Information Rate : 0.951           
# P-Value [Acc > NIR] : 0.8143          
# 
# Kappa : 0.0511          
# Mcnemar's Test P-Value : <2e-16          
# 
# Sensitivity : 0.99624         
# Specificity : 0.03319         
# Pos Pred Value : 0.95237         
# Neg Pred Value : 0.31250         
# Prevalence : 0.95099         
# Detection Rate : 0.94741         
# Detection Prevalence : 0.99480         
# Balanced Accuracy : 0.51471         
# 
# 'Positive' Class : No              
