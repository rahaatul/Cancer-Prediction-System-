library(corrplot)
library(caret)
library(dplyr)
library(class)
library(ggplot2)

setwd("~/datasets/cancer_dataset") 
data <- read.csv("~/datasets/cancer_dataset/cancer_prediction.csv")
print(data)

dim(data)
names(data)
str(data)
summary(data)

drops <- c('Patient.Id', 'index')
data <- data[ , !(names(data) %in% drops)]

str(data)

colSums(is.na(data))

data_copy <- data
str(data_copy)

level_drops <- c('Level') 
data_copy <- data[ , !(names(data) %in% level_drops)]
str(data_copy)

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) 
}
norm_data <- as.data.frame(lapply(data[,-24], normalize))
str(norm_data)
summary(norm_data)

set.seed(123) 
data.d <- sample(1:nrow(norm_data),size=nrow(norm_data)*0.8,replace = FALSE) 
train_data <- norm_data[data.d,]
test_data <- norm_data[-data.d,]
train_labels <- data[data.d,24]
test_labels <- data[-data.d,24]

opt_k <- round(sqrt(nrow(train_data)))
if((opt_k %% 2) == 0) {
  opt_k<-opt_k+1
  cancer_prediction <- knn(train = train_data, test = test_data, cl = train_labels, k=opt_k)
} else {
  cancer_prediction <- knn(train = train_data, test = test_data, cl = train_labels, k=opt_k)
}

prediction_result <- as.data.frame(cancer_prediction)
prediction_result

test_data_with_labels <- merge(test_data, prediction_result)
test_data_with_labels

table(factor(cancer_prediction))
table(test_labels, cancer_prediction)
confusionMatrix(table(cancer_prediction, test_labels))

corrplot(cor(norm_data),   
         method = "shade", 
         type = "full",    
         diag = TRUE,      
         tl.col = "black", 
         bg = "white",     
         title = "",       
         col = NULL)       

