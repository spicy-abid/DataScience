mainData<- read.csv("C:/Users/88013/Desktop/DataScienceFinal-G-14/dengueData.csv", header=TRUE, sep=",")
mainData

str(mainData)


missingValFun <- c("", "NA", "N/A", "NULL", "?")
missingValues <- sapply(mainData, function(col) sum(col %in% missingValFun))
missingValues

library(dplyr)

numericData <- mainData %>% mutate_all(~as.numeric(factor(.)))
numericData







checkCor <- numericData[, sapply(numericData, sd) != 0]
FinalCor <- round(cor(checkCor), digits = 2)
FinalCor

install.packages("corrplot") 
library(corrplot) 
corrplot(FinalCor, method = "color", type = "upper")



modelcor <- chisq.test(numericData, simulate.p.value = TRUE)
modelcor


FinalData <- numericData[, -which(names(numericData) == "District")]
FinalData

FinalData2<- FinalData[, -which(names(FinalData) == "HouseType")]
FinalData2




  install.packages("e1071")

library(e1071)

naive_bayes_model <- naiveBayes(Outcome ~ ., data = FinalData2)
naive_bayes_model



install.packages("caret")
library(caret)

trainIndices <- createDataPartition(FinalData2$Outcome, p = 0.7, list = FALSE)
trainData <- FinalData2[trainIndices, ]
testData <- FinalData2[-trainIndices, ]
testData


naive_bayes_model <- naiveBayes(Outcome ~ ., data = trainData)


predictions <- predict(naive_bayes_model, newdata = testData)


accuracy <- sum(predictions == testData$Outcome) / nrow(testData)


Accuracy1= round(accuracy * 100, 2)

library(naivebayes)
ctrl <- trainControl(method = "cv", number = 10, savePredictions = TRUE)
cv_model <- train(Outcome ~ ., data = FinalData2, method = "naive_bayes",
                  trControl = ctrl)
cv_model
cv_accuracy <- cv_model$results$Accuracy
Accuracy2<- round(mean(cv_accuracy) * 100, 2)


cv_model

cv_accuracy <- cv_model$results$Accuracy
Accuracy2<- round(mean(cv_accuracy) * 100, 2)


predictions <- predict(naive_bayes_model, newdata = FinalData2)
conf_matrix <- confusionMatrix(predictions, FinalData2$Outcome)

recall <- conf_matrix$byClass["Sensitivity"]
precision <- conf_matrix$byClass["Pos Pred Value"]
f_measure <- 2 * (precision * recall) / (precision + recall)

Recall<-round(recall, 2)
Precision<- round(precision, 2)
F_measure<- round(f_measure, 2)
