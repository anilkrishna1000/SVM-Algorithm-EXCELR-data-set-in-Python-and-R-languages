####### Support Vector Machine ######
View(forestfires)
# custom normalization function
normalise <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}

# apply normalization to required columns in  data frame
forestfires$temp <- normalise(forestfires$temp)
forestfires$rain <- normalise(forestfires$rain)
forestfires$RH <- normalise(forestfires$RH)
forestfires$wind <- normalise(forestfires$wind)

sum(forestfires$area<5) ## 366 
sum(forestfires$area>5) ## 151 

forestfires$size_category <- NULL

forestfires$size_category <- factor(ifelse(forestfires$area < 5, 0, 1),
                                    labels = c("small", "large"))

table(forestfires$size_category)

# create training and test data
forestfires_train <- forestfires[1:413, ]
forestfires_test <- forestfires[414:517, ]

View(forestfires_test)
##Training a model on the data ----
# begin by training a simple linear SVM
install.packages("kernlab")
library(kernlab)
forestfires_classifier <- ksvm(size_category ~ temp+rain+RH+wind, data = forestfires_train,
                          kernel = "vanilladot")
 

help(ksvm)

?ksvm
# basic information about the model
forestfires_classifier

## Evaluating model performance ----
# predictions on testing dataset
forestfires_predictions <- predict(forestfires_classifier, forestfires_test)
View(forestfires_test)

head(forestfires_predictions)

table(forestfires_predictions, forestfires_test$size_category)


agreement <- forestfires_predictions == forestfires_test$size_category
table(agreement)
prop.table(table(agreement))



## NOTE: My SVM model gave 67.30% Accuracy  
#### small  Predicted small as  70 times and small predicted large as 34 Times  



##  For Improving model performance  Tried Different Kernel methods ----
forestfires_classifier_rbf <- ksvm(size_category ~ temp+rain+RH+wind , data = forestfires_train, kernel = "laplacedot")
forestfires_predictions_rbf <- predict(forestfires_classifier_rbf, forestfires_test)

agreement_rbf <- forestfires_predictions_rbf == forestfires_test$size_category
table(agreement_rbf)
prop.table(table(agreement_rbf))
?ksvm


 ## CONCLUSION: Hence we Tried Different kernel Methods to our SVM Algorithm But we got The Same Accuracy i,e 67.30 only 

