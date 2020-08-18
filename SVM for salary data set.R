### Support Vector Machine #### 

View(salarydata_test)
View(salarydata_train)

# Check  data set  which data type 

str(salarydata_test) 
str(salarydata_train)
 
salarydata_test$educationno =as.factor(salarydata_test$educationno)
salarydata_train$educationno=as.factor(salarydata_train$educationno)
 

### Building the  SVM model ###

install.packages("kernlab")
library(kernlab)
library(caret)
library(e1071)
library(plyr)
model=ksvm(salarydata_train$Salary~.,data=salarydata_train,kernel="vanilladot")
model

Salary_prediction <- predict(model, salarydata_test)

table(Salary_prediction,salarydata_test$Salary)

agreement <- Salary_prediction == salarydata_test$Salary
table(agreement)
prop.table(table(agreement))


### CONCLUSION : Hence vanilladot kernel type   Model gave 84.64% Accuracy 
 ###### salary 50k less than predicted 50k less than 10601 times and wrongly predicted means greater than 1554 times 
#################  50k Greater than predicted greater than 2146 times and wrongly means less than predicted 759 times 



# Different types of kernels 
# "rbfdot", "polydot", "tanhdot", "vanilladot", "laplacedot", 
# "besseldot", "anovadot", "splinedot", "matrix"

# kernel = rfdot 
model_rfdot<-ksvm(salarydata_train$Salary~., 
                  data= salarydata_train,kernel = "rfdot")
pred_rfdot<-predict(model_rfdot,newdata=salarydata_test)
mean(pred_rfdot==salarydata_test$Salary)

####  NOTE : Hence kernel type is rbfdot my Model gave 85.19 % Accuracy 

# kernel = polydot 
model_polydot<-ksvm(salarydata_train$Salary~., 
                  data= salarydata_train,kernel = "polydot")
pred_polydot<-predict(model_polydot,newdata=salarydata_test)
mean(pred_polydot==salarydata_test$Salary)

####  NOTE : Hence kernel type is polydot my Model gave 84.61 % Accuracy 


# kernel = tanhdot 
model_tanhdot<-ksvm(salarydata_train$Salary~., 
                    data= salarydata_train,kernel = "tanhdot")
pred_tanhdot<-predict(model_tanhdot,newdata=salarydata_test)
mean(pred_tanhdot==salarydata_test$Salary)

####  NOTE : Hence kernel type is polydot my Model gave 63.87 % Accuracy 


