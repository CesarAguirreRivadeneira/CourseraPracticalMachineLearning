#Practical Machine Learning Project

#The goal of your project is to predict the manner in which they did the exercise. This is the "classe" variable in the training set. You may use any of the other variables to predict with. You should create a report describing how you built your model, how you used cross validation, what you think the expected out of sample error is, and why you made the choices you did. You will also use your prediction model to predict 20 different test cases.

#Predict classe variable usando cualquiera de las otras variables
#Crear un informe donde:
  #Describir el modelo   
  #Usar cross validacion
  #Expected out of sample error
  #20 diferent test cases
  #Please constrain the text of the writeup to < 2000 words 
  #number of figures to be less than 5
  

#Cross validacion es dividir los datos en dos conjuntos training/testing. Los 20 ejemplos son validation.
  #Random sampling/se trata de datos temporales...
#Data is in folder Data
getwd()
setwd("..")
library(caret)
setwd("./CourseraPracticalMachineLearning/Data")
csvtraining <- read.csv("pml-training.csv", na.strings = c("NA", "#DIV/0!", ""))
csvtesting <- read.csv("pml-testing.csv", na.strings = c("NA", "#DIV/0!", ""))
  #eliminar los datos donde la mayoria es 0.
  
  ColumnIndexNA <- colSums(is.na(csvtraining))/nrow(csvtraining) < 0.95
  TrainingDataNA <- csvtraining[,ColumnIndexNA]
  #eliminamos la primera columna y las de fecha, -c(1,3:7)
  FinalTraining <- TrainingDataNA[, -c(1,3:7)]
  dim(FinalTraining)
  #Convert the classe column to a factor variable
  FinalTraining$classe <- factor(FinalTraining$classe)
  #Apply the same cleaning on validation data
  ColumnNames <- names(FinalTraining)
  Testing <- csvtesting[,ColumnNames[1:53]]
  #Divide de dataset for cross validation
  set.seed(12345)
  TrainIndex <- createDataPartition(FinalTraining$classe, p=0.75,list = FALSE)
  Training <- FinalTraining[TrainIndex,]
  Validation <- FinalTraining[-TrainIndex,]
  
  #Modelo a probar
  #rf random forest, set the number of trees to increase performance
  set.seed(12345)
FitRF <- train(classe~.,method='rf',data=Training,ntree=10)
RFPrediction <- predict (FitRF,Validation)
  #validamos con confusion matrix o con lo que sea
  confusionMatrix(Validation$classe, RFPrediction)
  
  #Prediction on Testing data
  RFPrediction <- predict (FitRF,Testing)
  RFPrediction
  
