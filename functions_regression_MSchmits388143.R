############## Regression ###########

#### knn regression ####
#Create partition, apply and tuned regression model
run_reg_model_tuning <- function(AllData, perc_pred, times, bacteria, 
                                    method, tuneGrid, preProcess){
  RMSE.val.preProcess <- 0
  RMSE.val <- 0
  
  if (bacteria == "TVC"){
    predictor = AllData$TVC
    formula = TVC~.
    }
  
  else if (bacteria == "Pseudomonads"){
    predictor = AllData$Pseudomonads
    formula = Pseudomonads~.
  }
  
  else {print("False bacteria name")}
  
  #Create a partition of the data
  part <- partition_data(AllData, predictor, perc_pred, times)
  
  #Train the model with preProcessing
  model.fit.preProcess <- caret::train(formula, method = method, data = part$trainSet,
                            tuneGrid = tuneGrid, metric = "RMSE", 
                            preProcess = preProcess)
  
  #Train the model without preProcessing
  model.fit <- caret::train(formula, method = method, data = part$trainSet,
                                       tuneGrid = tuneGrid, metric = "RMSE")
  
  #RMSE for k nearest neighbours regression with and without preProcessing
  predict.model.preProcess <- predict(model.fit.preProcess, part$testSet)
  predict.model <- predict(model.fit, part$testSet)
  
  if (bacteria == "TVC"){
    #Calculate RMSE values for TVC
    RMSE.val.preProcess <- RMSE(predict.model.preProcess, part$testSet$TVC)
    RMSE.val <- RMSE(predict.model, part$testSet$TVC)
  }
  else if (bacteria == "Pseudomonads"){
    #Calculate RMSE values for Pseudomonads
    RMSE.val.preProcess <- RMSE(predict.model.preProcess, part$testSet$Pseudomonads)
    RMSE.val <- RMSE(predict.model, part$testSet$Pseudomonads)
  }
  
  if (RMSE.val.preProcess > RMSE.val){
    prePro = TRUE
    model.fit = model.fit.preProcess
    predict.model = predict.model.preProcess
    RMSE.val = RMSE.val.preProcess
  }
  else{
    prePro = FALSE
  }
  
  return(list(model.fit = model.fit, predict.model = predict.model,
              RMSE.val = RMSE.val, preProcess = prePro))
}




#Create partition, apply model, calculate and plot RMSE values
run_reg_model_iteration <- function(AllData, predictor, perc_pred, times, 
                      formula, method, bacteria, tuneGrid, preProcess){
  #Create a partition of the data
  part <- partition_data(AllData, predictor, perc_pred, times)
  
  #Train the model
  model.fit <- caret::train(formula, method = method, data = part$trainSet,
                            tuneGrid = tuneGrid, metric = "RMSE", 
                            preProcess = preProcess)
  
  #RMSE for k nearest neighbours regression
  predict.model <- predict(model.fit, part$testSet)
  
  if (bacteria == "TVC"){
    #Calculate RMSE values for TVC
    RMSE.val <- RMSE(predict.model, part$testSet$TVC)
  }
  else{
    #Calculate RMSE values for Pseudomonads
    RMSE.val <- RMSE(predict.model, part$testSet$Pseudomonads)
  }
  
  #Plot RMSE
  plt_RMSE <- plot(model.fit)
  
  return(list(model.fit = model.fit, predict.model = predict.model,
              RMSE.val = RMSE.val, plt_RMSE = plt_RMSE))
}