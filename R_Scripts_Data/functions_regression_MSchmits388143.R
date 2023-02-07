############## Regression ###########

#Tune regression model with train (caret package)
#Create partition, apply and tuned regression model
#Arguments:
  ##AllData: Data set
  ## perc_pred: Percentage of values in train set
  ##times: Number of iterations
  ##bacteria: Type of bacteria (TVC or Pseudomonads)
  ##Method: method for the regression
  ##tuneGrid: Hyperparameters for tuning with grid search
  ##preProcess: Pre-processing of data (ex: scaling, centering)
#Returns:
  ##model.fit: Fitted model
  ##predict.model: Predictive model
  ##RMSE.val: RMSE value for best tuned model
  ##preProcess: (TRUE or FALSE) Pre-processing or not the data before training
  ##plt_RMSE: Plot of RMSE values
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
  if (RMSE.val.preProcess < RMSE.val){
    prePro <- TRUE
    model.fit <- model.fit.preProcess
    predict.model <- predict.model.preProcess
    RMSE.val <- RMSE.val.preProcess
  }
  else{
    prePro <- FALSE
  }
  #Plot RMSE
  plt_RMSE <- plot(model.fit)
  return(list(model.fit = model.fit, 
              predict.model = predict.model,
              RMSE.val = RMSE.val, 
              preProcess = prePro, 
              plt_RMSE = plt_RMSE))
}


# Run selected regression for 100 iterations
#Create partition, apply model, calculate and plot RMSE values
#Arguments:
  ##perc_pred: Percentage of training data
  ##times: Number of iterations
  ##method: Method of training
  ##bacteria: Type of bacteria (TVC or Pseudomonads)
  ##tuneGrid: Grid with hyperparameters for tuning
  ##preProcess: (TRUE or FALSE) should the data be scaled and centered before training
#Returns list:
  ##list_RMSE: List of RMSE values 
  ##mean_RMSE: Mean of RMSE for all iterations 
  ##model.fit: Fitted model
  ##predict.model: Predicted data 
  ##observed: Observed data
  ##sd_RMSE: Standard error of RMSE
  ##CI_RMSE: 95% CI of RMSE
run_reg_model_iteration <- function(AllData, 
                                    perc_pred, 
                                    times, 
                                    method, 
                                    bacteria, 
                                    tuneGrid, 
                                    preProcess){
  #Initialisation
  list_RMSE <- c()
  mean_RMSE <- 0
  sd_RMSE <- 0
  CI_RMSE <- 0
  #Attribute formula and predictor acording to bacteria
  if (bacteria == "TVC"){
    predictor = AllData$TVC
    formula = TVC~.
  }
  else if (bacteria == "Pseudomonads"){
    predictor = AllData$Pseudomonads
    formula = Pseudomonads~.
  }
  else {print("False bacteria name")}
  #Create index of partition
  set.seed(8)
  trainIndex <- createDataPartition(predictor, p = perc_pred,
                                    list = FALSE,
                                    times = times)
  for (i in 1:times){
    #Partition data into train and test datasets
    trainSet <- AllData[trainIndex[,i],]
    testSet <- AllData[-trainIndex[,i],]
    trainCl <- trainSet[,ncol(trainSet)]
    testCl <- testSet[,ncol(testSet)]
    #Train the model
    model.fit <- caret::train(formula, 
                              method = method, 
                              data = trainSet,
                              tuneGrid = tuneGrid, 
                              metric = "RMSE", 
                              preProcess = preProcess)
    #Prediction model
    predict.model <- predict(model.fit, testSet)
    #Calculate RMSE, depending on bacteria type
    if (bacteria == "TVC"){
      observed = testSet$TVC
      #Calculate RMSE values for TVC
      RMSE.val <- RMSE(predict.model, testSet$TVC)
    }
    else if (bacteria == "Pseudomonads"){
      observed = testSet$Pseudomonads
      #Calculate RMSE values for Pseudomonads
      RMSE.val <- RMSE(predict.model, testSet$Pseudomonads)
    }
    #Add RMSE to list
    list_RMSE <- append(list_RMSE, RMSE.val)
  }
  #Calculate RMSE mean
  mean_RMSE = mean(list_RMSE[1:length(list_RMSE)])
  #Calculate RMSE sd
  sd_RMSE = sd(list_RMSE)
  #Calculate RMSE 95% CI
  CI_RMSE = CI(list_RMSE, 0.95)
  return(list(list_RMSE = list_RMSE, 
              mean_RMSE = mean_RMSE, 
              model.fit = model.fit, 
              predict.model = predict.model, 
              observed = observed,
              sd_RMSE = sd_RMSE,
              CI_RMSE = CI_RMSE))
}


#Plot of predicted against observed
#Arguments:
  ##observed: Observed data
  ##predicted: Predicted data
  ##title: Title of plot
#Returns:
  ##plt: Plot of observed against predicted values
plot_obs_pred <- function(observed, 
                          predicted, 
                          title){
  df_obs_pred <- data.frame(observed, predicted)
  plt <- ggplot(df_obs_pred, aes(x = predicted, y = observed))+
    geom_point(color = "steelblue")+
    geom_smooth(method = "lm", color = "steelblue")+ #Fitted line x=y
    labs(title = title,
         x = "Predicted values", y = "Observed values")+
    #Line at -1log difference from actual values
    geom_smooth(method = "lm", color = "salmon", aes(y = observed - 1), se = FALSE)+
    #Line at +1log difference from actual values
    geom_smooth(method = "lm", color = "salmon", aes(y = observed + 1), se = FALSE)
  #Save plot
  ggsave(file= paste("Plots/", title, ".png"))
  return(plt)
}