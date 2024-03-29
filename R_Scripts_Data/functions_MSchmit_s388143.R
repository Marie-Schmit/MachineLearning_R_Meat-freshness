#Merging imported data
#Arguments:
  ##Predictor: predictor of the class
  ##Predict: predicted class
  ##AllData: data frame containing the imported data
#Returns: data frame of merged predictor and predict

merge_data <- function(predictor, 
                       predict, 
                       AllData){
  # Combine all rows from enose and sensory
  merged <- merge(predictor, predict, by = "row.names")
  rownames(merged) = merged[,1]
  # Remove raw names from merged column
  AllData <- as.data.frame(merged[-1])
  return(AllData)
}


#Plot pca
#Arguments
  ##pca.AllData: pca model of data
  ##AllData: Data fram containing data of interest (imported and merged data)
  ##ncomp: Number of components for pca
  ##sensory: Vector of classes
  ##style: Style of pca plot
#Returns: Pca model of data

pca_visualisation <- function(pca.AllData, 
                              AllData, 
                              ncomp, 
                              sensory, 
                              style){
  #Apply PCA
  pca.AllData <- pca(AllData, ncomp = ncomp, scale = TRUE)
  #Save plot
  main <- deparse(substitute(AllData)) # The plot takes AllData name
  png(file= paste("Plots/", main, style, ".png"))
  #PCA scatter plot
  plotIndiv(pca.AllData, ind.names = samples, group = sensory, style = style, title = main)
  graphics.off()
  plotIndiv(pca.AllData, ind.names = samples, group = sensory, style = style, title = main)
  return(pca.AllData)
}


#Create biplot of pca model
##Arguments
  ##pca.AllData: Pca model of all data
  ##sensory: Vector of classes
  ##AllData: Dataframe containing data of interest
##Returns: Biplot of pca model

pca_var <- function(pca.AllData, 
                    sensory, 
                    AllData){
  var_PC <- pca.AllData$prop_expl_var$X
  #Save plot
  main <- deparse(substitute(AllData)) #Name the plot acording to AllData
  biplt <- biplot(pca.AllData, xlab = paste("PC1", var_PC[1], "%"),
                  ylab = paste("PC2", var_PC[2], "%"), group = sensory,
                  col.per.group = c("orange", "green", "gray"))
  #Save plot
  ggsave(filename = paste("Plots/biplot_", main, ".png"), plot = biplt)
  return(biplt)
}


#Plot hca heatmap and dendograms
#Arguments:
  ##AllData: Data of interest
#Returns: heatmap

hca_visualisation <- function(AllData){
  # Distance matrix of Data without sensory column
  All <- AllData[order(AllData$sensory),]
  sampleDist <- dist(All)
  sampleDistMatrix <- as.matrix(sampleDist)
  #Create color palette
  colors <- colorRampPalette(rev(brewer.pal(n = 7, name =
                                              "RdYlBu")))(250)
  main <- deparse(substitute(AllData)) #Name the plot acording to AllData
  #Save plot
  png(file = paste("Plots/heatmap_", main, ".png"))
  # Heatmap construction
  heatmap <- pheatmap(sampleDistMatrix,
           clustering_distance_rows = sampleDist,
           clustering_distance_cols = sampleDist,
           col = colors, main = paste("HCA analysis", main),
           labels_col = All$sensory
  )
  graphics.off()
  return(heatmap)
}

#Save temperature and time (according to sample ID) in dataframe
#Arguments:
  ##AllData: Data of interest
#Returns: AllData, dataframe of data of interest, with a column time and a column temperature
temp_time <- function(AllData){
  #ID equal to list of temperature time
  ID <- strsplit(row.names(AllData), "F")
  Temperature <- c()
  Time <- c()
  for (i in 1:length(ID)){
    Temperature <- append(Temperature, ID[[i]][1])
    Time <- append(Time, ID[[i]][2])
  }
  AllData <- data.frame(AllData, Temperature, Time)
  return(AllData)
}


#Function to partition dataset into train and test values
#Arguments:
  ##AllData: dataframe containing the data of interest
  ##Predict: Class or counts values
  ##Times: number of iteration ofr the partition
#Returns: List of dataset and vectors train and set

partition_data <- function(AllData, predict, perc_predict, times){  
  set.seed(8)
  #Preserve correspondance between numerical and categorical data (predictor and response respectively)
  trainIndex <- createDataPartition(predict, p = perc_predict,
                                    list = FALSE,
                                    times = times)

  for (i in 1:times){
    trainSet <- AllData[trainIndex[,i],]
    testSet <- AllData[-trainIndex[,i],]
    trainCl <- trainSet[,ncol(trainSet)]
    testCl <- testSet[,ncol(testSet)]
  return(list(trainSet = trainSet,
              testSet = testSet,
              trainCl = trainCl,
              testCl = testCl))
  }
}


############################# Classification #########################
######### knn, svm #########

#Model training and optimisation: find the best scale and k parameter
#Arguments:
  ##trainSet: Training set after partition
  ##testSet: Test set after partition
  ##trainCl: Training set factors
  ##testCl: Test set factors
  #scalingMethod: Scaling method (for instance: "knn")
  #n: Number of k to test
#Returns: List of best accuracy, best scaling method, best k, list of accuracies
  #list of misclassifications

knn.optimisation <- function(trainSet, 
                             testSet, 
                             trainCl, 
                             testCl, 
                             n, 
                             scalingMethod){
  #Remove the class variable of the train and test sets
  trainSet.knn <- trainSet[, -ncol(trainSet)]
  testSet.knn <- testSet[, -ncol(testSet)]
  #Variable initialisation
  k.accuracies <- c()
  bestAccuracy <- 0
  currentScale <- c()
  currentk <- 0
  bestk <- 0
  # Find best k without scaling
  # test of k values from 1 to 20
  for (currentk in 1:n) {
    model.k<-knn(trainSet.knn, testSet.knn, trainCl, currentk)
    confusion.matrix <- confusionMatrix(model.k, testCl, positive="1")
    modelAccuracy <- confusion.matrix$overall[1] #Accuracy
    #List of all accuracies to plot the accuracy values against k values
    k.accuracies <- c(k.accuracies, modelAccuracy)
    if(modelAccuracy > bestAccuracy){
      bestAccuracy <- modelAccuracy
      bestScale <- currentScale
      bestk <- currentk
      print(bestk)
      print(bestAccuracy)
      bestModel <- model.k
    }
  }
  #Test different scales to find the best
  for (currentScale in scalingMethod){
    #Test different k from 1 to n to find the best
    for (currentk in seq(1, n, 1)){
      #Define scaling method for three types of scaling
      preProcValues <- preProcess(trainSet.knn, method = currentScale)
      #Apply scaling
      trainTransformed <- predict(preProcValues, trainSet.knn)
      testTransformed <- predict(preProcValues, testSet.knn)
      #Create model with scaled data
      model.k <- knn(trainTransformed, testTransformed, trainCl, currentk)
      #Evaluate accuracy of the model
      #1 is the positive value, when the meat is fresh
      confusion.matrix <- confusionMatrix(model.k, testCl, positive="1")
      modelAccuracy <- confusion.matrix$overall[1] #Accuracy
      #List of accuracies to plot the accuracy values against k values
      k.accuracies <- c(k.accuracies, modelAccuracy)
      # If the previous models are outperformed, current accuracy is the new best accuracy
      if(modelAccuracy > bestAccuracy){
        bestAccuracy <- modelAccuracy
        bestScale <- currentScale
        bestk <- currentk
        bestModel <- model.k
      }
    }
  }
  return(list(bestAccuracy = bestAccuracy,
              bestScale = bestScale,
              bestk = bestk,
              bestModel = bestModel,
              k.accuracies = k.accuracies))
}

#Calculate model and cross table for knn
#Returns:
  ##trainSet: Training set after partition
  ##trainSet: Training set after partition
  ##trainCl: Training set of factors
  ##testCl: Training set of factors
#Returns:Knn model and cross table display
cross_table_knn <- function(trainSet, 
                            testSet, 
                            trainCl, 
                            testCl, 
                            k){
  #Remove the class variable of the train and test sets
  trainSet.knn <- trainSet[, -ncol(trainSet)]
  testSet.knn <- testSet[, -ncol(testSet)]
  #Create model with scaled data
  model.k <- knn(trainSet.knn, testSet.knn, trainCl, k)
  #Cross table
  cross.table <- CrossTable(testCl, model.k, prop.chisq=FALSE, 
                            prop.t=FALSE, prop.c=FALSE, prop.r=FALSE)
  return(list(cross.table = cross.table, 
              model.k = model.k))
}


#Create a partition with iterations
#For each iteration, run the model and store the accuracy
#Calculate the crosstable and number of missclassified elements for each iteration
#Arguments:
  ##AllData: Data of interest
  ##predict: Classes values
  ##perc_predict: Percentage of training values for partition
  ##times: Number of iterations
  ##operation: Function to apply to train and test datasets
#Returns: List of
  ##trainSet: Train data set
  ##testSet: Test dataset
  ##trainCl: Training factors dataset
  ##testCl: Testing factors dataset
  ##accuracies: List of accuracies for each iteration
  ##model: Trained model
  ##misclas: Vector of misclassification, one per class

model.run <- function(AllData, 
                      predict, 
                      perc_predict, 
                      times, 
                      operation){  
  set.seed(8)
  #Preserve correspondance between numerical and categorical data (predictor and response respectively)
  trainIndex <- createDataPartition(predict, p = perc_predict,
                                    list = FALSE,
                                    times = times)
  #List of accuracies init
  accuracies <- c()
  #Initialise vevctors sums of number and proportion of misclassifications
  mis_nb_sum <- c(0,0,0)
  mis_prop_sum <- c(0,0,0)
  #For each partition
  for (i in 1:times){
    #Split data into train and test sets
    trainSet <- AllData[trainIndex[,i],]
    testSet <- AllData[-trainIndex[,i],]
    trainCl <- trainSet[,ncol(trainSet)]
    testCl <- testSet[,ncol(testSet)]
    #Train the model
    model = operation(trainSet, trainCl, testSet, testCl)
    #Calculate cumulative natrix
    confusion.matrix <- confusionMatrix(model, testCl, positive="1")
    #Calculate model accuracy
    modelAccuracy <- confusion.matrix$overall[1]
    #Store accuracy in list
    accuracies<-c(accuracies, modelAccuracy)
    #Calculate cross table
    cross.table <- CrossTable(testCl, model, prop.chisq=FALSE, prop.t=FALSE, 
                              prop.c=FALSE, prop.r=FALSE)
    #Calculate missclassifications
    misclas <- misclassification(cross.table, mis_nb_sum, mis_prop_sum)
    mis_nb_sum <- misclas$mis_nb_sum
    mis_prop_sum <- misclas$mis_prop_sum
  }
  
  return(list(trainSet = trainSet, 
              testSet = testSet,
              trainCl = trainCl,
              testCl = testCl,
              accuracies = accuracies,
              model = model,
              misclas = misclas))
}

#Cumulative means accuracy calculation
#Arguments:
  ##accuracies: list of accuracies
#Returns:
  ##cumulative.means: list of cumulated means accuracies
cumulative.mean.accuracy <- function(accuracies){
  cumulative.means <- c()
  for(i in 1:length(accuracies)) {
    cumulative.means <- c(cumulative.means, mean(accuracies[1:i]))
  }
  return(cumulative.means)
}


############ SVM - rd #########
#Tuning of SVM to find the best kernels
#Arguments:
  ##trainSet, testSet, trainCl, testCl: Train and test datasets after partition
  ##kerbel: List of hyperparameters kernel to optimise
#Returns:
  ##bestAccuracy: Accuracy of the optimised model,
  ##bestKernel: Kernel of the optimised model,
  ##bestModel: Model of the optimised model,
  ##bestPrediction: Prediction of the optimised model,
  ##bestCrossTable: Cross table of the optimised model,
  ##bestConfusionMatrix: Confusion matrix of the optimised model

svm.optimisation <- function(trainSet, 
                             testSet, 
                             trainCl, 
                             testCl, 
                             kernel){
  #Variable initialisation
  bestAccuracy <- 0
  currentAccuracy <- 0
  bestKernel <- c()
  bestModel <- c()
  bestConfusionMatrix <- c()
  bestCrossTable <- c()
  bestPrediction <- c()
  for (kernel_type in kernel){
    model_svm <- ksvm(sensory ~ ., data=trainSet, kernel=kernel_type, C=1)
    kernel.predicted <- predict(model_svm, testSet, type="response")
    #Accuracy calculation
    kernel.confusion.matrix <- confusionMatrix(kernel.predicted, testCl, positive="1")
    modelAccuracy <- kernel.confusion.matrix$overall[1]
    #If current accuracy is the best, update best accuracy
    if(modelAccuracy > bestAccuracy){
      bestAccuracy <- modelAccuracy
      bestKernel <- kernel_type
      bestModel <- model_svm
      bestPrediction <- kernel.predicted
      bestConfusionMatrix <- kernel.confusion.matrix
      #Calculate cross table
      bestCrossTable <- CrossTable(testCl, kernel.predicted,prop.chisq=FALSE, 
                                   prop.t=FALSE, prop.c=FALSE, prop.r=FALSE)
    }
  }
  return(list(bestAccuracy = bestAccuracy,
    bestKernel = bestKernel,
    bestModel = bestModel,
    bestPrediction = bestPrediction,
    bestCrossTable = bestCrossTable,
    bestConfusionMatrix = bestConfusionMatrix))
}


########### Random forest ########
#Define a class and a learner
#Arguments:
  ##AllData: Data of interest
#Return list:
  ##rf_task: Task defined for data of interest, for class sensory
  ##learner: Classifier random forest
  ##plt: Plot of classes frequency
rf_class_learner <- function(AllData){
  #Define a task
  rf_task = as_task_classif(sensory~., data=AllData)
  rf_task$data()
  main <- deparse(substitute(AllData)) #Name the plot acording to AllData
  #Inspect the frequency of each class 
  plt <- autoplot(rf_task)+
    ggtitle(paste("Random forest, frequency of class", main))
  #Save the frequency plot
  ggsave(file= paste("Plots/RandomForest_", main, ".png"))
  #Set the learner for classify random forest
  learner = lrn("classif.randomForest")
  return(list(rf_task = rf_task, 
              learner = learner, 
              plt = plt))
}


#Model tuning for random forest
#Arguments:
  ##ntree_min, ntree_max: Minimal and maximal numbers of trees in the forest
  ##mtry_min, mtry_max: Minimal and maximal numbers of features to sample at each node
  ##nodesize_min, nodesize_max: Minimal and maximal number of cases authorised in a leaf
  ##maxnodes_min, maxnodes_max: Minimal and maximal numbers of nodes
  ##nb_evaluation: Budget allocation for tuning
  ##part_ratio: Ratio of training data
  ##task_rf: Task 
  ##AllData: Data of interest
#Return list:
  ##instance: Object of function that estimates performance of parameters
  ##learner_tun: Tuned learner 
  ##split: Partition of data
  ##task_train: Training of model with data AllData, and sensory
  
rf_tuning <- function(ntree_min, ntree_max, 
                      mtry_min, mtry_max,
                      nodesize_min, nodesize_max, 
                      maxnodes_min, maxnodes_max, 
                      nb_evaluation,
                      part_ratio, 
                      task_rf, 
                      AllData
                      ){
  #Tune the learner, for classifier random forest, with hyperparameters of interest
  learner_tun = lrn("classif.randomForest",
                    ntree = to_tune(ntree_min, ntree_max),
                    mtry = to_tune(mtry_min, mtry_max),
                    nodesize = to_tune(nodesize_min, nodesize_max),
                    maxnodes = to_tune(maxnodes_min, maxnodes_max)
  )
  #Resampling method
  resampling = rsmp("cv", folds = 3)
  #Performance measure
  measure = msr("classif.acc")
  
  #Budget allocation for tuning
  terminator = trm("evals", n_evals = nb_evaluation)
  #Partition data
  set.seed(8)
  split = partition(task_rf, ratio = part_ratio)
  #Define new train task
  train_Set <- AllData[split$train,]
  task_train <- as_task_classif(sensory~., data = train_Set)
  #Construct tunning instance single criterion 
  #(store objective function that estimate performance of hyperparameters)
  instance = ti(task = task_train,
                learner = learner_tun,
                resampling = resampling,
                measures = measure,
                terminator = terminator
           )
  print(instance)
  #Perform tuning with grid search
  tuner = tnr("grid_search", resolution = 5, batch_size = 4)
  #Initiate tuning process
  tuner$optimize(instance)
  return(list(instance = instance,
         learner_tun = learner_tun, 
         split = split,
         task_train = task_train))
  
}

#Build final model
#Arguments:
  ##learner_tun: Tuned learner
  ##instance: Object of function that estimates performance of parameters
  ##task_train: Trained task
#Returns:
  ##learner_tun: tuned learner
built_model <- function(learner_tun, 
                        instance, 
                        task_train){
  learner_tun$param_set$values = instance$result_learner_param_vals
  learner_tun$train(task_train)
  learner_tun$model
  return(learner_tun = learner_tun)
}

#Test model performances
#Arguments:
  ##task_rf: Task to test
  ##learner
  ##part-ratio: Percentage of training data
  ##split: Partition of data
  ##title: Title of frequency plot
#Returns:
  ##accuracy: Accuracy of prediction
  ##conf_matrix: Confusion matrix of prediction
  ##plt: Frequency plot
rf_test <- function(task_rf, 
                    learner, 
                    part_ratio, 
                    split, 
                    title){
  prediction = learner$predict(task_rf, split$test)
  #Classification accuracy
  measure = msr("classif.acc")
  accuracy <- prediction$score(measure)
  #Confusion matrix
  conf_matr <- prediction$confusion
  #Inspect the frequency of each class 
  plt <- autoplot(prediction)+
    ggtitle(paste("Frequency plot random forest", title))
  #Save frequency plot
  ggsave(file= paste("Plots/Test_RandomForest_", title, ".png"))
  return(list(accuracy = accuracy, 
              conf_matr = conf_matr, 
              plt = plt))
}


#Train and test random forest
#Arguments:
  #task: Task to train and test
  ##times: Number of iterations
  ##learner: Learner
  ##ratio: Percentage of data in train set
#Returns list:
  ##accuracies: List of accuracies for every iteration
  ##Misclas: List of misclassifications, for each iteration
run_rf <- function(task, times, learner, ratio){
  accuracies <- c()
  #List of accuracies init
  accuracies <- c()
  #Initialise vectors sums of number and proportions of misclassifications
  mis_nb_sum <- c(0,0,0)
  mis_prop_sum <- c(0,0,0)
  set.seed(8)
  #For each iteration
  for (i in 1:times){
    split = partition(task, ratio = ratio)
    #Model training
    learner$train(task, split$train)
    prediction = learner$predict(task, split$test)
    #Classification accuracy
    measure = msr("classif.acc")
    accuracy <- prediction$score(measure)
    accuracies <- append(accuracies, accuracy)
    #Calculate confusion matrix
    conf_matr <- prediction$confusion
    #Calculate missclassifications
    misclas <- misclassification_confMatrix(conf_matr, mis_nb_sum, mis_prop_sum)
    mis_nb_sum <- misclas$mis_nb_sum
    mis_prop_sum <- misclas$mis_prop_sum
  }
  return(list(accuracies = accuracies, 
              misclas = misclas))
}


######## Question1 #########
#Create and save plots of cumulative accuracy means 
#for each analytical platform and classification method
#Arguments:
  ##cum_mean_knn: Cumulative mean accuracy for knn
  ##cum_mean_rf: Cumulative mean accuracy for rf
  ##cum_mean_svm: Cumulative mean accuracy for svm
  ##dataset: Analytical platform
cumulative_plot <-  function(cum_mean_knn, 
                             cum_mean_rf, 
                             cum_mean_svm, 
                             dataset){
  #Data frame containing all cumulative means
  accuracy_cumulative_means <- data.frame(cum_mean_knn,
                                          cum_mean_rf,
                                          cum_mean_svm)
  accuracy_cumulative_means$id = 1:nrow(accuracy_cumulative_means)
  df_cum_means <- melt(accuracy_cumulative_means, id = "id")
  names(df_cum_means) <- c("id", "func", "value")
  
  ggplot() +
    geom_line(data = df_cum_means, 
                       aes(x = id, y = value, color = func, group = func), 
                       size = 1)+
    ggtitle(paste("Cumulative mean accuracies for", dataset, "data"))
  #Save plot
  ggsave(file= paste("Plots/Cumulative_means_", dataset, ".png"))
}


####### Question2 #########
## Calculate number and proportion of misclassifications (FP and FN) in crosstable
#Arguments
  ##CrossTable: Crosstable of interest
  ##mis_nb_sum: Vector of number of missclassification for each class
  ##mis_prop_sum: Vector of proportions for each classe (one value per class)
#Returns list:
  ##mis_nb_sum: Vector of sum of number of missclassification for each class (one value per class)
  ##mis_prop_sum: Vector of sum of proportions for each classe (one value per class)
misclassification <- function(CrossTable, 
                              mis_nb_sum, 
                              mis_prop_sum){
  nb_crossTable <- CrossTable$t
  prop_crossTable <- CrossTable$prop.tbl
  for (i in 1:nrow(nb_crossTable)){ #i is the value of the actual class
    #If j and i are equal, the classification is right (predicted = actual)
    for (j in 1:ncol(nb_crossTable)){
      if (i!=j){
        #Sum of number of misclassifications for atucal class i, stored in vector mis_nb_sum
        mis_nb_sum[i] = mis_nb_sum[i]+nb_crossTable[i,j]
        #Sum of proportions of misclassifications for class i
        mis_prop_sum[i] = mis_prop_sum[i]+prop_crossTable[i,j]
      }
    }
  }
  return(list(mis_nb_sum = mis_nb_sum,
              mis_prop_sum = mis_prop_sum))
}

#Calculate misclassifications with a confusion matrix
#Arguments:
  ##confMatrix: Confusion matrix containing FP and FN
  ##mis-nb_sum: Vector of number of missclassification for each class
  ##mis_prop_sum: Vector of proportions for each classe (one value per class)
#Returns list:
  ##mis_nb_sum: Vector of sum of number of missclassification for each class (one value per class)
  ##mis_prop_sum: Vector of sum of proportions for each classe (one value per class)
misclassification_confMatrix <- function(confMatrix, mis_nb_sum, mis_prop_sum){
  nb_crossTable <- confMatrix
  total = 0
  for (i in 1:nrow(nb_crossTable)){ #i is the value of the actual class
    #If j and i are equal, the classification is right (predicted = actual)
    for (j in 1:ncol(nb_crossTable)){
      #Count number of elements
      total = nb_crossTable[i,j] + total
      if (i!=j){
        #Sum of number of misclassifications for atucal class i, stored in vector mis_nb_sum
        mis_nb_sum[i] = mis_nb_sum[i]+nb_crossTable[i,j]
      }
    }
  }
  #Sum of proportions of misclassifications for class i
  mis_prop_sum = mis_nb_sum / total
  return(list(mis_nb_sum = mis_nb_sum,
              mis_prop_sum = mis_prop_sum))
}

#Create barplot of misclassified proportions
#Arguments:
  ##list_misclassified: List of sum of proportions of missclassifications
  #titleList: Title of the barplot
misclassified_proportion_barplot <- function(list_misclassified, titleList){
  i <- 0
  for (proportion in list_misclassified){
    i = i+1
    #Create a dataframe with classes and proportions
    data = data.frame(sum_proportion = proportion, classe = c(1:3))
    ggplot(data = data, aes(x = data$classe, y = data$sum_proportion))+
      geom_bar(stat = "identity")+
      xlab("Classe")+
      ylab("Sum of proportions of misclassification for 10 iteration")+
      ggtitle(paste("Sum of proportions of misclassifications, ", titleList[i]))
    #Save plot
    ggsave(file= paste("Plots/MisclassifiedProportion_", titleList[i], ".png"))
  }
}


############# 3. Variables importance #########
#Returns variable importance for knn
#Arguments:
  ##AllData: Dataset 
  ##predictor: Predictor
  ##perc_predict: Ratio of training data
  ##times: Number of iterations
  ##formula: Formula for training
  ##method: Classification method
  ##tuneGrid: Grid search
#Returns:
  ##Imp: Dataframe of importance of the variables
var_importance <- function(AllData, 
                           predictor, 
                           perc_predict, 
                           times, 
                           formula, 
                           method, 
                           tuneGrid){
  part <- partition_data(AllData, predictor, perc_predict, times)
  #Train the model
  model.fit <- caret::train(formula, method = method, data = part$trainSet,
                            tuneGrid = tuneGrid)
  Imp <- as.data.frame(varImp(model.fit)$importance)
  return(list(Imp = Imp, model = model.fit))
}

#Returns variables importance for random forest
#Arguments:
  ##AllData: Dataset
  ##perc_predict: Ratio of training data
  ##times: Number of iterations
  ##ntree: Number of trees in the forest
  ##mtry: Number of features to sample at each node
  ##nodesize: Number of cases authorised in a leaf
  ##maxnodes: Maximal numbers of nodes
#Returns:
  ##Imp: Dataframe of variables importance
rf_var_importance <- function(AllData, 
                              predict, 
                              perc_predict, 
                              times,
                              ntree, 
                              mtry, 
                              nodesize, 
                              maxnodes){
  part <- partition_data(AllData, predict, perc_predict, times)
  randomF <- randomForest(sensory~., data=part$trainSet, 
                          importance = TRUE, 
                          ntree = ntree, mtry = mtry, 
                          nodesize = nodesize, maxnodes = 20L)
  Imp <- as.data.frame(varImp(randomF))
  return(Imp)
}

#Make a plot showing variables importance for each class
#Arguments:
  ##importance: Data frame of variable importances
  ##name: Name of the plot
#Returns:
  #plot: Plot of variables importance
importance_plot <- function(importance, 
                            name){
  Imp <- data.frame(feature = rownames(importance), importance)
  #Reshape the data
  Imp <- melt(Imp[,c("feature", "X1", "X2", "X3")], id.vars = 1)
  
  plot <- ggplot(data = Imp, aes(x = feature, y =  value))+
    geom_bar(stat = "identity", width = 0.7, position = "dodge", aes(fill = variable))+
    ggtitle(paste("Importance of each feature for each class", name))+
    scale_fill_discrete(name = "Class",
                        breaks = c("X1", "X2", "X3"),
                        labels = c("1", "2", "3"))+
    coord_flip()
  #Save plot
  ggsave(file= paste("Plots/Features_importance_", name, ".png"))
  return(plot)
}

#Make a plot showing variables importance overall for every class
#Arguments:
##importance: Data frame of variable importances
##name: Name of the plot
#Returns:
#plot: Plot of variables importance
overall_importance_plot <- function(importance, 
                            name){
  #plot
  plot <- ggplot(data = importance, aes(x = rownames(importance), y =  Overall))+
    geom_bar(stat = "identity", width = 0.7)+
    ggtitle(paste("Importance of each feature for every class", name)) + 
    coord_flip()
  #Save plot
  ggsave(file= paste("Plots/Overall_Features_importance_", name, ".png"))
  return(plot)
}