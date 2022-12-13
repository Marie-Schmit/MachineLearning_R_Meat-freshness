
# Merge imported data
merge_data <- function(predictor, predict, AllData){
  # Combine all rows from enose and sensory
  merged <- merge(predictor, predict, by = "row.names")
  rownames(merged) = merged[,1]
  # Remove raw names from merged column
  AllData <- as.data.frame(merged[-1])
  return(AllData)
}

pca_visualisation <- function(pca.AllData, AllData, ncomp, sensory, style){
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

pca_var <- function(pca.AllData, sensory, AllData){
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


# Function to partition dataset into train and test values
## Return a list of two vectors (train and test)
## perc_predict is the percentage of train data
partition <- function(AllData, predict, perc_predict, times){  
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
    trainSet <- trainSet[,1:(ncol(trainSet)-1)]
    testSet <- testSet[,1:(ncol(testSet)-1)]
  return(list(trainSet = trainSet,
              testSet = testSet,
              trainCl = trainCl,
              testCl = testCl))
  }
}

############################# Classification #########################
######### knn #########
#Model training and optimisation: find the best scale and k parameter
knn.optimisation <- function(trainSet, testSet, trainCl, testCl, n, scalingMethod){
  #Remove the class variable of the train and test sets
  trainSet.knn <- trainSet[, -ncol(trainSet)]
  testSet.knn <- testSet[, -ncol(testSet)]
  
  #Variable initialisation
  k.accuracies <- c()
  bestAccuracy <- 0
  currentScale <- c()
  currentk <- 0

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

#knn model and cross table display
cross_table_knn <- function(trainSet, testSet, trainCl, testCl, k){
  #Remove the class variable of the train and test sets
  trainSet.knn <- trainSet[, -ncol(trainSet)]
  testSet.knn <- testSet[, -ncol(testSet)]
  
  #Create model with scaled data
  model.k <- knn(trainSet.knn, testSet.knn, trainCl, k)
  
  #Cross table
  cross.table <- CrossTable(testCl, model.k, prop.chisq=FALSE, prop.t=FALSE, prop.c=FALSE, prop.r=FALSE)
  return(cross.table = corss.table, model.k = model.k)
}


#Create a partition with times iteration
#For each iteration, run the model and store the accuracy
model.run <- function(AllData, predict, perc_predict, times, operation){  
  set.seed(8)
  #Preserve correspondance between numerical and categorical data (predictor and response respectively)
  trainIndex <- createDataPartition(predict, p = perc_predict,
                                    list = FALSE,
                                    times = times)
  #List of accuracies init
  accuracies <- c()
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
  }
  
  return(list(trainSet = trainSet, 
              testSet = testSet,
              trainCl = trainCl,
              testCl = testCl,
              accuracies = accuracies,
              model = model))
}

#Cumulative means accuracy calculation
cumulative.mean.accuracy <- function(accuracies){
  cumulative.means <- c()
  for(i in 1:length(accuracies)) {
    cumulative.means <- c(cumulative.means, mean(accuracies[1:i]))
  }
  return(cumulative.means)
}