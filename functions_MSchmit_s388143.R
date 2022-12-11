
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
  sampleDist <- dist(AllData_enose_sens)
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
           col = colors, main = paste("HCA analysis", main))
  graphics.off()
  return(heatmap)
}
