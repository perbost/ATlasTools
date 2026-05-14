#' @title Define the variable ranges of all features.
#' @name AtlasDefineMaxMinClinicalData
#' @description Define the variable ranges: maximum and minimum of the feature
#' and low mean values and high mean values for each features. Low and high mean
#' values are defined according the clinical data prediction probability (high
#' if prediction > 0.5 and low if < 0.5).
#' This values are used to plot radarchart (to define axis scale).
#' @param data (list) list of selected feature to analyse.
#' @param clinical.data (string) target clinical data name. 
#' @param df (data frame) clinical data prediction probability.
#' @param mapdata (list) list of data frame containing all feature values.
#' @return data frame with max and min values for each features
#' @export



AtlasDefineMaxMinClinicalData <- function(data, clinical.data, df, mapdata){
  
  # Define max min matrix for radarchart limits
  df.max.min <- data.frame(row = c("max","min","mean.low","mean.high"))
  df.rows.names <-  c("Max",
                      "Min",
                      "Low (mean values)",
                      "High (mean values)")
  
  nodes_high <- df$code[df[, clinical.data]>0.5]
  nodes_low <- df$code[df[, clinical.data]<=0.5]
  
  it = 1
  for(view in names(data)){
    for(feature in data[[view]]$feature){
      
      it=it+1
      
      mini <- min(mapdata[[view]][feature,])
      maxi <- max(mapdata[[view]][feature,])
      
      # average value for each feature of map nodes with a prediction 
      # probability for the associated clinical data of less than 0.5.
      mean.low <- mean(mapdata[[view]][feature, nodes_low])
      # same for nodes with high prediction probability
      mean.high <- mean(mapdata[[view]][feature, nodes_high])
      
      df.max.min <- cbind(df.max.min, 
                          data.frame(mm = c(maxi,mini,mean.low,mean.high)))
      colnames(df.max.min)[it] <- feature
    }
  }
  
  # rename feature as column name:
  colnames(df.max.min) <- gsub("__.*", "", colnames(df.max.min))
  rownames(df.max.min) <- df.rows.names
  
  return(df.max.min[,-1])
}
