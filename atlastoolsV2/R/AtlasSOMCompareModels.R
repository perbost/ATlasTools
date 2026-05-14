#' @title Compare clustering of two models for same samples
#' @name AtlasSOMCompareModels
#' @description This function compare clustering of two models.
#' We add in sample name the cluster number of other model for the same sample.
#' We fill in the cluster group number of the other model by add 
#' ("_cl%d", cluster_group) in the sample name.
#' @param data current data frame with all clusters groups
#' @param path.other.model.clustering path to the csv file of clustering for 
#' the model to compare
#' @return data
#' @export


AtlasSOMCompareModels <- function(data, path.other.model.clustering){
  
  other.model.clustering <- readr::read_delim(path.other.model.clustering, 
                                              delim = ";", 
                                              escape_double=FALSE,
                                              trim_ws=TRUE)
  
  for(row in 1:nrow(data)){
    if(rownames(data)[row] %in% other.model.clustering$sample){
      cl <- other.model.clustering[other.model.clustering$sample == 
                                     rownames(data)[row],]$cluster
      rownames(data)[row] <- paste(rownames(data)[row], cl, sep='_cl')
    }
  }
  
  return(data)
}
