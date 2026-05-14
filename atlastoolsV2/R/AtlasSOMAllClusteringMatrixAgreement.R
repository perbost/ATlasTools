#' @title Compute matrix agreement of the "nb.SOM.maps" clustering results.
#' @name AtlasSOMAllClusteringMatrixAgreement
#' @description We compute all clustering matrix with shape (n_sample, 
#' n_seed*n_cluster).
#' For each clustering loop (among the nb.SOM.maps made), we provide clustering
#' result in this big matrix. Column names is loop number + cluster number.
#' This is a binary matrix with 1 for cluster belonging and zero other.
#' @param path_save path to save the global clustering matrix.
#' @param path.maps.model.cluster path to load existing clusters.csv for each 
#' generated map.
#' @param nb.SOM.maps (int) number of generated map.
#' @param nb.cluster (description)int) number of cluster
#' @param nb.samples (int) number of sample
#' @param compare.models (bool) if we want to compare current clustering with
#' clustering of other model for same sample.
#' @param path.models.compare path to the clustering matrix of the model to 
#' compare.
#' @param row.names.remove.constante (bool) if we want clean row names by 
#' removing all constant nomenclature.
#' @return global.clustering.matrix
#' @param clustering.method (description)str) clustering method name 
#' (default=hierarchical_clustering)
#' @export


AtlasSOMAllClusteringMatrixAgreement <- function(path_save, 
                                                 path.maps.model.cluster,
                                                 nb.SOM.maps, nb.cluster,
                                                 nb.samples, compare.models=F, 
                                                 path.models.compare=NULL,
                                                 row.names.remove.constante=T,
                                                 clustering.method=
                                                   "hierarchical_clustering"){
  
  path.df.save <- file.path(path_save,
                            sprintf("global_clustering_matrix_%d_%s.csv",
                                    nb.SOM.maps, clustering.method))
  
  if(file.exists(path.df.save)){
    
    global.clustering.matrix <- read.csv(path.df.save, row.names=1)
    
  } else{
    
    for(nth.map in 1:nb.SOM.maps){

      # load clusters.csv for each generated maps to complete global clustering
      # matrix:
      path.maps.model.cluster.nth.map <- file.path(path.maps.model.cluster,
                                                   sprintf("Map_lin%03d",
                                                           nth.map))
      
      path.path.map.cluster.nth.map.clustering <- 
        file.path(path.maps.model.cluster.nth.map, clustering.method)
      
      
      clusters <-  
        AtlasSOMClusterMatrixToSpread(
          path.path.map.cluster.nth.map.clustering,
          matrix.agreement=T,
          nth.map=nth.map,
          nb.cluster=nb.cluster)
      
      if(!all(dim(clusters) == c(nb.samples, nb.cluster))) 
        warning(sprintf("cluster non conformed %d", nth.map))
      
      if(nth.map == 1) global.clustering.matrix <- clusters
      else global.clustering.matrix <- cbind(global.clustering.matrix, clusters)
    }
    
    if(row.names.remove.constante){
      rownames(global.clustering.matrix) <- 
        gsub("ATLAS_", "", gsub("@SCREENING", "", 
                                rownames(global.clustering.matrix)))
    }
    
    if(compare.models){
      global.clustering.matrix <- 
        AtlasSOMCompareModels(global.clustering.matrix,
                                            path.models.compare)
    }
    
    global.clustering.matrix_df <- 
      data.frame(sample=rownames(global.clustering.matrix),  
                 global.clustering.matrix)
    
    if(ValidatePath(path.df.save)) {
      write.csv(global.clustering.matrix_df, path.df.save, row.names = FALSE)
    }
  }
  return(global.clustering.matrix)
}
