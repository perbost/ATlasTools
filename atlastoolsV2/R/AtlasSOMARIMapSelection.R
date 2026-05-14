#' @title We use adjusted rand index score to find most similar clustering
#' of the clustering consensus.
#' @name AtlasSOMARIMapSelection
#' @description We save a data frame with a list of the n best maps contains 
#' clustering the most similar as consensus clustering. We measure the similarity
#' with adjusted rand index score.
#' @param path.maps.baseline.cluster path to cluster folder.
#' @param path.consensus.clustering path to folder with consensus information.
#' @param clustering.consensus data frame with clustering consensus.
#' @param nb.SOM.maps number of maps to generate.
#' @param nb.maps.to.keep (int) number of best maps to keep.
#' @export


AtlasSOMARIMapSelection <- function(path.maps.baseline.cluster, 
                                    path.consensus.clustering, 
                                    clustering.consensus, nb.SOM.maps, 
                                    nb.maps.to.keep=50){
  
  path.global.ARI.format.clustering.matrix <-
    file.path(path.consensus.clustering,
              sprintf("global_clustering_matrix_ARI_format_%d.csv",
                      nb.SOM.maps))
  
  if(file.exists(path.global.ARI.format.clustering.matrix)){
    clusterings <- readr::read_csv(path.global.ARI.format.clustering.matrix)
  } else{
    
    clusterings <- data.frame()
    
    for(nth.map in 1:nb.SOM.maps){
      
      path.cluster <- file.path(path.maps.baseline.cluster,
                                sprintf("Map_lin%03d", nth.map),
                                "Clusters.csv")
      
      cluster.csv <-readr::read_delim(path.cluster,
                                      delim=";",
                                      escape_double=FALSE,
                                      trim_ws=TRUE)
      
      cluster <- cluster.csv$cluster
      clusterings <- rbind(clusterings, cluster)
    }
    clusterings <- t(clusterings)
    if(ValidatePath(path.global.ARI.format.clustering.matrix)){
      readr::write_csv(as.data.frame(clusterings), 
                       path.global.ARI.format.clustering.matrix)
    }
  }
  
  # Compute ARI between each clustering in matrix and target clustering:
  ari.values <- apply(clusterings, 2, 
                      function(x) mclust::adjustedRandIndex(x,
                                                  clustering.consensus$cluster))
  
  maps.to.keep <- head(rev(order(ari.values)), n=nb.maps.to.keep)
  
  path.maps.to.keep <- data.frame()
  
  for(nth.map in maps.to.keep){
    
    path.maps.to.keep <- rbind(path.maps.to.keep, 
                               data.frame(map=sprintf("Map_lin%03d", nth.map),
                                          ARI_score=ari.values[[nth.map]]))
  }
  if(ValidatePath(file.path(path.consensus.clustering, 
                            sprintf("best_map_ARI_%d.csv", nb.SOM.maps)))){
    readr::write_csv(path.maps.to.keep,
                     file.path(path.consensus.clustering, 
                               sprintf("best_map_ARI_%d.csv", nb.SOM.maps)))
  }
}
