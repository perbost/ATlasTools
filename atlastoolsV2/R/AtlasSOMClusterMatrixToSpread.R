#' @title read and build cluster matrix for current nth.map
#' @name AtlasSOMClusterMatrixToSpread
#' @description Transform clusters.csv data frame in a matrix with nb_cluster
#' columns (column name: "map_1_cl").
#' Each cell contains the number of the node if the associated sample is part
#' of the cluster of the column and contains 0 otherwise.
#' @param path.maps.model.cluster.nth.map path to maps folder
#' @param delim character delimiter for read file
#' @param escape_double (bool) default: FALSE
#' @param trim_ws (bool) default: TRUE
#' @param matrix.agreement (bool) Complete documentation argument
#' @param nth.map current map number
#' @param nb.cluster (int) number of cluster for data frame shape.
#' @return clusters
#' @export


AtlasSOMClusterMatrixToSpread <- function(path.maps.model.cluster.nth.map,
                                          delim=";", escape_double=FALSE, 
                                          trim_ws=TRUE, matrix.agreement=F,
                                          nth.map=NULL, nb.cluster=NULL){
  
  nth.map.clusters <- 
    readr::read_delim(file.path(path.maps.model.cluster.nth.map, 
                                "Clusters.csv"),
                      delim=delim,
                      escape_double=escape_double,
                      trim_ws=trim_ws)

  clusters <- tidyr::spread(nth.map.clusters, "cluster", "code")[,-c(1,3)]
  clusters <- as.matrix(clusters[,-c(1)])
  clusters[!is.na(clusters)] <- 1
  clusters[is.na(clusters)] <- 0
  
  # we check if all number cluster are as column and if not we add it:
  column.sequence.name <- 1:nb.cluster
  column.cluster.name <- as.integer(colnames(clusters))
  missing.column.cluster <- setdiff(column.sequence.name, column.cluster.name)
  
  if(!length(missing.column.cluster) == 0){
    
    clusters <- data.frame(clusters, check.names = FALSE)
    
    for(i in missing.column.cluster) {
      clusters[[as.character(i)]] <- 0
    }
    
    clusters <- clusters[, order(as.numeric(colnames(clusters)))]
  }
 
  rownames(clusters) <- nth.map.clusters$sample
  if(matrix.agreement)
    colnames(clusters) <- paste("map", nth.map, colnames(clusters), sep="_")
  
  return(clusters)
}
