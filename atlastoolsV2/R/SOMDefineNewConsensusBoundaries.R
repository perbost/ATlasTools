#' @title Define new clustering boundaries based on consensus clustering
#' @name SOMDefineNewConsensusBoundaries
#' @description Use a function like "heat function" to establish the new 
#' clustering boundaries from the consensus clustering.
#' In order to simulate a kind of heat dispersion with the already known 
#' clusters, we propagate the clustering from the nodes we know to the nodes 
#' we don't know with the while loop (see the function below).
#' The variable "nb.indeter" allows us to know the count of the nodes that don't
#' have an associated cluster yet and so we continue the dispersal as long as
#' all the nodes are not filled in.
#' @param groups data frame with cluster for SOM nodes
#' @param nb.clusters (int) current number of cluster
#' @param Clusters data frame with clustering consensus information
#' @param matrix.som.pts matrix SOM nodes
#' @return rank.matrix.cluster
#' @export


SOMDefineNewConsensusBoundaries <- function(groups, nb.clusters, Clusters, 
                                            matrix.som.pts){
  
  matrix.cluster <- matrix(data=0., ncol=length(groups), nrow=nb.clusters)
  colnames(matrix.cluster) <- names(groups)
  
  for(row in 1:nrow(Clusters)){
    matrix.cluster[Clusters[row,]$cluster, Clusters[row,]$code] <- 1.
  }
  
  nb.zero <- sum(colSums(matrix.cluster)==0)
  nb.indeter <- sum(colSums(apply(matrix.cluster, 2, rank) == nb.clusters) == 0)
  
  while(nb.zero > 0 | nb.indeter > 0){
    matrix.new <- matrix(data=0., ncol=length(groups), nrow=nb.clusters)
    for(col in 1:ncol(matrix.cluster)){
      for(row in 1:nrow(matrix.cluster)){
        if(matrix.cluster[row, col] == 0) next
        neighbors.nodes <- .SOMGetNodeNeighbors(col, 
                                                              matrix.som.pts)
        n_neighbor <- length(neighbors.nodes)
        for(neighbor in neighbors.nodes){
          matrix.new[row, neighbor] <- 
            matrix.new[row, neighbor] + matrix.cluster[row, col] / n_neighbor
        }
      }
    }
    matrix.cluster <- matrix.new
    nb.zero <- sum(colSums(matrix.cluster) == 0)
    rank.matrix.cluster <- apply(matrix.cluster, 2, rank)
    nb.indeter <- sum(colSums(rank.matrix.cluster == nb.clusters) == 0)
  }  
  return(rank.matrix.cluster)
}
