#' @title Define new group values
#' @name SOMGetNewConsensusClusteringElements
#' @description From rank matrix result, in this function we define the new
#' groups values for SOM nodes.
#' @param groups data frame with cluster for SOM nodes
#' @param nb.clusters (int) current number of cluster
#' @param clusters data frame with clustering consensus information
#' @param clusters.original original cluster
#' @param matrix.som.pts matrix SOM nodes
#' @param path.map.directory path to save consensus results
#' @return new.groups
#' @export


SOMGetNewConsensusClusteringElements <- function(groups, nb.clusters, clusters,
                                                 clusters.original, 
                                                 matrix.som.pts,
                                                 path.map.directory){
  
  rank.matrix.cluster <- 
    SOMDefineNewConsensusBoundaries(groups,
                                                  nb.clusters,
                                                  clusters,
                                                  matrix.som.pts)
  new.groups<- c()
  for(i in 1:length(groups)){
    new.groups <- c(new.groups, which(rank.matrix.cluster[,i] == nb.clusters))
  }
  
  cluster.new <- clusters.original
  for(row in 1:nrow(cluster.new)){
    cluster.new[row,]$cluster <- new.groups[cluster.new[row,]$code]
  }
  names(new.groups) <- names(groups)

  path.save <- file.path(path.map.directory, "groups_consensus.csv")
  
  if(ValidatePath(path.save)){
    write.csv2(new.groups, path.save)
  }
  return(list(new.groups=new.groups, cluster.new=cluster.new))
}
