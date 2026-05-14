#' @title Function to compute Kmeans clustering with ConsensusClusterPlus 
#' algorithm.
#' @name AtlasSomKmeansConsensus
#' @description This function use ConsensusClusterPlus algorithm to compute the
#' clustering consensus of distances SOM nodes.
#' If we don't provide number of cluster, we run the algorithm for each number
#' of cluster of the list cluster number.
#' Then, outputs are save in Map folder path.
#' Then, it need human supervisor to do number of cluster choice.
#' @param path.result (str) path to save result.
#' @param distances (list) data to be clustered.
#' @param nb.cluster.min (int) minimum number of cluster (default=2).
#' @param nb.cluster.max (int) maximum number of cluster (default=10).
#' @param clusterAlg (str) name of the algorithm to use (default=km).
#' @param distance (str) type of distance to use (default=euclidean).
#' @param reps (int) iteration number for ConsensusClusterPlus algortihm.
#' @param pItem see ConsensusClusterPlus documentation
#' @param return.nb.cluster (int) condition to return result only if we have at
#' least 1 cluster.
#' @export


AtlasSomKmeansConsensus <- function(path.result, distances, nb.cluster.min=2, 
                                    nb.cluster.max=10, clusterAlg="km",
                                    distance="euclidean", reps=100,
                                    pItem=0.8, return.nb.cluster=0){
  
  path.ConsensusClusterPlus.folder <- file.path(path.result, 
                                                "ConsensusClusterPlus_Outputs")
  if(!dir.exists(path.ConsensusClusterPlus.folder))
    dir.create(path.ConsensusClusterPlus.folder, recursive=T)
  
  clustering.result <- 
    ConsensusClusterPlus::ConsensusClusterPlus(distances,
                                             maxK=nb.cluster.min:nb.cluster.max,
                                               clusterAlg=clusterAlg,
                                               distance=distance,
                                               reps=reps,
                                               pItem=pItem,
                                               plot="pdf",
                                               title=
                                              path.ConsensusClusterPlus.folder)
  
  # Save results
  if(ValidatePath(file.path(path.ConsensusClusterPlus.folder,
                            "consensusClusterPlus_results.rds"))){
    saveRDS(clustering.result,
            file=file.path(path.ConsensusClusterPlus.folder,
                                          "consensusClusterPlus_results.rds"))
  }
  
  if(return.nb.cluster>0){
    return(clustering.result[[return.nb.cluster]]$consensusClass)
  }
}
  