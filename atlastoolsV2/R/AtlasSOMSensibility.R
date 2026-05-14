#' @title Compute the sensibility of cluster_consensus.
#' @name AtlasSOMSensibility
#' @description Compute the sensibility of cluster_consensus.
#' Distance between these samples.
#' This sum should be as small as possible because we want the distance to be 
#' as small as possible for samples of the same cluster.
#' @param cluster_consensus data frame with clustering consensus results
#' @param jaccard_distances_all_clusters jaccard distances between all clusters
#' @return mean sensibility
#' @export


AtlasSOMSensibility <- function(cluster_consensus, 
                                jaccard_distances_all_clusters){

  sensibility <- c()

  for(sample_1 in 1:nrow(jaccard_distances_all_clusters)){
    for(sample_2 in sample_1:nrow(jaccard_distances_all_clusters)){
      if(sample_1 == sample_2) next

      cluster_1 <- cluster_consensus$cluster[sample_1]
      cluster_2 <- cluster_consensus$cluster[sample_2]

      if(cluster_1 == cluster_2){
        sensibility <- c(sensibility,
                         jaccard_distances_all_clusters[sample_1, sample_2])
      }
    }
  }
  return(mean(sensibility))
}
