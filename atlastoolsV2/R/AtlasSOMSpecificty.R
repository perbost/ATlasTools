#' @title Compute the specificity of cluster_consensus.
#' @name AtlasSOMSpecificty
#' @description Compute the specificity of cluster_consensus.
#' Here we add the distances of clusters that are not clustered together.
#' We want this value to be as high as possible because two samples that are not 
#' from the same cluster should have a large distance.
#' @param cluster_consensus data frame with clustering consensus results
#' @param jaccard_distances_all_clusters jaccard distances between all clusters
#' @return mean specificity
#' @export

AtlasSOMSpecificty <- function(cluster_consensus, 
                               jaccard_distances_all_clusters){

  specificity <- c()

  for(sample_1 in 1:nrow(jaccard_distances_all_clusters)){
    for(sample_2 in sample_1:nrow(jaccard_distances_all_clusters)){
      if(sample_1 == sample_2) next

      cluster_1 <- cluster_consensus$cluster[sample_1]
      cluster_2 <- cluster_consensus$cluster[sample_2]

      if(cluster_1 != cluster_2){
        specificity <- c(specificity,
                         jaccard_distances_all_clusters[sample_1, sample_2])
      }
    }
  }
  return(mean(specificity))
}
