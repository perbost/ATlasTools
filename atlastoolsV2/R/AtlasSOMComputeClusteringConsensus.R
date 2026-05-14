#' @title Compute consensus clustering from jaccard distances
#' @name AtlasSOMComputeClusteringConsensus
#' @description This function compute a dataframe with the consensus clustering
#' based on all clustering made and jaccard distances between them.
#' First, we compute the Jaccard distances to measure the dissimilarity between
#' all clusters in global cluster matrix.
#' Then, we cut the obtained dendrogramme with nb_cluster variable.
#' @param global_clustering_matrix data frame with all clustering results
#' @param path_save path to save pdf and csv 
#' @param nb_cluster current number of cluster
#' @param nb.SOM.maps (int) number of generated map
#' @param plot_title_jaccard_distances title of the plot Jaccard distances
#' @param clustering.method (str) clustering method used.
#' @return clustering_consensus, jaccard_distances_all_clusters
#' @export


AtlasSOMComputeClusteringConsensus <- function(jaccard_distances_all_clusters,
                                               path_save, n.cluster,
                                               nb.SOM.maps, 
                                               clustering.method=
                                                 "hierarchical_clustering",
                                               plot_title_jaccard_distances=
                                              "heatmap_global_jaccard_sample"){

  breaksList <- seq(0, 1, by = 0.01)
  colors <- 
    colorRampPalette(
      c("white", "white","white",
               RColorBrewer::brewer.pal(4, name="YlOrRd")))(length(breaksList))

  jc_sample <- pheatmap::pheatmap(jaccard_distances_all_clusters,
                                  color=colors,
                                  breaks=breaksList,
                                  cutree_rows=n.cluster,
                                  cutree_cols=n.cluster,
                                  fontsize=6)

  file_name <- paste0(plot_title_jaccard_distances, "_" , clustering.method)
  
  HeatmapGlobalJaccardSample(jc_sample, 
                                                 nb.SOM.maps,
                                                 path_save,
                                                 file_name=file_name)

  clustering_consensus <- 
    data.frame(sample=rownames(jaccard_distances_all_clusters),
               cluster=cutree(jc_sample$tree_row,
                              k=n.cluster))
  
  path.save <- file.path(path_save, 
                         sprintf("clustering_consensus_%d_%s.csv",
                                 nb.SOM.maps, clustering.method))
  if(ValidatePath(path.save)){
    readr::write_csv(clustering_consensus, path.save)
  }

  
  return(clustering_consensus)
}
