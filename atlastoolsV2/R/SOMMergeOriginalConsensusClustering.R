#' @title Merge original and consensus clustering to return new consensus group
#' and clustering.
#' @name SOMMergeOriginalConsensusClustering
#' @description This function load original and clustering consensus matrix
#' of a specific map. Then merge the both matrix and return a list of all 
#' clustering.
#' @param path.map.original path to the original folder.
#' @param path.map.consensus path to the consensus folder.
#' @param nb.SOM.maps number of generated maps to compute consensus clustering.
#' @param clustering.method (str) clustering methode used.
#' @param return a list of cluster matrix (merged clustering, original 
#' clustering and consensus clustering).
#' @export


SOMMergeOriginalConsensusClustering <- function(path.map.original,
                                                path.map.consensus, 
                                                nb.SOM.maps,
                                                clustering.method){
  
  cluster.original <- readr::read_delim(file.path(path.map.original, 
                                                  "Clusters.csv"), 
                                        delim = ";", 
                                        escape_double=FALSE,
                                        trim_ws=TRUE)
  # below code line in comment was for a specific case for a specific dataset.
  # cluster.original$sample <- substr(cluster.original$sample,7,18)
  
  cluster.consensus <- 
    readr::read_csv(file.path(path.map.consensus, 
                              sprintf("clustering_consensus_%d_%s.csv",
                                      nb.SOM.maps, clustering.method)))
  # below code line in comment was for a specific case for a specific dataset.
  # cluster.consensus$sample <- substr(cluster.consensus$sample, 1, 12)
  
  clusters <- 
    merge(cluster.original[,!names(cluster.original) %in% c("cluster")],
          cluster.consensus,
          by=("sample"))
  
  return(list(clusters=clusters, 
              cluster.original=cluster.original, 
              cluster.consensus=cluster.consensus))
}
