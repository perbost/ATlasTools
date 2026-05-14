#' @title Compute consensus clustering
#' @name AtlasSOMCreateGroupCluster
#' @description This function determine each cluster group for samples.
#' First, this function compute the euclidean distance matrix between codes's
#' SOM map.
#' Code is the embedding of samples (using MOFA) and each code are compute for
#' all nodes of the map.
#' Then, Hierarchical clustering analysis on this distances is made with the
#' ward.2 algorithm.
#' If the number of cluster is not provided, we compute the best number of
#' cluster based on the hierarchical clustering.
#' Then, we cut the obtained dendrogramme with nb.cluster variable.
#' This function can also compute kmeans clustering with ConsensusClusterPlus
#' algorithm.
#' @param data.som (.rds) SOM map 
#' @param path.map.cluster.nth.map.clustering path to the cluster.csv file for
#' the nth map.
#' @param nb.cluster (int) number of cluster groups.
#' @param decision.help.nb.cluster (bool) if we plot figure for help decision 
#' about best number of cluster to choose.
#' @param clustering.method (str) clustering method used.
#' @return list of group for each sample
#' @export


AtlasSOMCreateGroupCluster <- function(data.som, 
                                       path.map.cluster.nth.map.clustering,
                                       nb.cluster=NULL, 
                                       decision.help.nb.cluster=F,
                                       clustering.method=
                                         "hierarchical_clustering"){
  
  path.map.cluster.nth.map.clustering.cluster <- 
    file.path(path.map.cluster.nth.map.clustering, "Clusters.csv")
  
  if(dir.exists(path.map.cluster.nth.map.clustering.cluster)){
    cluster <- 
      readr::read_delim(path.map.cluster.nth.map.clustering.cluster, 
                        delim = ";", 
                        escape_double=FALSE,
                        trim_ws=TRUE)
  } else{
    distance.matrix <- as.dist(dist(data.som$codes[[1]]))
    
    if(clustering.method == "hierarchical_clustering"){
      
      hierrarchical.cluster.analysis <- hclust(distance.matrix, 
                                               method="ward.D2")
      
      if(is.null(nb.cluster) || decision.help.nb.cluster){
        
        data.som.codes <- data.som$codes[[1]]
        
        nb.cluster.optimal <- 
          AtlasSOMOptimalClusterNumber(
            return.best.nb.cluster=T, 
            data.som.codes=data.som.codes,
            hierrarchical.cluster.analysis=hierrarchical.cluster.analysis,
            path=path.map.cluster.nth.map.clustering)
        
        if(is.null(nb.cluster)) nb.cluster <- nb.cluster.optimal
      }
      cluster <- cutree(hierrarchical.cluster.analysis, k=nb.cluster)
    }
    
    if(clustering.method == "kmeans_clustering"){
      
      path.maps.model.cluster.nth.map.cluster.kmeans <- 
        file.path(path.map.cluster.nth.map.clustering, 
                  "groups_kmeans_ccp.csv")
      
      if(file.exists(path.maps.model.cluster.nth.map.cluster.kmeans)){
        dt.cluster <- 
          readr::read_delim(path.maps.model.cluster.nth.map.cluster.kmeans,
                            delim=";",
                            escape_double=FALSE,
                            trim_ws=TRUE)
        
        cluster <- dt.cluster$x
        
      } else{
        
        cluster <- AtlasSomKmeansConsensus(
          path.map.cluster.nth.map.clustering, 
          distance.matrix, 
          nb.cluster.min=nb.cluster, 
          nb.cluster.max=nb.cluster, 
          clusterAlg="km",
          distance="euclidean",
          reps=100,
          pItem=0.8,
          return.nb.cluster=nb.cluster)
        
        write.csv2(cluster, path.maps.model.cluster.nth.map.cluster.kmeans)
      }
    }
  }
  return(cluster)
}

  