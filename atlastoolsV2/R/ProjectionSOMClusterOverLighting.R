#' @title Function to create a figure with the cluster zones highlighted on the 
#' SOM map.
#' @name ProjectionSOMClusterOverLighting
#' @description This function generates a pdf with the SOM map of the current
#' MOFa model. Nodes in the map for a chosen cluster are highlighted in green 
#' by default, and other nodes in other clusters are highlighted in gray by 
#' default.
#' Different colors can be selected with the main.cluster.color and 
#' background.cluster.color parameters.
#' This function generates a pdf image for each map in the 
#' '4_Best_maps_consensus' folder.
#' @param nb.cluster (int) numerous of cluster to highlight.
#' @param path.maps path to the map folder.
#' @param model.name name og the current MOFA model.
#' @param nb.samples (description)int) number of sample.
#' @param main.cluster.color (str) Hexadecimal code to define color for the main
#' cluster to highlight.
#' @param background.cluster.color (str) Hexadecimal code to define color for 
#' others background clusters.
#' @export


ProjectionSOMClusterOverLighting <- function(nb.cluster, path.maps, model.name,
                                             nb.samples, 
                                             main.cluster.color="#66CC00",
                                            background.cluster.color="#E0E0E0"){
  
  path.consensus <- file.path(path.maps,
                              "4_Best_maps_consensus",
                              model.name,
                              sprintf("Cluster_%02d", nb.cluster))
  if(!dir.exists(path.consensus))stop("No best_maps directory")
  
  list.directory.consensus <- list.dirs(path.consensus, 
                                        full.names=T, 
                                        recursive=F)
  
  palette.name <- colorRampPalette(c(background.cluster.color, 
                                     main.cluster.color), 
                                   interpolate="linear")
  list.clusters <- list(1:nb.cluster)
  
  for(path.consensus.cluster in list.directory.consensus){
    
    path.map.som <- file.path(path.consensus.cluster, "map.rds")
    data.som <- readRDS(path.map.som)
    
    groups.consensus <- readr::read_delim(file.path(path.consensus.cluster,
                                                    "groups_consensus.csv"), 
                                          delim=";", 
                                          escape_double=FALSE, 
                                          trim_ws=TRUE)
    
    groups <- t(groups.consensus[,"x"])[1,]
    names(groups) <- t(groups.consensus[,1])

    path.clinical.projection <- file.path(path.consensus.cluster,
                                          "Clinical_Projection")
    path.clusters.overlighting <- file.path(path.clinical.projection, 
                                            "Clusters_overlighting")
    if(!dir.exists(path.clusters.overlighting)) 
      dir.create(path.clusters.overlighting, recursive=T)
    
    for(cluster in list.clusters[[1]]){
      name <- paste0("cluster_", cluster)

      groups.consensus[, name] <- 0
      groups.consensus[groups.consensus$x == cluster, ][, name] <- 1
      
      groups.cluster <- t(groups.consensus[, name])[1,]
      
      SomMapCluster(data.som, 
                                        path.clusters.overlighting,
                                        groups=groups, 
                                        property=groups.cluster,
                                        title=name, 
                                        color.palette=palette.name,
                                        nb.samples=nb.samples)
    }
  }
}
