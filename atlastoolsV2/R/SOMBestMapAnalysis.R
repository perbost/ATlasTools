#' @title Additional analyse of best maps selected based on scores.
#' @name SOMBestMapAnalysis
#' @description We add more specific analysis for selected maps based on 
#' predefined scores.
#' In a first step we select the n best maps according to the scores we have 
#' calculated (diagonal score and score). 
#' Then, for each of these selected maps, we copy the original folder of the 
#' selected map to a new location (in order to facilitate its access later). 
#' We then merge the values of the consensus clustering with the values of the 
#' original clustering, keeping the consensus values.
#' We obtain a new clustering based on the consensus clustering of the current 
#' model with the current number of clusters.
#' We then create the new groups according to this new clustering to create the 
#' new figures.
#' @param nb.cluster (int) number of cluster to test.
#' @param path.maps path to maps folder.
#' @param model.name name of the current model to analysis.
#' @param nb.SOM.maps (int) number of maps generated.
#' @param nb.best.maps.to.keep (int) number of the first best maps to keep 
#' (for each score).
#' @param best.map.type (str) type of score to use for best maps selection.
#' @param verbose (bool) if function print messages or not.
#' @param clustering.method (str) clustering method used.
#' @export


SOMBestMapAnalysis <- function(nb.cluster, path.maps, model.name, nb.SOM.maps, 
                               nb.best.maps.to.keep=3, best.map.type="ARI",
                               verbose=F, clustering.method=
                                 "hierarchical_clustering"){
  
  best.map.type <- tolower(best.map.type)
  if(!(best.map.type %in% c("ari", "baseline"))) 
    stop("You have to choose correct best map type among 'ARI' or 'baseline'")
  
  # create target folder to save results:
  path.original <- file.path(path.maps, 
                             "1_Baseline_analysis",
                             model.name,
                             sprintf("Cluster_%02d", nb.cluster))
  
  path.result <- file.path(path.maps, 
                           "2_Consensus_analysis",
                           model.name,
                           sprintf("Cluster_%02d", nb.cluster))
  
  path.best.maps <- file.path(path.maps, 
                              "3_Best_maps",
                              model.name,
                              best.map.type, 
                              sprintf("Cluster_%02d", nb.cluster))
  if(!dir.exists(path.best.maps)) dir.create(path.best.maps, recursive=T)

  keep.ARI.maps <- F
  keep.baseline.maps <- F
  if(best.map.type == "ari") keep.ARI.maps <- T
  if(best.map.type == "baseline") keep.baseline.maps <- T
  
  keep.maps <- SOMMapsToKeep(path.result, 
                                           nb.SOM.maps,
                                           nb.best.maps.to.keep,
                                          keep.baseline.maps=keep.baseline.maps,
                                           keep.ARI.maps=keep.ARI.maps)

  for(map.name in keep.maps){
    
    path.original.map <- file.path(path.original, map.name)
    path.target.map <- file.path(path.best.maps, map.name)
    
    file.copy(path.original.map, 
              path.best.maps,
              recursive=TRUE, overwrite=T)

    list.clusters <- SOMMergeOriginalConsensusClustering(path.target.map,
                                                         path.result, 
                                                         nb.SOM.maps,
                                                         clustering.method)
    
    clusters <- list.clusters$clusters
    
    data.som <- readRDS(file=file.path(path.original.map, "map.rds"))
    groups <- AtlasSOMCreateGroupCluster(data.som, 
                                                       path.target.map,
                                                       nb.cluster=nb.cluster)
    
    SomMapCluster(data.som, path.target.map,
                                      groups=groups, add.boundaries=T,
                                      add.samples=T, is.samples.colored=T,
                                      clusters=clusters, property=groups,
                                      title=
                                      "SOM_cluster_sample_consensus_clustering",
                                      nb.samples=nrow(clusters))
  }
}
