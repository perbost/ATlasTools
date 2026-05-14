#' @title Determine and update best clustering for best map.
#' @name SOMTransformToConsensusMap
#' @description In this function we transform all original maps with new
#' consensus clustering values and we compute new consensus boundaries on 
#' the original map.
#' Then, we compute scores regarding the fit between the new consensus 
#' clustering of the samples and clustering of the node (determined by the new
#' consensus boundaries).
#' @param parallel.run computing parameter for parallel running.
#' @param model.name current model with current list.nb.clusters and current
#' nth.map.
#' @param path.maps path to the maps folder.
#' @param nb.SOM.maps (int) number of generated maps.
#' @param list.nb.clusters (list) list number of cluster to run.
#' @param verbose (bool) if function print messages or not.
#' @param clustering.method (str) clustering method used.
#' @export


SOMTransformToConsensusMap <- function(parallel.run, model.name, path.maps, 
                                       nb.SOM.maps, list.nb.clusters, 
                                       verbose=F, clustering.method=
                                         "hierarchical_clustering"){
  
  .CreateConsensMapWorkflow <- function(nth.map, nb.clusters, model.name,
                                        path.result, path.maps, nb.SOM.maps,
                                        clustering.method){

    path.map.directory <- file.path(path.maps,
                                    "1_Baseline_analysis",
                                    model.name,
                                    sprintf("Cluster_%02d", nb.clusters),
                                    sprintf("Map_lin%03d", nth.map))
    if(!dir.exists(path.map.directory)) stop("No Map directory")
    
    path.map.cluster.nth.map.clustering <- file.path(path.map.directory,
                                                          clustering.method)
    
    list.clusters <- 
      SOMMergeOriginalConsensusClustering(
        path.map.cluster.nth.map.clustering,
        path.result, 
        nb.SOM.maps,
        clustering.method)
    
    clusters <- list.clusters$clusters
    cluster.original <- list.clusters$cluster.original
    cluster.consensus <- list.clusters$cluster.consensus
    
    data.som <- readRDS(file=file.path(path.map.directory, "map.rds"))
    groups <- 
      AtlasSOMCreateGroupCluster(data.som, 
                                      path.map.cluster.nth.map.clustering,
                                      nb.cluster=nb.clusters,
                                      clustering.method=clustering.method)

    nb.samples <- nrow(clusters)
    
    SomMapCluster(data.som,
                                      path.map.cluster.nth.map.clustering, 
                                      groups=groups,
                                      add.boundaries=T,
                                      add.samples=T, 
                                      is.samples.colored=T,
                                      clusters=clusters, 
                                      property=groups,
                                      title="SOM_map_clusters_consensus",
                                      nb.samples=nb.samples)
    
    matrix.som.pts <- data.som$grid$pts
    
    new.elements.clustering.consensus <- 
      SOMGetNewConsensusClusteringElements(groups, 
                                                         nb.clusters,
                                                         clusters, 
                                                         cluster.original,
                                                         matrix.som.pts,
                                      path.map.cluster.nth.map.clustering)
    
    new.groups <- new.elements.clustering.consensus$new.groups
    cluster.new <- new.elements.clustering.consensus$cluster.new

    SomMapCluster(data.som, 
                                      path.map.cluster.nth.map.clustering, 
                                      groups=new.groups, 
                                      add.boundaries=T,
                                      add.samples=T,
                                      is.samples.colored=T,
                                      clusters=clusters,
                                      property=new.groups,
                                      title=
                                    "SOM_map_clusters_and_boundaries_consensus",
                                      nb.samples=nb.samples)
    
    SOMComputeFitToConsensus(new.groups,
                                           nb.clusters, 
                                       path.map.cluster.nth.map.clustering, 
                                           cluster.consensus,
                                           cluster.new,
                                           model.name,
                                           matrix.som.pts,
                                           nth.map,
                                           data.som=data.som)
  }
  
  for(nb.clusters in unlist(list.nb.clusters)){
    
    path.result <- file.path(path.maps,
                             "2_Consensus_analysis",
                             model.name,
                             sprintf("Cluster_%02d", nb.clusters))
    
    parallel::parLapply(parallel.run,
                        1:nb.SOM.maps,
                        .CreateConsensMapWorkflow,
                        nb.clusters,
                        model.name,
                        path.result,
                        path.maps,
                        nb.SOM.maps,
                        clustering.method)
  }
}
