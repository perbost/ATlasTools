#' @title Generate Primary SOM map analysis and save it as figure.
#' @name AtlasSOMPrimaryAnalysis
#' @description We build and save basics SOM map analysis as figure from the 
#' passed in argument MOFA model or we load it by path passed in argument.
#' @param data.som (rds format) SOM map.
#' @param nb.samples (int) number of sample data/
#' @param nb.cluster(int) number of cluster
#' @param path.map.cluster.nth.map path to the nth SOM map for specific MOFA
#' model.
#' @param sample.groups cluster sample group
#' @param hierarchical.clustering (bool) if we use hierarchical clustering 
#' algorithm
#' @param kmeans.clustering (bool) if we use kmeans clustering algorithm
#' @export


AtlasSOMPrimaryAnalysis <- function(data.som, nb.samples, nb.cluster, 
                                    path.map.cluster.nth.map, 
                                    sample.groups=NULL,
                                    hierarchical.clustering=F,
                                    kmeans.clustering=T){
  
  if(is.null(sample.groups)){
    # we determine the sample groups according to the number of clusters:
    sample.groups <- 
      AtlasSOMCreateGroupCluster(data.som,
                                               path.map.cluster.nth.map,
                                               nb.cluster=nb.cluster,
                                               hierarchical.clustering=
                                                 hierarchical.clustering,
                                               kmeans.clustering=
                                                 kmeans.clustering)
  }

  SomMapCodes(data.som, 
                                  path.map.cluster.nth.map,
                                  as.pdf=T, 
                                  as.png=F, 
                                  file.name="SOM_factors_codes", 
                                  title="Codes",
                                  width=7, height=7)
  
  SomMapCluster(data.som,
                                    path.plot.save=path.map.cluster.nth.map,
                                    groups=sample.groups,
                                    add.boundaries=T, 
                                    add.samples=T,
                                    type="mapping", 
                                    property=sample.groups,
                                    main="SOM augmented data samples",
                                    title="SOM_augmented_data_samples", 
                                    nb.samples=nb.samples)
  
  SomMapCluster(data.som,
                                    path.plot.save=path.map.cluster.nth.map,
                                    groups=sample.groups,
                                    add.boundaries=T, 
                                    add.samples=T,
                                    type="dist.neighbours", 
                                    property=sample.groups,
                                    main="SOM distance neighbours",
                                    title="SOM_distance_neighbours", 
                                    nb.samples=nb.samples)
  
  SomMapCluster(data.som, 
                                    path.plot.save=path.map.cluster.nth.map,
                                    groups=sample.groups,
                                    add.boundaries=T,
                                    add.samples=T,
                                    type="count",
                                    property=sample.groups,
                                    main="SOM node counts",
                                    title="SOM_node_counts", 
                                    nb.samples=nb.samples)
  
  SomMapCluster(data.som, 
                                    path.plot.save=path.map.cluster.nth.map,
                                    groups=sample.groups,
                                    add.boundaries=T, add.samples=T,
                                    property=sample.groups,
                                    main="SOM samples projection",
                                    title="SOM_samples_projection", 
                                    nb.samples=nb.samples)
  
  SomMapCluster(data.som, 
                                    path.plot.save=path.map.cluster.nth.map,
                                    groups=sample.groups,
                                    add.boundaries=T, 
                                    add.samples=T,
                                    type="changes", property=sample.groups,
                                    main="SOM changes",
                                    title="SOM_changes", 
                                    nb.samples=nb.samples)
  
  SomMapCluster(data.som, 
                                    path.plot.save=path.map.cluster.nth.map,
                                    groups=sample.groups,
                                    add.boundaries=T, 
                                    add.samples=T,
                                    type="quality", property=sample.groups,
                                    main="SOM quality",
                                    title="SOM_quality", 
                                    nb.samples=nb.samples)
}
