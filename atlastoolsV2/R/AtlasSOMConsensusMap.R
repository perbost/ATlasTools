#' @title High level function to analyze created maps.
#' @name AtlasSOMConsensusMap
#' @description High level function to analyse all created maps
#' @param nb.cluster (int) number of cluster to test.
#' @param path.maps.baseline path to baseline maps folder
#' @param path.maps.consensus path to consensus maps folder
#' @param nb.SOM.maps number of maps to generate
#' @param nb.samples
#' @param model.name.factor model name with number of factors.
#' @param clustering.method (str) clustering method used.
#' @param compare.models (bool) if we compare two models factors.
#' @param path.models.compare path to the models to compare.
#' @param row.names.remove.constante (bool) if we want clean row names by 
#' removing all constant nomenclature.
#' @export


AtlasSOMConsensusMap <- function(nb.cluster, path.maps.baseline, 
                                 path.maps.consensus, nb.SOM.maps, nb.samples,
                                 model.name.factor, 
                                 clustering.method="hierarchical_clustering",
                                 compare.models=F,
                                 path.models.compare=NULL,
                                 row.names.remove.constante=T){
  
  path.maps.consensus.cluster <- file.path(path.maps.consensus,
                                           sprintf("Cluster_%02d", nb.cluster))
  if(!dir.exists(path.maps.consensus.cluster)) 
    dir.create(path.maps.consensus.cluster, recursive=T)
  
  path.maps.baseline.cluster <- file.path(path.maps.baseline,
                                       sprintf("Cluster_%02d", nb.cluster))
  
  global.clustering.matrix <- 
    AtlasSOMAllClusteringMatrixAgreement(
      path.maps.consensus.cluster,
      path.maps.baseline.cluster,
      nb.SOM.maps,
      nb.cluster,
      nb.samples,
      compare.models=compare.models,
      path.models.compare=
        path.models.compare,
      row.names.remove.constante=row.names.remove.constante,
      clustering.method=clustering.method)

  jaccard.distances.all.clusters <- 
    prabclus::jaccard(t(global.clustering.matrix))
  
  clustering.consensus.outputs <- 
    AtlasSOMComputeClusteringConsensus(
      jaccard.distances.all.clusters,
      path.maps.consensus.cluster,
      nb.cluster,
      nb.SOM.maps,
      clustering.method=clustering.method)
}
