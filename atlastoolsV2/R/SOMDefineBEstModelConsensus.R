#' @title Define best model consensus
#' @name SOMDefineBEstModelConsensus
#' @description This function allow to get the n first best map for the best 
#' model according concordance score.
#' We copy this best maps in folder best maps consensus.
#' @param path.maps path to the maps folder
#' @param model.name name of the current model
#' @param nb.SOM.maps number of generated maps
#' #' @param use.quantization.error (bool) if TRUE, we take into count the 
#' quantization error of the map. Quantization error measures how well the SOM 
#' represents the input data.
#' @param concordance.precision is the precision to sort best map for 
#' concordance score, i.e. we round all concordance values with this precision 
#' (To round a value to n digits after the decimal point, by default is 2) and
#' for all same score we sort regarding the quantization error.
#' @param file.name (str) name of the data frame to save with fit scores.
#' @param clustering.method (str) clustering method used.
#' @export


SOMDefineBEstModelConsensus <- function(path.maps, model.name, nb.SOM.maps,
                                        list.nb.clusters,
                                        use.quantization.error=T,
                                        concordance.precision=2, 
                                        file.name="Best_model.consensus.csv",
                                        clustering.method=
                                          "hierarchical_clustering"){
  
  for(nb.clusters in unlist(list.nb.clusters)){
    
    path.result <- file.path(path.maps,
                             "2_Consensus_analysis",
                             model.name,
                             sprintf("Cluster_%02d", nb.clusters))
    
    path.best.maps <- file.path(path.maps, 
                                "4_Best_maps_consensus",
                                model.name,
                                sprintf("Cluster_%02d", nb.clusters))
    if(!dir.exists(path.best.maps)) dir.create(path.best.maps, recursive=T)
    
    model.consensus <- SOMGetConsensusModel(path.maps,
                                                          model.name,
                                                          nb.clusters,
                                                          nb.SOM.maps,
                                                          clustering.method=
                                                            clustering.method)
    
    SOMSelectAndCopyBestMaps(model.consensus, 
                                           path.result,
                                           path.maps,
                                           path.best.maps,
                                           use.quantization.error=
                                             use.quantization.error, 
                                           concordance.precision=
                                             concordance.precision,
                                           file.name=file.name,
                                           clustering.method=
                                             clustering.method)
  }
}
