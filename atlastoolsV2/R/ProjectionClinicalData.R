#' @title Project Clinical data passed in argument in SOM map.
#' @name ProjectionClinicalData
#' @description This function project clinical data (as data frame) in SOM map
#' loaded.
#' @param nb.cluster (int) number of cluster of the current map.
#' @param dt.Atlas samples and features data frame.
#' @param dt.Atlas.clinical clinical data as data frame.
#' @param list.clinical.projection (list) list of clinical data to project on
#' the map.
#' @param path.maps path to the SOM map to loaded to project data.
#' @param model.name name of the current MOFA model to use.
#' @param nb.SOM.maps (int) number of SOM map generated.
#' @param nb.samples (description)int) number of sample.
#' @param clustering.method (str) clustering method used.
#' @export


ProjectionClinicalData <- function(nb.cluster, dt.Atlas, dt.Atlas.clinical, 
                                   list.clinical.projection, path.maps, 
                                   model.name, nb.SOM.maps, nb.samples,
                                   clustering.method=
                                     "hierarchical_clustering"){
  
  path.consensus.cluster <- file.path(path.maps,
                                      "2_Consensus_analysis",
                                      model.name,
                                      sprintf("Cluster_%02d", nb.cluster))
  
  path.best <- file.path(path.maps,
                         "4_Best_maps_consensus",
                         model.name,
                         sprintf("Cluster_%02d", nb.cluster))
  if(!dir.exists(path.best)) stop("No best_maps directory")
  
  list.directory.best.map <- list.dirs(path.best, 
                                       full.names=T, 
                                       recursive=F)
  
  for(path.best.map in list.directory.best.map){
    
    map.file <- file.path(path.best.map, "map.rds")
    data.som <- readRDS(map.file)
    
    groups.consensus <- readr::read_delim(file.path(path.best.map,
                                                    "groups_consensus.csv"), 
                                          delim=";", 
                                          escape_double=FALSE, 
                                          trim_ws=TRUE)
    groups <- t(groups.consensus[,"x"])[1,]
    names(groups) <- t(groups.consensus[,1])
    
    projected <- 
      readr::read_csv(file.path(path.consensus.cluster, 
                                sprintf("clustering_consensus_%d_%s.csv",
                                        nb.SOM.maps, clustering.method)))
  
    dt.projected <- merge(projected,
                          dt.Atlas.clinical,
                          by.x=c("sample"),
                          by.y=c("SAMPLE_ID"))
  
    ProjectionSOMData(dt.projected,
                                    data.som,
                                    path.best.map,
                                    dt.Atlas.clinical,
                                    list.clinical.projection,
                                    groups,
                                    nb.samples)
  }
  
}
