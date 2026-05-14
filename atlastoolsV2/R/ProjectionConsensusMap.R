#' @title Create SOM maps with feature values
#' @name ProjectionConsensusMap
#' @description We create SOM map with feature values.
#' @param parallel.run config to run script in parallel
#' @param nb.cluster (int) number of cluster
#' @param path.maps maps path on which to project feature values
#' @param model.name name of the current analysed model
#' @param path.model.factor path to the model for a specific factor number.
#' @param path.best (string) path to the folder to save best maps.
#' @param keep.groups (list) list of dat agroups to keep dor analysis.
#' @param groups (list) list of group to which we want the data to belong
#' @param regularized (bool) if we regularized data.
#' @param show.samples (bool) if we plot projection sample in the figure.
#' @param all.features (bool) if we do analysis for all features
#' @param add.intercept (bool) if add intercept (default=T).
#' @param delete.existing.mapdata.folder (bool) if we want delete existing 
#' mapdata folder to replace all values by new one.
#' @param clustering.method (str) clustering method used.
#' algorithm (default=T)
#' @export


ProjectionConsensusMap <- function(parallel.run, nb.cluster, path.maps, 
                                   model.name, path.model.factor, 
                                   path.best="4_Best_maps_consensus",
                                   keep.groups=NULL, groups=NULL, regularized=T,
                                   show.samples=F, all.features=T, 
                                   add.intercept=T, 
                                   delete.existing.mapdata.folder=F,
                                   clustering.method="hierarchical_clustering"){
  
  .WorkflowProjectionConsensusMap <- function(path.directory.cluster, model,
                                              nb.cluster, keep.groups, 
                                              regularized, show.samples,
                                              all.features, add.intercept){
    
    path.map.som <- file.path(path.directory.cluster, "map.rds")
    map <- readRDS(path.map.som)
    
    groups.consensus <- readr::read_delim(file.path(path.directory.cluster,
                                                    clustering.method,
                                                    "groups_consensus.csv"), 
                                          delim=";",
                                          escape_double=FALSE,
                                          trim_ws=TRUE)
    
    groups <- t(groups.consensus[,"x"])[1,]
    names(groups) <- t(groups.consensus[,1])
    
    ProjectionAllFeatures(path.directory.cluster,
                                        map,
                                        model,
                                        nb.cluster=nb.cluster,
                                        keep.groups=keep.groups, 
                                        groups=groups, 
                                        regularized=regularized, 
                                        show.samples=show.samples,
                                        all.features=all.features,
                                        add.intercept=add.intercept,
                                        delete.existing.mapdata.folder=
                                          delete.existing.mapdata.folder,
                                        clustering.method=clustering.method)
  }
  
  path.consensus <- file.path(path.maps,
                              path.best,
                              model.name,
                              sprintf("Cluster_%02d", nb.cluster))
  if(!dir.exists(path.consensus)) stop("No best maps directory")
  
  list.direcoty.consensus <- list.dirs(path.consensus,
                                       full.names=T,
                                       recursive=F)
  
  model <- MOFA2::load_model(file.path(path.model.factor,
                                       "model.hdf5"))
  
  parallel::parLapply(parallel.run,
                      unlist(list.direcoty.consensus),
                      .WorkflowProjectionConsensusMap,
                      model, 
                      nb.cluster,
                      keep.groups, 
                      regularized, 
                      show.samples,
                      all.features, 
                      add.intercept)
}
