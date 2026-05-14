#' @title projection of features on existing SOM maps
#' @name ProjectionAllFeatures
#' @description We project feature values on existing map.
#' @param path.directory path to current directory where save outputs
#' @param data.som SOM map codes matrix.
#' @param model MOFA model trained
#' @param nb.cluster (int) number of cluster
#' @param keep.groups specify which views should be kept in the model.
#' This parameter takes a vector of integers representing the indices of the 
#' groups that should be kept.
#' By default, if keep_group = NULL, all groups are kept in the model. 
#' @param groups specify which groups should be kept in the model.
#' @param regularized (bool) if we regularize or not data
#' @param show.samples (bool) if show sample on generated plot
#' @param all.features (bool) if we do analysis for all features
#' @param add.intercept (bool) if we add intercept
#' @param delete.existing.mapdata.folder (bool) if we want delete existing 
#' mapdata folder to replace all values by new one.
#' @param clustering.method (str) clustering method used.
#' algorithm (default=T)
#' @export


ProjectionAllFeatures <- function(path.directory, data.som, model, nb.cluster,
                                  keep.groups=NULL, groups=NULL, regularized=T,
                                  show.samples=F, all.features=T, 
                                  add.intercept=T, 
                                  delete.existing.mapdata.folder=F,
                                  clustering.method="hierarchical_clustering"){
  
  path.mapdata <- file.path(path.directory, "mapdata")
  
  if(delete.existing.mapdata.folder){
    if(dir.exists(path.mapdata)) unlink(path.mapdata, recursive=TRUE)
    dir.create(path.mapdata)
  }
  
  if(!is.null(keep.groups)){
    model <- MOFA2::subset_groups(model, keep.groups)
  }
  
  if(is.null(groups)){
    groups <- 
      AtlasSOMCreateGroupCluster(data.som,
                                               path.directory,
                                               nb.cluster=nb.cluster,
                                               clustering.method=
                                                 clustering.method)
    }
  
  data.som.codes <- data.som$codes[[1]]
  data.som.features.data <- 
    ProjectionMapToView(data.som.codes,
                                      model,
                                      regularized=regularized,
                                      add.intercept=add.intercept)
  
  saveRDS(data.som.features.data, file.path(path.mapdata, "mapdata.rds"))
  
  if(all.features){
    for(v in names(data.som.features.data)){
      if(v %in% c("WES","NGS_WES","Clinical")) next
      
      data.tmp <- data.som.features.data[[v]]
      path.mapdata.view <- file.path(path.mapdata, v)
      dir.create(path.mapdata.view)
      
      for(feature in rownames(data.tmp)){
  
        ProjectionFeature(model, data.som, path.mapdata.view, 
                                        data.tmp[feature,], groups, feature)
          
      }
    }
  }
}
