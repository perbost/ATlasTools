#' @title Feature projection on existing SOM maps
#' @name ProjectionFeature
#' @description We project feature values on existing map.
#' @param model MOFA model trained.
#' @param data.som SOM map codes matrix.
#' @param path.save path to save plot.
#' @param dt.feature data frame with feature values.
#' @param groups specify which groups should be kept in the model.
#' @param feature feature name to plot.
#' @export


ProjectionFeature <- function(model, data.som, path.save, dt.feature, groups, 
                              feature){
  
  # define rank color palette from min and max features values:
  color.rank <- 
    floor(abs(min(dt.feature)) / max(dt.feature) * 10)
  
  if(!is.nan(color.rank)){
    
    if(color.rank < 0) color.rank <- 1
    
    color.palette <-
      colorRampPalette(
        c(colorRampPalette(rev(RColorBrewer::brewer.pal(n=11,
                                name="RdYlBu")))(color.rank * 2)[1:color.rank],
          colorRampPalette(rev(RColorBrewer::brewer.pal(n=11,
                                                  name="RdYlBu")))(21)[11:20]))
    
    SomMapCluster(data.som, 
                                      path.save,
                                      property=dt.feature,
                                      groups=groups,
                                      add.boundaries=T,
                                      add.samples=F, 
                                      title=
                                        gsub(" ", "_", gsub("/","_", feature)),
                                      nb.colors=64,
                                      main=feature,
                                      color.palette=color.palette,
                                      nb.samples=
                                        length(
                                          model@samples_metadata$sample))
  }
}
