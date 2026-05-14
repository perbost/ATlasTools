#' @title High level function to create all map.rds for model analysis
#' @name AtlasSOMAllMapsWorkflow
#' @description High level function to create all map.rds and pdfs (codes map, 
#' clusters map). We do that for the current model passed in argument.
#' @param nb.factor (int) number of factor for the current MOFA
#' @param path.maps path to maps folder
#' @param path.model.factor path to the model directory
#' @param model.name name of the current model
#' @param nb.SOM.maps number of maps to generate
#' @param first.number.map (int) default=1, if the first seed to test
#' @param parallel.run config to run script in parallel
#' @param regularized (bool) regularized data or not
#' @param n.grid (int) size of SOM map
#' @param std (float) standard deviation
#' @param list.nb.clusters (list) list of all cluster to test
#' @param outliers.substitution (bool) to reduce outliers or not
#' @param factor.data.augmentation (int) factor augmentation for data 
#' augmentation
#' @param decision.help.nb.cluster (bool) if we plot figure for help decision 
#' about best number of cluster to choose.
#' @export


AtlasSOMAllMapsWorkflow <- function(nb.factor, path.maps, path.model.factor,
                                    model.name, nb.SOM.maps=1000,
                                    first.number.map=1, parallel.run=NULL, 
                                    regularized=F, n.grid=20, std=0.5, 
                                    list.nb.clusters=NULL, 
                                    outliers.substitution=T, 
                                    factor.data.augmentation=20,
                                    decision.help.nb.cluster=F, super.som=F,
                                    clustering.method="hierarchical_clustering",
                                    first.nb.cluster.SOM.generated=3){

  data <- readRDS(file.path(path.model.factor, "Zmatrix.rds"))

  if(outliers.substitution) data <-
    AtlasSOMDataOutliersSubstitution(data)

  path.maps.save <- file.path(path.maps,
                              "1_Baseline_analysis",
                              model.name,
                              sprintf("Multi_%03d", nb.factor))
  if(!dir.exists(path.maps.save)) dir.create(path.maps.save, recursive=T)

  for(nb.cluster in unlist(list.nb.clusters)){
    
    path.map.cluster <- file.path(path.maps.save,
                                  sprintf("Cluster_%02d", nb.cluster))
    if(!dir.exists(path.map.cluster)) dir.create(path.map.cluster)
    
    if(nb.cluster == first.nb.cluster.SOM.generated){
      first.cluster <- T
    } else first.cluster <- F

    parallel::parLapply(parallel.run,
                        first.number.map:nb.SOM.maps,
                        AtlasSOMUnitMapWorkflow,
                        data,
                        path.map.cluster = path.map.cluster,
                        path.first.cluster =
                          file.path(path.maps.save,
                                    sprintf("Cluster_%02d",
                                            first.nb.cluster.SOM.generated)),
                        nb.cluster=nb.cluster,
                        n.grid=n.grid,
                        regularized=regularized,
                        std=std,
                        factor.data.augmentation=factor.data.augmentation,
                        first.cluster=first.cluster,
                        decision.help.nb.cluster=decision.help.nb.cluster,
                        super.som=super.som,
                        clustering.method=clustering.method)
  }
}
