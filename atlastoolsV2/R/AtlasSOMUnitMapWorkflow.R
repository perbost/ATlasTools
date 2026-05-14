#' @title Create and save SOM map as rds format and plot to analyse map.
#' @name AtlasSOMUnitMapWorkflow
#' @description Create Kohonen's self-organising maps from Kohonen R package.
#' This map is saved as .rds file in the define "som_save_path" destination.
#' Data in argument is usually model's Zmatrix with shape (n_sample, n_factors).
#' By default, parameters of SOM are already defined, but it is possible to
#' entire defined it by passed this arguments as dictionary.
#' Artificial data augmentation based on Gaussian noise is made to perform
#' Kohonen SOM algorithm. The data augmentation multiplication factor is
#' "factor.data.augmentation".
#' @param nth.map (int) current nth.map.
#' @param data rds file.
#' @param path.map.cluster path to save map for specific cluster number.
#' @param path.first.cluster path where maps with the first number of cluster 
#' to test was saved.
#' @param nb.cluster(int) number of cluster.
#' @param n.grid (int) size of SOM map.
#' @param regularized (bool) regularized data or not.
#' @param std (float) standard deviation.
#' @param factor.data.augmentation (int) factor augmentation for data
#'  augmentation.
#' @param first.cluster the first number of cluster to test.
#' @param decision.help.nb.cluster (bool) if we plot figure for help decision 
#' about best number of cluster to choose.
#' @param super.som (bool) if we use supersom instead som algorithm (default=F).
#' @param return.clusters (bool) if function return clusters result (default=F).
#' @param save.outputs (bool) if save the result (default=F).
#' @param map.analysis (bool) if we make analysis map (default=T).
#' @param clustering.method (str) name of clustering method to use
#' (default=hierarchical_clustering).
#' @export


AtlasSOMUnitMapWorkflow <- function(nth.map, data, path.map.cluster, 
                                    path.first.cluster, nb.cluster=NULL, 
                                    n.grid=20, regularized=FALSE, std=0.5, 
                                    factor.data.augmentation=20, 
                                    first.cluster=F, 
                                    decision.help.nb.cluster=F, super.som=F,
                                    return.clusters=F, save.outputs=T, 
                                    map.analysis=T,
                                    clustering.method=
                                      "hierarchical_clustering"){
  
  nb.samples <- nrow(data)
  path.map.cluster.nth.map <- file.path(path.map.cluster, 
                                        sprintf("Map_lin%03d", nth.map))
  
  path.path.map.cluster.nth.map.clustering <- 
    file.path(path.map.cluster.nth.map, clustering.method)
  
  if(dir.exists(path.path.map.cluster.nth.map.clustering)) 
    unlink(path.path.map.cluster.nth.map.clustering, recursive=TRUE)
  dir.create(path.path.map.cluster.nth.map.clustering, recursive=TRUE)
  
  # If we have already create SOM map for the first number of cluster, we load 
  # existing SOM map
  path.first.map.generated <- file.path(path.first.cluster,
                                        sprintf("Map_lin%03d",
                                                nth.map),"map.rds")
  if(!first.cluster && file.exists(path.first.map.generated)){
    data.som <- readRDS(path.first.map.generated)
  } else{
    nb.samples <- nrow(data)
    if(regularized){
      data <- t(t(data)/sqrt(colSums(data*data)))
      std <- sd(data)
    }
    
    data.noised <- data
    
    if(factor.data.augmentation>0){
      nb.rep.data.augmentation <-
        (floor(n.grid^2 / nb.samples) + 1) * factor.data.augmentation
      
      set.seed(nth.map)  
      
      for(i in 1:nb.rep.data.augmentation){
        data.noised = rbind(data.noised,
                            data + matrix(rnorm(n=nb.samples * ncol(data),
                                                sd=std), ncol=ncol(data)))
      }
    }
    
    data.som <- AtlasSOMUnitMapCreation(data.noised,
                                                      n.grid=n.grid,
                                                      radius=NULL,
                                                      topo="hexagonal", 
                                                      toroidal=F, 
                                                      maxNA.fraction=1, 
                                                      rlen=2000, 
                                                      alpha=c(0.5,0.01),
                                                      super.som=super.som)
  }
  path.save <- file.path(path.map.cluster.nth.map, "map.rds")
  if(ValidatePath(path.save)){
    saveRDS(data.som, path.save)
  }
    
  # we determine the sample groups according to the number of clusters:
  sample.groups <- 
    AtlasSOMCreateGroupCluster(data.som, 
                                      path.path.map.cluster.nth.map.clustering,
                                             nb.cluster=nb.cluster,
                                             decision.help.nb.cluster=
                                               decision.help.nb.cluster,
                                             clustering.method=
                                               clustering.method)
  
  if(map.analysis){
    # Basics plot SOM map analysis:
    AtlasSOMPrimaryAnalysis(
      data.som, 
      nb.samples, 
      nb.cluster, 
      path.path.map.cluster.nth.map.clustering,
      sample.groups=sample.groups)
  }
    
  clusters <- 
    data.frame(sample=rownames(data),
               code=data.som$unit.classif[1:nb.samples],
               cluster=sample.groups[paste0("V",
                                          data.som$unit.classif[1:nb.samples])],
               distance=data.som$distances[1:nb.samples])
  
  path.save <- file.path(path.path.map.cluster.nth.map.clustering, 
                         "Clusters.csv")
  
  if(ValidatePath(path.save)){
    write.csv2(clusters, file=path.save)
  }
    
  if(return.clusters)
    return(clusters)
}
