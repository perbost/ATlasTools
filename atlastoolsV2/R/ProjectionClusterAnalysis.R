#' @title Analysis of cluster
#' @name ProjectionClusterAnalysis
#' @description Cluster analysis for current MOFA model.
#' @param nb.cluster (int) number of cluster.
#' @param path.maps path to the maps directory.
#' @param model.name (str) name of the current MOFA model to analyse.
#' @param nb.SOM.maps (int) number of generated SOM map.
#' @param dt.Atlas Atlas data frame data.
#' @param path.best (string) path to the folder to save best maps.
#' @param list.view pass a list of views to analyse. By default is "NULL"' (do 
#' analyse for all views).
#' @param threshold (float) threshold for select significant features regarding
#' p value.
#' @param p.adj.threshold (bool) to use adjusted p value.
#' @param log2 (bool) to add log fold change value.
#' @param all.feature.test (bool) if we compute all features test for cluster
#' analyse.
#' @param hierarchical.test (bool) if we compute hierarchical test for cluster
#' analysis.
#' @param plot.box.plot (bool) if plot box plot figures as pdf.
#' @param color.cluster color palette for each cluster.
#' @param clustering.method (str) clustering method used.
#' @export


ProjectionClusterAnalysis <- function(nb.cluster, path.maps, model.name, 
                                      nb.SOM.maps, dt.Atlas, 
                                      path.best="4_Best_maps_consensus",
                                      list.view=NULL, 
                                      threshold=0.05, p.adj.threshold=T, log2=F,
                                      all.feature.test=T, hierarchical.test=T,
                                      plot.box.plot=T, color.cluster=NULL,
                                      clustering.method=
                                        "hierarchical_clustering"){
  
  path.consensus <- file.path(path.maps,
                              "2_Consensus_analysis",
                              model.name,
                              sprintf("Cluster_%02d", nb.cluster))
  if(!dir.exists(path.consensus)) stop("No Consensus_analysis directory")
  
  path.best.maps <- file.path(path.maps,
                              path.best,
                              model.name,
                              sprintf("Cluster_%02d", nb.cluster))
  
  list.directory.best.maps <- list.dirs(path.best.maps,
                                        full.names=T,
                                        recursive=F)
  
  for(path.directory.cluster in list.directory.best.maps){
    
    path.cluster.analysis <- file.path(path.directory.cluster, 
                                       clustering.method,
                                       "Cluster_Analysis")
    if(!dir.exists(path.cluster.analysis)) dir.create(path.cluster.analysis)
    
    cluster.consensus <- 
      readr::read_csv(file.path(path.consensus, 
                                sprintf("clustering_consensus_%d_%s.csv",
                                        nb.SOM.maps, clustering.method)))

    if(!is.null(list.view)){
      
      # we add a specificity to the code here to change the working directory.
      # -> feature names to be saved can be too long to be handled by the OS.
      # To anticipate this, we put the working directory the deeper as possible
      # to shorten the path.
      # Then reset the working directory.
      # Store the current working directory
      original.working.directory <- getwd()
      # Set the new working directory
      setwd(path.cluster.analysis)
      shorter.path.cluster.analysis <- paste0(basename(path.cluster.analysis))
      
      for(view in list.view){
        ProjectionClusterAnalysisFeaturesTest(
          dt.Atlas,
          clusters=cluster.consensus,
          path.result=shorter.path.cluster.analysis,
          threshold=threshold,
          view=view,
          log2=log2,
          p.adj.threshold=p.adj.threshold,
          nb.cluster=nb.cluster,
          all.feature.test=all.feature.test,
          hierarchical.test=hierarchical.test,
          plot.box.plot=plot.box.plot,
          color.cluster=color.cluster)
      }
      # Revert back to the original working directory
      setwd(original.working.directory)
    }
  }
}
