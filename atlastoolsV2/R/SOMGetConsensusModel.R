#' @title Create data frame with all concordance to consensus scores
#' @name SOMGetConsensusModel
#' @description This function stack all concordance to consensus scores of all
#' nth.map map for one model and return it. 
#' @param path.maps path to maps folder
#' @param model.name name of current model
#' @param nb.cluster (int) current number of cluster
#' @param nb.SOM.maps number of generated maps
#' @param clustering.method (str) clustering method used.
#' @return model.consensus
#' @export


SOMGetConsensusModel <- function(path.maps, model.name, nb.cluster, 
                                 nb.SOM.maps, clustering.method=
                                   "hierarchical_clustering"){
  
  model.consensus <- data.frame()
  
  path.cluster <-file.path(path.maps,
                           "1_Baseline_analysis",
                           model.name,
                           sprintf("Cluster_%02d", nb.cluster))

  path.df.model.consensus <- 
    file.path(path.cluster, 
              sprintf("df_fit_to_consensus__all_maps__%s.csv",
                      clustering.method))
  
  if(file.exists(path.df.model.consensus)){
    
    model.consensus <- readr::read_csv(path.df.model.consensus)
    
  } else{
    
    for(nth.map in 1:nb.SOM.maps){
      
      path.cluster.map <- file.path(path.cluster,
                                    sprintf("Map_lin%03d", nth.map))
      
      path.path.map.cluster.nth.map.clustering <- file.path(path.cluster.map,
                                                            clustering.method)
      
      df.fit.to.consensus <- 
        readr::read_delim(file.path(path.path.map.cluster.nth.map.clustering, 
                                    "df_fit_to_consensus.csv"), 
                          delim=";", 
                          escape_double=FALSE,
                          trim_ws=TRUE)
      
      df.fit.to.consensus$concordance_to_consensus <- 
        as.numeric(gsub(",",".", df.fit.to.consensus$concordance_to_consensus))
      
      model.consensus <- rbind(model.consensus, df.fit.to.consensus)
    }
    
    if(ValidatePath(path.df.model.consensus)){
      
      readr::write_csv(model.consensus, path.df.model.consensus)
      
    }
  }
  return(model.consensus)
}
