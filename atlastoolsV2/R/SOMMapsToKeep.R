#' @title Load and keep best maps based on early computed scores
#' @name SOMMapsToKeep
#' @description This function return the nb.best.maps.to.keep first maps 
#' with the best scores.
#' We keep n_score * nb.best.maps.to.keep maps (nb.best.maps.to.keep maps for 
#' "diag" score and nb.best.maps.to.keep maps for "score" score).
#' @param path.result path to result folder
#' @param nb.SOM.maps (int) number of generated maps
#' @param nb.best.maps.to.keep (int) number of the first best maps to keep
#' @param keep.baseline.maps (bool) if we return best map according baseline
#' score.
#' @param keep.ARI.maps (bool) if we return best map according ARI score.
#' @return keep.maps
#' @export


SOMMapsToKeep <- function(path.result, nb.SOM.maps, nb.best.maps.to.keep,
                          keep.baseline.maps=F, keep.ARI.maps=T){

  keep.maps <- c()
  
  if(keep.baseline.maps){
    
    best.maps.csv <- readr::read_csv(file.path(path.result,
                                               sprintf("best_map_%02d.csv",
                                                       nb.SOM.maps)))
    best.maps.diag <- best.maps.csv[order(best.maps.csv$diag, decreasing=T),]
    keep.maps <- union(keep.maps, best.maps.diag[1:nb.best.maps.to.keep,]$map)
    
    best.maps.score <- best.maps.csv[order(best.maps.csv$score, decreasing=T),]
    keep.maps <- union(keep.maps, best.maps.score[1:nb.best.maps.to.keep,]$map)
  }
  
  if(keep.ARI.maps){
   
    best.maps.ARI.csv <- readr::read_csv(file.path(path.result,
                                              sprintf("best_map_ARI_%02d.csv",
                                                      nb.SOM.maps)))
    best.maps.ARI <- best.maps.ARI.csv[order(best.maps.ARI.csv$ARI_score,
                                             decreasing=T),]
    keep.maps <- union(keep.maps, 
                           best.maps.ARI[1:nb.best.maps.to.keep,]$map)
  }
  return(keep.maps)
}
