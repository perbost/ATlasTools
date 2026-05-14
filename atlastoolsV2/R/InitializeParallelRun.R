#' @title Set up parallel settings for computation.
#' @name InitializeParallelRun
#' @description Function to automatically allocated n percent of available
#' cores to parallel running.
#' By default we use 80% of available cores. If max=T, we use all available
#' cores except one.
#' @param n (float): percent of available cores
#' @param max (bool) if we use th emaximum ressources allowed.
#' @return makeCluster(nb_cores_to_use)
#' @export


InitializeParallelRun <- function(n=0.8, max=F){
  
  nb_cores_available <- parallel::detectCores() - 1
  if(max || n>=1) nb_cores_to_use <- nb_cores_available
  else{
    nb_cores_to_use <- floor(n*parallel::detectCores())
  }
  if(nb_cores_to_use == parallel::detectCores()){
    nb_cores_to_use <- nb_cores_to_use - 1
  }

  parallel.run <- parallel::makeCluster(nb_cores_to_use)
  .AtlasConfigureParallelCluster(parallel.run)

  return(parallel.run)
}
