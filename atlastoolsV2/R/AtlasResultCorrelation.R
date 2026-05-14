#' @title High-level function designed to generate an Immunogram from target
#' clinical data.
#' @name AtlasResultCorrelation
#' @description This high-level function is designed to generate an Immunogram 
#' from the desired data.
#' Many options are available for calculating or retrieving interest feature to
#' generate the figure according to the chosen target.
#' Input is target clinical data as data frame and the values of all features
#' to compute correlation scores.
#' @param file.path (string) the csv path of data frame with 
#' saved clinical data probabilities prediction.
#' @param list.clinical.data list of names of clinical data for which 
#' probabilities are to be calculated. If we 
#' @param mapdata (rds file) large list with all views and for each view a data
#' frame with faeture values (float).
#' @param df.clinical.data.prediction.probability (data frame)
#' @export


AtlasResultCorrelation <- function(file.path, list.clinical.data=NULL,
                                   mapdata=NULL,
                                   df.clinical.data.prediction.probability=NULL)
  {
  
  # compute feature clinical data correlation scores for clinical data in 
  # list.name.clinical.data
  
  if(file.exists(file.path)){
    
    list.df.correlation_scores <- readRDS(file.path)
    
    # Check if clinical data name exist as column in the df
    if(all(list.clinical.data %in% names(list.df.correlation_scores))){
      return(list.df.correlation_scores)
    } else{
      list.clinical.data <- setdiff(list.clinical.data,
                                    names(list.df.correlation_scores))
    }
  }else{
    list.df.correlation_scores <- list()
  }
  
  message("... in progress...")
  for(clinical.data in list.clinical.data){
    df.correlation_scores <-
      AtlasComputeCorrelation(
        mapdata,
        unlist(df.clinical.data.prediction.probability[, clinical.data]))
    # we don't save because we bind results for any df clinical data result.
    cat("All correlations and Lasso coefficients computed for", clinical.data,
        "\n")
    list.df.correlation_scores[[clinical.data]] <- df.correlation_scores
  }
  saveRDS(list.df.correlation_scores, file.path)
  cat("list of df with results correlation scores are saved.")
  
  return(list.df.correlation_scores)
}

