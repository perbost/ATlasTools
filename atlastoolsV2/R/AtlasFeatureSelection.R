#' @title Select top features based on chosen correlation scores.
#' @name AtlasFeatureSelection
#' @description We select n top features based on correlation scores between
#' all features and target clinical data.
#' @param df.results.correlation (data frame) correation scores for each
#' features and method.
#' @param clinical.data (string) target clinical data name. 
#' @param chosen_methods (string) chosen name of correlation method.
#' @param n (int) number of top features to return.
#' @return data frame containing best features
#' @export


AtlasFeatureSelection <- function(df.results.correlation, clinical.data, 
                                  chosen_methods, n=10) {
  
  if(!(chosen_methods %in% 
       names(df.results.correlation[[clinical.data]][[1]]))){
    stop(paste("Error: Variable", method, "does not exist in the data frame."))
  }
  
  correlation_result <- df.results.correlation[[clinical.data]]
  
  # Print warnings or information for chosen methods
  for(method in chosen_methods) {
    message <- switch(method,
                      pearson = "Watch out for linear relationships only.",
                      spearman = "Robust to outliers, but assumes monotonic
                      relationship.",
                      kendall = "Robust and suitable for small sample sizes.",
                      combined = "Combined score is experimental, interpret
                      with caution.",
                      lasso = "Lasso regression includes feature selection,
                      interpret with context.")
    cat(paste("Warning for", method, ":", message, "\n"))
  }
  
  top_features_and_scores <- lapply(correlation_result, function(df) {
    df <- df[order(-df[[chosen_methods]]), ] # Order by the method column
    top_n_rows <- head(df[, c("feature", chosen_methods)], n)
    # Get the top n features and method scores
    return(top_n_rows)
  })
  
  return(top_features_and_scores)
}
