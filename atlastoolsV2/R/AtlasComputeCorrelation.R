#' @title Compute correlation score between features and clinical data outcomes
#' @name AtlasComputeCorrelation
#' @description We compute multiple correlation scores between features and
#' clinical value outcomes. By default, we compute "pearson", "spearman", 
#' "kendall" and "combined". Note that combined score is a experimental custom
#' score.
#' @param data (list) list of data frame containing all feature values.
#' @param clinical_data (list) string list of the clinical data to compute 
#' correlation scores.
#' @return list of data frame containing correlation scores
#' @export


AtlasComputeCorrelation <- function(data, clinical_data){
  
  require(dplyr)
  
  all_methods <- c("pearson", "spearman", "kendall", "combined")
  cor.methods = c("pearson", "spearman", "kendall")
  list_of_dfs <- list()
  
  for(view in names(data)){
    
    data.view <- data[[view]]
    
    # Initialize a matrix to store the correlation values
    correlations <- matrix(0, nrow = nrow(data.view), ncol=length(all_methods))
    colnames(correlations) <- all_methods
    
    # Compute the correlation for each row using each method
    for(feature in 1:nrow(data.view)) {
      row_data <- data.view[feature, ]
      for(method in cor.methods) {
        correlations[feature, method] <- cor(row_data,
                                             clinical_data,
                                             method = method)
      }
    }
    df.correlations <- as.data.frame(correlations)
    
    # Adding the new columns as the first and second columns
    df.correlations <- df.correlations %>%
      mutate(view = view,
             feature = row.names(data.view)) %>%
      select(view, feature, everything())
    
    # compute combined score
    df.correlations[, "combined"] <-
      apply(df.correlations[, cor.methods]^2, 1, prod)
    
    # combine data frames
    list_of_dfs[[view]] <- df.correlations
  }
  
  # compute lasso correlation:
  result_lasso <-
    AtlasLassoCorrelation(data, clinical_data, names(data))
  
  # combine lasso result with list of data frame result
  for(view_name in names(list_of_dfs)){
    # Extract the DataFrame from the list
    current_df <- list_of_dfs[[view_name]]
    # Extract the rows of df_to_merge that correspond to the current view
    subset_to_merge <- result_lasso[result_lasso$view == view_name,
                                    c("feature", "lasso_coef")]
    # Merge by feature, only updating the lasso column
    current_df <- merge(current_df,
                        subset_to_merge,
                        by = "feature",
                        all.x = TRUE)
    # replace NA value by 0
    current_df$lasso_coef[is.na(current_df$lasso_coef)] <- 0
    list_of_dfs[[view_name]] <- current_df
  }
  return(list_of_dfs)
}
