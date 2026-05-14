#' @title 
#' @name AtlasLassoCorrelation
#' @description 
#' @param name description
#' @return 
#' @export


AtlasLassoCorrelation <- function (mapdata_, Y_, list_view){
  
  result_correl_lasso <- data.frame()
  
  for(view in list_view){
    
    data_tmp <- mapdata_[[view]]
    X <- t(data_tmp)
    # Penalty type (alpha=1 is lasso and alpha=0 is the ridge)
    cv.lambda.lasso <- glmnet::cv.glmnet(x=X, y=Y_, standardize=TRUE, alpha=1)
    # now get the coefs with the lambda found above
    l.lasso.min <- cv.lambda.lasso$lambda.min
    lasso.model <- glmnet::glmnet(x=X, y=Y_,
                                  alpha  = 1,
                                  lambda = l.lasso.min)
    scores_lasso <- lasso.model$beta
    nonZeroIdx <- which(scores_lasso[,1] != 0)
    features <- rownames(scores_lasso)[nonZeroIdx]
    features.coef <- scores_lasso[nonZeroIdx]
    names(features.coef) <- features
    features.coef <- features.coef[order(features.coef, decreasing = T)]
    for(f in names(features.coef)){
      result_correl_lasso <- rbind(result_correl_lasso,
                                   data.frame(view=view,
                                              feature=f,
                                              lasso_coef=features.coef[[f]]))
    }
  }
  return(result_correl_lasso)
}
