#' @title return predicted data from the MOFA model trained
#' @name ProjectionMapToView
#' @description return predicted data from the current MOFA model trained.
#' @param data.som.codes matrix of codes for each current SOM map.
#' @param model current MOFA model trained.
#' @param regularized (bool) if Z matrix data are regularized or not.
#' @param views (list) list of views to which we want the data to belong
#' @param groups (list) list of group to which we want the data to belong
#' @param factors (list) list of factors to which we want the data to belong
#' @param add.intercept (bool) if add intercept
#' @return predicted.data
#' @export


ProjectionMapToView <- function(data.som.codes, model, regularized=F, 
                                views="all", groups="all", factors="all",
                                add.intercept=T){
  
  if(!is(model, "MOFA")) 
    stop("'model' has to be an instance of MOFA")

  views <- MOFA2:::.check_and_get_views(model, views, non_gaussian=FALSE)
  groups <- MOFA2:::.check_and_get_groups(model, groups)
  
  if(any(views %in% names(which(model@model_options$likelihoods != 
                                "gaussian")))) 
    stop("predict does not work for non-gaussian modalities")
  
  if(paste0(factors, collapse = "") == "all"){
    factors <- MOFA2::factors_names(model)
  }
  else if(is.numeric(factors)){
    factors <- MOFA2::factors_names(model)[factors]
  }
  else stopifnot(all(factors %in% MOFA2::factors_names(model)))
  
  W <- MOFA2::get_weights(model, views=views, factors=factors)
  Z <- data.som.codes

  if(regularized){
    dbtb = model@expectations$Z[[1]]
    Z=t(t(Z)*sqrt(colSums(dbtb*dbtb)))
  }
  
  Z[is.na(Z)] <- 0
  
  predicted.data <- lapply(views, function(m){
    pred <- t(Z %*% t(W[[m]]))
    tryCatch({
      if (add.intercept & length(model@intercepts[[1]]) > 
          0) {
        intercepts <- model@intercepts[[m]][[g]]
        intercepts[is.na(intercepts)] <- 0
        pred <- pred + model@intercepts[[m]][[g]]
      }
    }, error = function(e) {
      NULL
    })
    return(pred)
  })
  predicted.data <- MOFA2:::.name_views_and_groups(predicted.data, 
                                                   views, 
                                                   groups)
  return(predicted.data)
}
