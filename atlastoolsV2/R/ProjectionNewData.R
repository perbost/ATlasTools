#' @title Function to compute coefficient for new data projection.
#' @name ProjectionNewData
#' @description This function optimize coefficients to align new Z projection 
#' data with original Z reference projection.
#' To optimize coefficient, we have three normalization mode for coefficient:
#' With mode 1, we normalize coefficient with sum(coef**2). With mode 2, we 
#' normalize coefficient with sum(coef[1:length(Zproj)]), we don't take into
#' count intercept. And with mode 3, we normalize coefficient with 
#' sum(coef[1:length(Zproj)]**2), we don't take into count intercept.
#' If we have already compute this coefficients, this function can return 
#' new Z projection data frame computed from this coefficients.
#' @param data.new data frame with new data to be projected.
#' @param model the trained MOFA model to be used to project the new data.
#' @param path.model path to the model to save coef.
#' @param coef.optimized coeff to weight Z matrix.
#' @return Z.new.projection
#' @export


ProjectionNewData <- function(data.new, model, path.model, coef.optimized=NULL){
  
  
  .ProjectionOptimizationCoefficientsZ <- function(coef, data.train.original,
                                                   Zref, Zproj, views, 
                                                   delta.conv=1.e-6, 
                                                   coef.optimization=T,
                                                   coef.normalization.mode=1){
    
    # coefficients normalization:
    if(coef.normalization.mode == 1) 
      coef <- coef / sum(coef**2)
    if(coef.normalization.mode == 2) 
      coef[1:length(Zproj)] <- 
        coef[1:length(Zproj)] / sum(coef[1:length(Zproj)])
    if(coef.normalization.mode == 3) 
      coef[1:length(Zproj)] <- 
        coef[1:length(Zproj)] / sum(coef[1:length(Zproj)]**2)
    
    if(coef.optimization){
      
      Z.new.projection <- 0
      for(v in views){
        Z.new.projection <- Z.new.projection + Zproj[[v]] * coef[[v]]
      }
      
      Z.new.projection <- Z.new.projection + matrix(coef[["intercept"]],
                                                    nrow=dim(Zproj[[v]])[1],
                                                    ncol=dim(Zproj[[v]])[2])
      
      # compute error only between intersect rows because we don't have other
      # Z reference data
      error <- 
        sqrt(sum((Z.new.projection[intersect(rownames(Z.new.projection),
                                             rownames(Zref)),] 
                  - Zref[intersect(rownames(Z.new.projection),
                                   rownames(Zref)),])**2) / 
               ncol(Z.new.projection))
      
      print(sprintf("--- error:%4.4f ---", error))
    
    return(error) 
    } else{
      
      Z.new.projection <- 0
      for(v in views){
        Z.new.projection <- Z.new.projection + Zproj[[v]] * coef[[v]]
      }
      
      Z.new.projection <- Z.new.projection + matrix(coef[["intercept"]],
                                                    nrow=dim(Zproj[[v]])[1],
                                                    ncol=dim(Zproj[[v]])[2])
      
      return(Z.new.projection)
    }
  }
  

  views <- MOFA2:::.check_and_get_views(model, views="all", non_gaussian=F)
  group <- MOFA2:::.check_and_get_groups(model, groups="all")
  model.imputed <- MOFA2::impute(model)
  
  Z <- MOFA2::get_expectations(model, "Z")
  W <- MOFA2::get_expectations(model, "W")
  Z.reference <- Z[[group]][,]
  
  Zproj <- list()
  for(v in views){
    Zproj[[v]] <- t(data.new[[v]]) %*% MASS::ginv(t(W[[v]]))
    Zproj[[v]][is.na(Zproj[[v]])]<- 0
  }
  
  if(is.null(coef.optimized)){
    
    coef <- rep(1,length(views) + 1)
    names(coef) <- views
    names(coef)[length(views) + 1] <- "intercept"
    
    for(v in views){
      data.new[[v]][is.na(data.new[[v]])] <- 0
    }
    
    opt <- optim(coef,
                 .ProjectionOptimizationCoefficientsZ,
                 method="L-BFGS-B", 
                 lower=c(rep(0, length(views)), -10),
                 data.train.original=data.new, 
                 Zref=Z.reference,
                 Zproj=Zproj,
                 views=views,
                 delta.conv=1.e-6)
    
    coef.optimized  <- opt$par
    names(coef_optimized) <- c(names(Zproj), "intercept")
    
    readr::write_csv(as.data.frame(t(coef.optimized)),
                     file.path(path.model, "coef_optimized_intercept.csv"))
  }
  
  Z.new.projection <- .ProjectionOptimizationCoefficientsZ(coef.optimized,
                                                           data.train.original,
                                                           Z.reference,
                                                           Zproj,
                                                           views, 
                                                           coef.optimization=F)
  
  return(Z.new.projection)
}
