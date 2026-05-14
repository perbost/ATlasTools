#' @title Train n models and keep the best one according the ELBO score.
#' @name MOFAModelSeedSearch
#' @description We Train n MOFA models with different initialization seed.
#' Then we compare ELBO score between different MOFA model and we keep
#' the best one regarding the ELBO score.
#' @param nb.seed (int) Number of different seed to test in order to find out 
#' the best MOFA model training initialization.
#' @param path.model.factor (file.path) path to save model with specific factor
#' @param data data frame on which to train the MOFA model
#' @param drop.factor (float) numeric indicating the threshold on fraction of
#'  variance explained to consider a factor inactive and drop it from the model.
#'  For example, a value of 0.01 implies that factors explaining less than 1% of
#'  variance (in each view) will be dropped. Default is -1 (no dropping of 
#'  factors)
#' @param factor.number (int) current number of factors for MOFA model
#' @param max.iteration (int) numeric value indicating the maximum number of
#' iterations. Default in this function is 15000. 
#' Convergence is assessed using the ELBO statistic
#' @param keep.group (list) custom parameter to train MOFA model only on the 
#' subset of data belonging to the groups in the list. 
#' By default it is "NULL", which means that the MOFA model is trained on
#' the whole data set (all groups).
#' @param scale.view (bool) if views have different ranges/variances, it is 
#' good practice to scale each view to unit variance. Default is TRUE here.
#' @param scale.group (bool)  if groups have different ranges/variances, it is 
#' good practice to scale each group to unit variance. Default is TRUE here.
#' @export


MOFAModelSeedSearch <- function(nb.seed, path.model.factor, data=NULL, 
                                drop.factor=NULL, factor.number=20,
                                max.iteration=15000, keep.group=NULL, 
                                scale.view=F, scale.group=F){
  
  for(iseed in seq(1, nb.seed)){
    
    MOFAModelTraining(data, 
                                    path.model.factor,
                                    drop.factor=drop.factor, 
                                    factor.number=factor.number, 
                                    max.iteration=max.iteration, 
                                    keep.group=keep.group, 
                                    scale.view=scale.view, 
                                    scale.group=scale.group,
                                    seed=iseed)
    
    if(iseed == 1){
      file.rename(file.path(path.model.factor, "model.hdf5"), 
                  file.path(path.model.factor, "model_best.hdf5"))
      write(iseed, file.path(path.model.factor, "iseed"))
    }
    else{
      model <- MOFA2::load_model(file.path(path.model.factor, "model.hdf5"))
      elbo <- MOFA2::get_elbo(model)
      
      model_best <- MOFA2::load_model(file.path(path.model.factor,
                                                "model_best.hdf5"))
      elbo_best <- MOFA2::get_elbo(model_best)
      
      if(elbo > elbo_best){
        write(iseed, file.path(path.model.factor, "iseed"))
        file.remove(file.path(path.model.factor,"model_best.hdf5"))
        file.rename(file.path(path.model.factor,"model.hdf5"),
                    file.path(path.model.factor,"model_best.hdf5"))
      }
      if(file.exists(file.path(path.model.factor, "model.hdf5")))
        file.remove(file.path(path.model.factor,"model.hdf5"))
    }
  }
}
