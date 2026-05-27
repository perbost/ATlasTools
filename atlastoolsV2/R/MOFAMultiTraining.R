#' @title Train MOFA model with different seed to find the best one.
#' @name MOFAMultiTraining
#' @description We use different initialization seed to train the best MOFA 
#' model with multi-omics dataframe.
#' We use and test different parameters values to find out the best model 
#' according our dataframe.
#' The best MOFA model is selected according ELBO score.
#' @param nb.factor (int) current number of factor for MOFA model to train.
#' @param data (data frame) multi-omics data set to train MOFA model
#' @param path.model (file.path) path to save trained model and analysis model
#' @param model.name (str) name of the current model
#' @param drop.factor (float) numeric indicating the threshold on fraction of
#' variance explained to consider a factor inactive and drop it from the model.
#' For example, a value of 0.01 implies that factors explaining less than 1% of
#' variance (in each view) will be dropped. Default is -1 (no dropping of 
#' factors)
#' @param keep.group (list) custom parameter to train MOFA model only on the 
#' subset of data belonging to the groups in the list. 
#' By default it is "NULL", which means that the MOFA model is trained on
#' the whole data set (all groups).
#' @param max.iteration (int) numeric value indicating the maximum number of
#' iterations. Default in this function is 15000. 
#' Convergence is assessed using the ELBO statistic.
#' @param MOFA.nb.seed (int) Number of different seed to test in order to find 
#' out the best MOFA model training initialization. By default (here) is 20.
#' @param scale.view (bool) if views have different ranges/variances, it is 
#' good practice to scale each view to unit variance. Default is TRUE here.
#' @param scale.group (bool) if groups have different ranges/variances, it is 
#' good practice to scale each group to unit variance. Default is TRUE here.
#' @param Clear.models_results.existing (bool) if True,  we delete every files
#' and object and directories in existing folder. If False, we copy all 
#' existing files, objects and directory in folder called "old_model".
#' @param not.denoise (list) list of view that should not be denoised.
#' @param show_colnames (bool) if TRUE show sample names on heatmaps.
#' @export


MOFAMultiTraining <- function(nb.factor, data,
                              path.model="models/unnamed_model", 
                              model.name=NULL, drop.factor=0.001, 
                              keep.group=NULL, max.iteration=15000, 
                              MOFA.nb.seed=20, scale.view=F, scale.group=F, 
                              Clear.models_results.existing=F,
                              not.denoise=NULL,
                              show_colnames=T){
  
  path.model.factor <- file.path(path.model, sprintf("Multi_%03d", nb.factor))
  MOFAModelsFolderManager(path.model,
                                      path.model.factor, 
                                      Clear.models_results.existing,
                                      model.name)
  
  MOFAModelSeedSearch(MOFA.nb.seed, 
                                    path.model.factor,
                                    data=data,
                                    drop.factor=drop.factor,
                                    factor.number=nb.factor,
                                    max.iteration=max.iteration, 
                                    keep.group=keep.group, 
                                    scale.view=scale.view, 
                                    scale.group=scale.group)

  file.rename(file.path(path.model.factor, "model_best.hdf5"), 
              file.path(path.model.factor, "model.hdf5"))
  
  model <- MOFA2::load_model(file.path(path.model.factor, "model.hdf5"))
  
  if(!is.null(keep.group)){  
    list.unique.group <- keep.group
  } else {
    list.unique.group <- unique(data$group)
  }
  model <- MOFA2::subset_groups(model, list.unique.group)
  
  MOFAModelFactorsAnalysis(file.path(path.model.factor, 
                                                   "model.hdf5"),
                                         path.model.factor,
                                         keep.group=keep.group,
                                         show_colnames=show_colnames)
  
  dir.create(file.path(path.model.factor, "Analysis"))
  
  MOFAModelAnalysis(model, 
                                  file.path(path.model.factor), 
                                  perform.GSEA=T,
                                  not.denoise=not.denoise,
                                  show_colnames=show_colnames)
  
  # export Z matrix from MOFA model:
  MOFAModelZmatrixExport(model, path.model.factor)
}
