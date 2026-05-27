#' @title Train MOFA models with MOFA option.
#' @name MOFAModelTraining
#' @description We define all data, model, training options and we train MOFA
#' models.
#' @param data (data frame) multi-omics data set to train MOFA model
#' @param path (file.path) MOFA models and outputs directory path
#' @param drop.factor (float) numeric indicating the threshold on fraction of
#'  variance explained to consider a factor inactive and drop it from the model.
#'  For example, a value of 0.01 implies that factors explaining less than 1% of
#'  variance (in each view) will be dropped. Default is -1 (no dropping of 
#'  factors)
#' @param mode.convergence mode of convergence.
#' @param max.iteration (int) numeric value indicating the maximum number of
#' iterations. Default in this function is 15000. 
#' Convergence is assessed using the ELBO statistic
#' @param factor.number (int) current number of factors for MOFA model
#' @param scale.view (bool) if views have different ranges/variances, it is 
#' good practice to scale each view to unit variance. Default is TRUE here.
#' @param scale.group (bool)  if groups have different ranges/variances, it is 
#' good practice to scale each group to unit variance. Default is TRUE here.
#' @param keep.group (list) custom parameter to train MOFA model only on the 
#' subset of data belonging to the groups in the list. 
#' By default it is "NULL", which means that the MOFA model is trained on
#' the whole data set (all groups).
#' @param seed (int) current seed value for MOFA training initialization
#' @export


MOFAModelTraining <- function(data, path, drop.factor=0.001, 
                              mode.convergence="slow", max.iteration=15000,
                              factor.number=13, scale.view=F, scale.group=F,
                              keep.group=NULL, seed=1){
       
  if(!is.null(keep.group)){  
    MOFAobject <- MOFA2::create_mofa(data[data$group %in% keep.group,])
  } else {
    MOFAobject <- MOFA2::create_mofa(data)
  }
  Sys.sleep(5) # waiting time to give the system time to create the MOFA object.
  
  .ModelAnalysisPlot(MofaObject=MOFAobject, 
                                        path.save=path, 
                                        plot.data.overview=T,
                                        file.name="Data_overview.pdf")

  # Configure Data options:
  data_opts <- MOFA2::get_default_data_options(MOFAobject)
  data_opts$scale_views <- scale.view
  data_opts$scale_groups <- scale.group
  
  # Configure Model options:
  model_opts <- MOFA2::get_default_model_options(MOFAobject)
  model_opts$num_factors <- factor.number
  
  # get Bernoulli distribution for binary data:
  if("NGS_WES" %in% names(model_opts$likelihoods)) 
    model_opts$likelihoods["NGS_WES"] <- "bernoulli"  
  if("WES" %in% names(model_opts$likelihoods))
    model_opts$likelihoods["WES"] <- "bernoulli"
  if("Clinical" %in% names(model_opts$likelihoods))
    model_opts$likelihoods["Clinical"] <- "bernoulli"
  
  # Configure Train options:
  train_opts <- MOFA2::get_default_training_options(MOFAobject)
  train_opts$seed <- seed
  train_opts$maxiter <- max.iteration
  train_opts$convergence_mode <- mode.convergence
  train_opts$drop_factor_threshold <- drop.factor
  train_opts$freqELBO <- 1
  
  # Prepare MOFA:
  MOFAobject <- MOFA2::prepare_mofa(object=MOFAobject,
                                    data_options=data_opts,
                                    model_options=model_opts,
                                    training_options=train_opts)
  
  # Train MOFA:
  outfile = file.path(path, "model.hdf5")
  MOFAobject.trained <- MOFA2::run_mofa(MOFAobject,
                                        use_basilisk=TRUE,
                                        outfile)
  Sys.sleep(5) # waiting time
}
