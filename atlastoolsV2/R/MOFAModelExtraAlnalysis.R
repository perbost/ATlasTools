#' @title additional plot to compare different phases model
#' @name MOFAModelExtraAlnalysis
#' @description we compare several factors models in different phase.
#' @param list.factor (lsit) list of factor number tested for MOFA mdoel.
#' @param path.model path to save generated plot
#' @param model_phase1_name path to model for the first phase
#' @param model_phase2_name path to model for the second phase
#' @export


MOFAModelExtraAlnalysis <- function(list.factor, path.model,
                                    model_phase1_name, model_phase2_name){
 
  tot_models <- c()
  model.names <- c()
  
  model.phase1 <- MOFA2::load_model(model_phase1_name)
  model.phase2 <- MOFA2::load_model(model_phase2_name)
  
  for(i in unlist(list.factor)){
    
    path.model.factor <- file.path(path.model, 
                                             sprintf("Multi_%03d", i))
    
    if(!dir.exists(path.model.factor)) next
    model.name <- file.path(path.model.factor, "model.hdf5")
    
    if(!file.exists(model.name)) next
    MofaObject <- MOFA2::load_model(model.name)
    
    path.analyse <- file.path(path.model.factor, "Extra_analyse")
    if(!dir.exists(path.analyse)) dir.create(path.analyse)
    
    it <- MOFA2::get_dimensions(MofaObject)$K
    
    phase3_names <- MOFA2::samples_names(MofaObject)
    phase3_names <- read.table(text=phase3_names[[1]], sep = "@")$V1
    phase3_names <- read.table(text=phase3_names, sep = "_")$V2
    newnames <- list(ATLAS=phase3_names)
    
    MOFA2::samples_names(MofaObject) <- newnames
    MOFA2::groups_names(MofaObject) <- "single_group"
    
    .ModelAnalysisPlot(MofaObject, 
                                          path.save=path.analyse, 
                                          plot.factor_grid=T,
                                          file.name=
                                            "factors_grid.pdf",
                                          width=14,
                                          height=14)
    
    .ModelAnalysisPlot(path.save=path.analyse, 
                                          plot.models_comparaison_factors=T,
                                          file.name=
                                        "comparaison_factors_phase1_phase2.pdf",
                                          width=12,
                                          height=10,
                                          models.compare=
                                            c(phase3=MofaObject,
                                              phase2=model.phase2,
                                              phase1=model.phase1))
    
    tot_models <- c(tot_models, MofaObject)
    model.names <- c(model.names,sprintf("Phase3_%d",it))
  }
  
  tot_models <- c(tot_models, model.phase1, model.phase2)
  model.names <- c(model.names, "Phase1","Phase2")
  names(tot_models) <- model.names
  
  .ModelAnalysisPlot(path.save=path.model, 
                                        plot.models_comparaison_factors=T,
                                        file.name=
                                      "comparaison_factors_phase1_phase2.pdf",
                                        width=18,
                                        height=16,
                                        models.compare=tot_models)
}
