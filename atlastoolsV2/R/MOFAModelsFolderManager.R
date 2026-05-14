#' @title Manage creation and/or deletion of models directories to respect 
#' folder structure.
#' @name MOFAModelsFolderManager
#' @description 
#' But by default, we copy last existing models and outputs model analysis in 
#' folder called "old_model".
#' This function, can delete whole existing directories if 
#' 'Clear.models_results.existing' are True.
#' @param path.model (str) path to the folder of model to manage.
#' @param path.model.factor (str) path to the factor folder of current model
#' @param Clear.models_results.existing (bool) if True,  we delete every files
#' and object and directories in existing folder. If False, we copy all 
#' existing files, objects and directory in folder called "old_model".
#' @param model.name (str) name of the current model.
#' @export


MOFAModelsFolderManager <- function(path.model, path.model.factor, 
                                    Clear.models_results.existing, model.name){
  
  if(!dir.exists(path.model) || !dir.exists(path.model.factor)){
    if(!dir.exists(path.model.factor)) dir.create(path.model.factor,
                                                  recursive=T)
  }
  else{
    path.old <- file.path("models/old_last_models")
    path.old.model <- file.path(path.old, model.name)
    if(Clear.models_results.existing){
      unlink(path.model, recursive=TRUE)
      unlink(path.old.model, recursive=TRUE)
      dir.create(path.model.factor, recursive=T)
    }else{
      if(!dir.exists(path.old.model)) dir.create(path.old.model, recursive=T)
      # copy all existing files and folder in target directory:
      file.copy(from=path.model.factor, 
                to=path.old.model, 
                overwrite=TRUE, 
                recursive=TRUE)
      unlink(path.model.factor, recursive=TRUE, force=TRUE)
      dir.create(path.model.factor)
    }
  }
}
