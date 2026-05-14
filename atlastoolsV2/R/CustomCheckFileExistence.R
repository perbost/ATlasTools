#' @title Check if file exist with custom Atlas assistance
#' @name CustomCheckFileExistence
#' @description This function check if file exist and depending of request,
#' custom help is provide.
#' If we check directory existence, by default if is missing we create it.
#' e.g.: if we check existence of specific file and this file is missing,
#' help assistance will provide the function name to be run to generate missing
#' file.
#' @param path_to_check path to the file to check existence.
#' @param create_dir_if_missing (bool) if we want to create directory if missing
#' @return (bool) T or F depending file or directory existence
#' @export


CustomCheckFileExistence <- function(path_to_check, create_dir_if_missing=T){
  if(file_test("-f", path_to_check)){
    if(file.exists(path_to_check)){
      return(TRUE)
    }
    else{
      warning("fill in this part of code")
    }
  }

  if(file_test("-D", path_to_check)){
    if(!dir.exists(path_to_check)){
      if(create_dir_if_missing){
        dir.create(path_save,recursive=T)
        message("Directory '{path_to_check}' doesn't exist. It has just been
                created")
      }
      else message("Directory '{path_to_check}' doesn't exist. If you want to
                 create it, create_dir_if_missing must be True")
    }
    else return(TRUE)
  }
}
