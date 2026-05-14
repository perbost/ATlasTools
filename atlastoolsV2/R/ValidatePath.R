#' @title Function to test path to save file.
#' @name ValidatePath
#' @param path The path to the file to be saved.
#' @return A boolean value indicating whether the path is valid.
#' @export


ValidatePath <- function(path, nb.char.max=182){
  
  # Check if the path is shorter than nb.char.max bits:
  if (nchar(path) > nb.char.max) {
    stop(sprintf("The access path is too long to save the object. 
         The program stops because it's better to check whether the unregistered
         object is essential for the next step, and so the problem must be 
         corrected before continuing.
         path: %s", path))
  }
  return(TRUE)
}
