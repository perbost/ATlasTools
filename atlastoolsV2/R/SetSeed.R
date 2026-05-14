#' @title Private function to set seed for reproductibility
#' @name .SetSeed
#' @description Function to define same seed for all project.
#' @param seed (int): by default is 42
#' @export

.SetSeed <- function(seed=42){
  
  path_dir_to_seed <- "reports/seed"
  path_file_to_seed <- "reports/seed/seed.txt"
  
  if(file.exists(path_file_to_seed)) 
    existing_seed <- as.numeric(read.table(file=path_file_to_seed)[[1]][2]) 
  else{
    if(!dir.exists(path_dir_to_seed)) dir.create(path_dir_to_seed)
    write.table(seed, file=path_file_to_seed, sep="\t", row.names=FALSE)
  }
  if(seed != 42) warning("Warning: you use a different seed than the one defined
                         by default !")
  set.seed(seed)
}
