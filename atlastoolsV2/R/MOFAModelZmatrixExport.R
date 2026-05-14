#' @title Extract and save Z matrix of MOFA model
#' @name MOFAModelZmatrixExport
#' @description Load MOFA model trained and extract Z matrix as data frame, then
#' this Z matrix is saved as .RDS file.
#' @param model (MOFA) MOFA model trained containing weights to extract.
#' @param path.save path to save the Z matrix
#' @param regularized (bool) if we regularized weights of the weights matrix
#' @export


MOFAModelZmatrixExport <- function(model, path.save, regularized=F){
  
  if(regularized){
    init <- T
    for(group in names(model@expectations$Z)){
      if(init){
        dbtb <-  model@expectations$Z[[group]]
        data <- t(t(dbtb)/sqrt(colSums(dbtb*dbtb)))
      } else {
        dbtb <-  model@expectations$Z[[group]]
        data <- rbind(data,t(t(dbtb)/sqrt(colSums(dbtb*dbtb))))
      }
    }
  } else {
    init <- T
    for(group in names(model@expectations$Z)){
      if(init){
        data <-  model@expectations$Z[[group]]
      } else {
        data <- rbind(data,model@expectations$Z[[group]])
      }
    }
  }
  saveRDS(data, file.path(path.save, "Zmatrix.rds"))
}
