#' @title Compute and generate plot about model variance
#' @name MOFAModelVarianceAnalysis
#' @description Compute R2 variance and cumulative explained variance and plot
#' several pdf about variance.
#' @param path.model directory to save plot
#' @export


MOFAModelVarianceAnalysis <- function(path.model, results=NULL){
  
  if(is.null(results)) 
    results <- MOFAModelExtractR2(path.model)
  
  MofaModelAnalysis(path.save=path.model, 
                                        plot.VarianceByFactor=T,
                                        file.name=
                                          "Variance_additionnal_byfactors.pdf",
                                        result=results,
                                        additional.variance=T)
  
  MofaModelAnalysis(path.save=path.model, 
                                        plot.VarianceByFactor=T,
                                        file.name=
                                      "Variance_additionnal_byfactors_loss.pdf",
                                        result=results,
                                        additional.variance=T,
                                        additional.variance.loss=T)
}
