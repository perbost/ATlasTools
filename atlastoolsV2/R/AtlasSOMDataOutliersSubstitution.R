#' @title Replaces outliers values in the data frame.
#' @name AtlasSOMDataOutliersSubstitution
#' @description Function to replace outliers.
#' By default we compute Low and Up values from quantiles (0.25 and 0.75).
#' Then the values above and below boundaries are replaced by these values.
#' Default boundaries values are:
#'     Low:  Q3 + 3*IQR
#'     Up: Q - 3*IQR
#' @param data data to be modified.
#' @param low (float) value of lower boundary (the values below "low" will be 
#' replaced by "low").
#' @param up (float) value of upper boundary (the values above "up" will be 
#' replaced by up").
#' @param by_column (boolean) Replace outliers independently by column 
#' (recommended).
#' @return data frame modified
#' @export


AtlasSOMDataOutliersSubstitution <- function(data, low=NULL, up=NULL,
                                             by_column=TRUE){

  .BoundariesCompute <- function(data_, low_=NULL, up_=NULL){
    if(is.null(low_)) low_ <- quantile(data_, probs = 0.25)
    if(is.null(up_)) up_ <- quantile(data_, probs = 0.75)
    iqr <- up_-low_
    upper_bound <- up_+3*iqr
    lower_bound <- low_-3*iqr
    return(c(upper_bound, lower_bound))
  }

  if(by_column){
    for(column in colnames(data)){
      list_boundaries <- .BoundariesCompute(data[,column])
      upper_bound <- list_boundaries[1]
      lower_bound <- list_boundaries[2]
      data[data[,column]<lower_bound,column]=lower_bound
      data[data[,column]>upper_bound,column]=upper_bound
    }
  }
  else{
    list_boundaries <- .BoundariesCompute(data)
    upper_bound <- list_boundaries[1]
    lower_bound <- list_boundaries[2]
    data<lower_bound = lower_bound
    data>upper_bound = upper_bound
  }
  return(data)
}
