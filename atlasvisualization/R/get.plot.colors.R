#' @title Return list of n colors scaled on color palette defined
#' @name get.plot.colors
#' @description
#' @param n (int) number of colors to return
#' @param color.palette color palette to use.
#' @return list of colors
#' @export

get.plot.colors <- function(n=5, color.palette=NULL, sample.color.flash=T){
  #
  if(is.null(color.palette)){
    if(sample.color.flash){
      return(color.palette <-
               colorRampPalette(c("#5bcefa","#efdf00","#f7785e"),
                                interpolate="linear")(n))
    } else{
      return(color.palette <-
               colorRampPalette(c("#0073C2FF","#EFC000FF","#CD534CFF"),
                                interpolate="linear")(n))
    }
  } else{
    return(color.palette(n))
  }
}
