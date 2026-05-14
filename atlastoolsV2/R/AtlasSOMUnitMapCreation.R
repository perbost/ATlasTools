#' @title Create and save SOM map as rds format.
#' @name AtlasSOMUnitMapCreation
#' @description Create Kohonen SOM map with argument parameters and save it.
#' @param data matrix or data frame containing the input data to be used in the 
#' Self-Organizing Map (SOM) algorithm.
#' @param n.grid (int) number of nodes or neurons to be used in the 
#' Self-Organizing Map (SOM) algorithm.
#' @param radius neighborhood radius used in the Self-Organizing Map (SOM)
#'  algorithm. The neighborhood radius determines the size of the neighborhood 
#'  around the winning neuron or node that will be updated during each training 
#'  iteration.
#'  During the training process, the neighborhood radius is gradually decreased 
#'  over time as the SOM converges to a stable configuration. This allows the 
#'  SOM to start with a large neighborhood radius that covers the entire map,
#'  and then gradually narrow the focus to smaller areas as the training 
#'  progresses.
#' @param topo  type of neighborhood function to use in the Self-Organizing Map
#'  (SOM) algorithm.
#' @param toroidal (bool) specifies whether the Self-Organizing Map (SOM) grid
#'  should be toroidal or not
#' @param maxNA.fraction maximum fraction of missing values allowed in the input
#' data. If the maxNA.fraction argument is set to a value between 0 and 1, 
#' the SOM algorithm will automatically remove any columns (variables) from the 
#' input data that have a fraction of missing values greater than maxNA.fraction.
#' @param rlen (int) specifies the number of iterations or epochs that the 
#' Self-Organizing Map (SOM) algorithm will be run for during training.
#' @param alpha  specifies the learning rate or step size used during the 
#' training process of the Self-Organizing Map (SOM) algorithm.
#' @return data.som generated
#' @export


AtlasSOMUnitMapCreation <- function(data, n.grid=20, radius=NULL,
                                    topo="hexagonal", toroidal=F, 
                                    maxNA.fraction=1, rlen=2000, 
                                    alpha=c(0.5,0.01), super.som=F){
  
  if(is.null(radius))
    radius <- c(1.5 * n.grid, 
                quantile(kohonen::unit.distances(kohonen::somgrid()), 1/3))
  
  grid <- kohonen::somgrid(xdim=n.grid, 
                           ydim=n.grid, 
                           topo=topo, 
                           toroidal=toroidal)
  
  if(super.som){
    data.som <- 
      kohonen::supersom(data,
                        grid,
                        radius=radius,
                        maxNA.fraction=maxNA.fraction,
                        rlen=rlen,
                        alpha=alpha)
  } else{
    data.som <- 
      kohonen::som(data,
                   grid,
                   radius=radius,
                   maxNA.fraction=maxNA.fraction,
                   rlen=rlen,
                   alpha=alpha)
  }
  
  return(data.som)
}
