#' @title Associate nearest SOM neuron for each Z rows.
#' @name ProjectionGetSOMNodes
#' @description Compute the nearest neuron unit for each Z rows on the SOM map.
#' @param dt.Z Z data frame to find nearest neuron.
#' @param data.som SOM map
#' @export


ProjectionGetSOMNodes <- function(dt.Z, data.som){
  
  data.projection <- data.frame()
  
  for(nth.sample in rownames(dt.Z)){
    
    dt.Z.vector <- dt.Z[nth.sample,]
    minimal.differnce <- 1e20
    
    for(node in 1:nrow(data.som$codes[[1]])){
      
      data.som.codes.vector <- data.som$codes[[1]][node,]
      
      distance <- sqrt(sum((dt.Z.vector - data.som.codes.vector)**2))
      
      if(distance < minimal.differnce){
        minimal.differnce <- distance
        minimize.node <- node
      }
    }
    data.projection <- rbind(data.projection,
                                 data.frame(sample=nth.sample,
                                            node=minimize.node,
                                            distance = minimal.differnce))
  }
  return(data.projection)
}
