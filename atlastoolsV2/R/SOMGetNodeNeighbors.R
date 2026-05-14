#' @title Find out node's neighbors
#' @name SOMGetNodeNeighbors
#' @description Return the numerous of node which are neighbor of current node 
#' passed in parameter.
#' @param current_node_number (int) the numerous of node to find neighbors.
#' @param matrix_som_pts matrix SOM nodes.
#' @param toroidal (bool) If toroidal (i.e. the ends of the map meet and the 
#' values are therefore continuous.).
#' @param eps (float) physical length of one node on y axis.
#' @return (list) neighbors nodes
#' @export


.SOMGetNodeNeighbors <- function(current_node_number, matrix_som_pts, 
                                 toroidal=F, eps = 0.8660254){
  
  # min and max of the SOM coordinates for axis x and y:
  min_x = min(matrix_som_pts[,1])
  max_x = max(matrix_som_pts[,1])
  min_y = min(matrix_som_pts[,2])
  max_y = max(matrix_som_pts[,2])
  
  coord = matrix_som_pts[current_node_number,]
  x0 = coord[1]
  y0 = coord[2]
  
  coord = rbind(coord, matrix(c(x0-1, y0), nrow=1))
  coord = rbind(coord, matrix(c(x0+1, y0), nrow=1))
  coord = rbind(coord, matrix(c(x0+0.5, y0+eps), nrow=1))
  coord = rbind(coord, matrix(c(x0+0.5, y0-eps), nrow=1))
  coord = rbind(coord, matrix(c(x0-0.5, y0+eps), nrow=1))
  coord = rbind(coord, matrix(c(x0-0.5, y0-eps), nrow=1))
  
  if(toroidal){
    coord[coord[,1]>max_x,1]=min_x
    coord[coord[,1]<min_x,1]=max_x
    coord[coord[,2]>max_y,2]=min_y
    coord[coord[,2]<min_y,2]=max_y
  } else {
    # remove coordinates outside interval [min, max] for both axis:
    coord = coord[(coord[,1] <= max_x) & (coord[,1] >= min_x),]
    coord = coord[(coord[,2] <= max_y) & (coord[,2] >= min_y),]
  }
  
  neighbors_node = c()
  # double loop to find out and get index of same node between coord and 
  # matrix_som_pts
  # for each cells of this both matrix we test if coordinates are same, in which
  # case is the same node.
  for(i in 1:nrow(coord)){
    for(j in 1:nrow(matrix_som_pts)){
      if(as.integer(matrix_som_pts[j,1]*100) == as.integer(coord[i,1]*100) &
         as.integer(matrix_som_pts[j,2]*100) == as.integer(coord[i,2]*100)){
        neighbors_node = c(neighbors_node, j)
      }
    }
  }
  
  return(neighbors_node)
}
