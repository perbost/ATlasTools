#' @title Check of the cluster entity on the map
#' @name SOMComputeFitToConsensus
#' @description check that the clusters are not split into two or more parts.
#' This is checked in order to respect the intuition that the samples physically
#' close on the map must be biologically similar and therefore they must belong
#' to the same cluster.
#' We define also the concordance to consensus score to measure how close 
#' the map is to the consensus map. 
#' @param new.groups clustering.
#' @param nb.clusters (int) current number of cluster.
#' @param path.map.directory path to save consensus results.
#' @param cluster.consensus data frame of consensus clustering.
#' @param cluster.new data frame with new clustering obtained with new.groups.
#' @param model.name name of current model.
#' @param matrix.som.pts nodes SOM coordinates as matrix.
#' @param nth.map (int) current map number.
#' @param add.quantization.score (bool) if we add quantization score to measure
#' best som map.
#' @param data.som matrix with SOM map factors values.
#' @export


SOMComputeFitToConsensus <- function(new.groups, nb.clusters, 
                                     path.map.directory, cluster.consensus,
                                     cluster.new, model.name, matrix.som.pts, 
                                     nth.map, add.quantization.score=T,
                                     data.som=NULL){
  
  list.node <- data.frame(cluster = new.groups)
  list.node$node <- names(new.groups)
  list.node$sub <- paste(list.node$cluster, list.node$node, sep="|")
  
  sub_list <- list()
  
  for(row in rownames(list.node)){
    sub_list[[list.node[row,]$sub]] <- c(row)
  }
  
  for(row in rownames(list.node)){
    n <- as.integer(gsub("V", "", row))
    nb.cluster <- list.node[n,]$cluster
    nb.sub <- list.node[n,]$sub
    for(neighbor in .SOMGetNodeNeighbors(n, matrix.som.pts)){
      neighbor.cluster <- list.node[neighbor,]$cluster
      neighbor.sub <- list.node[neighbor,]$sub
      
      if((nb.cluster != neighbor.cluster) | (nb.sub == neighbor.sub)) next
      
      for(ncl in sub_list[[neighbor.sub]]) list.node[ncl,]$sub <- nb.sub
      
      sub_list[[nb.sub]] <- union(sub_list[[nb.sub]], sub_list[[neighbor.sub]])
      sub_list <- within(sub_list, rm(list = neighbor.sub))
    }
  }
  
  # if no subdivides cluster:
  if(length(sub_list) == nb.clusters) write(NULL, 
                                           file.path(path.map.directory, 
                                                     ".ConsensusCluster.ok"))
  
  analyse.cluster <- merge(cluster.consensus,
                           cluster.new,
                           by=c("sample"),
                           suffixes=c("_cons", "_new"))[,c("sample",
                                                           "cluster_cons",
                                                           "cluster_new")]
  
  fisher.table <- stats::ftable(analyse.cluster$cluster_cons,
                                analyse.cluster$cluster_new)
  
  errors.quantization <- NA
  
  if(add.quantization.score){
    # quantization error measures how well the SOM represents the input data
    errors.quantization <- mean(data.som$distances)
  }
  
  df.fit.to.consensus <- 
    data.frame(model=model.name,
               nb.clusters=sprintf("Cluster_%02d", nb.clusters),
               map=sprintf("Map_lin%03d", nth.map), 
               concordance_to_consensus=
                 sum(diag(fisher.table)) / sum(fisher.table), 
               over.cluster=length(sub_list) - nb.clusters,
               errors.quantization=errors.quantization)
  
  if(ValidatePath(file.path(path.map.directory,
                                          "df_fit_to_consensus.csv"))){
    readr::write_csv2(df.fit.to.consensus, 
                      file.path(path.map.directory, "df_fit_to_consensus.csv"))
  }
}

