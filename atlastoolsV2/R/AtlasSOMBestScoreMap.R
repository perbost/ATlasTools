#' @title Compute three scores to compare all maps clustering
#' @name AtlasSOMBestScoreMap
#' @description Build matrix of shape (nb.SOM.maps, 3) with scores for each map.
#' @param nb.cluster (int) number of cluster to test.
#' @param nb.SOM.maps number of maps to generate.
#' @param path.maps.baseline path to cluster folder.
#' @param path.maps.consensus path to save csv.
#' @param nb.clusters (int) number of cluster.
#' @param clustering.consensus.outputs data frame with clustering consensus.
#' @param model.name name of current model.
#' @param compare.models (bool) if we compare two models factors.
#' @param ARI.score (bool) if we use Adjust Rand Index score to find best map.
#' @param matrix.trace.score (bool) if we use matrix trace score to find best 
#' map.
#' @param nb.maps.to.keep (int) number of best maps to keep.
#' @param jaccard.index (bool) if use jaccard index (default=F).
#' @param clustering.method (str) clustering method used.
#' @export


AtlasSOMBestScoreMap <- function(nb.cluster, nb.SOM.maps, 
                                 path.maps.baseline, 
                                 path.maps.consensus, nb.clusters,
                                 clustering.consensus.outputs, model.name,
                                 compare.models=F, ARI.score=T, 
                                 matrix.trace.score=T, nb.maps.to.keep=50,
                                 jaccard.index=F, clustering.method=
                                   "hierarchical_clustering"){
  
  path.maps.baseline.cluster <- file.path(path.maps.baseline,
                                          sprintf("Cluster_%02d", nb.cluster))
  path.maps.save <- file.path(path.maps.consensus,
                              sprintf("Cluster_%02d", nb.cluster))
  
  if(matrix.trace.score){
    
    clustering.consensus <- tidyr::spread(clustering.consensus.outputs,
                                          "cluster",
                                          "cluster")
    clustering.consensus <- as.matrix(clustering.consensus[,-c(1)])
    
    clustering.consensus[!is.na(clustering.consensus)] <- 1
    clustering.consensus[is.na(clustering.consensus)] <- 0
    
    results <-data.frame()
    
    for(nth.map in 1:nb.SOM.maps){
      
      path.maps.baseline.cluster.nth.map <- 
        file.path(path.maps.baseline.cluster, sprintf("Map_lin%03d",nth.map))
      
      clustering.nth.map <- 
        readr::read_delim(file.path(path.maps.baseline.cluster.nth.map,
                                    "Clusters.csv"),
                          delim=";",
                          escape_double=FALSE,
                          trim_ws=TRUE)
      
      if(compare.models){
        rownames(clustering.consensus) <- gsub("_cl.$", "",
                                               clustering.nth.map$sample)
      }
      
      clusters <- 
        AtlasSOMClusterMatrixToSpread(
          path.maps.model.cluster.nth.map=
            file.path(path.maps.baseline.cluster,
                      sprintf("Map_lin%03d", nth.map)))

      matrix.correlation <- t(clustering.consensus) %*% clusters
      permutation <- combinat::permn(1:nb.clusters)
      
      matrix.trace <- c()
      score.trace <- c()
      
      for(p in permutation){
        matrix.permutation <- matrix.correlation[, p]
        matrix.trace <- c(matrix.trace,
                          sum(diag(matrix.permutation))/sum(matrix.permutation))
        score <-0
        for(i in 1:nb.clusters){
          score <- score + matrix.permutation[i,i]/sum(matrix.permutation[i,])
        }
        score.trace <- c(score.trace, score/i)
      }
      results <- rbind(results,
                       data.frame(map=sprintf("Map_lin%03d", nth.map),
                                  diag=max(matrix.trace),
                                  score=max(score.trace)))
    }
    if(ValidatePath(file.path(
      path.maps.save, sprintf("best_map_%d.csv", nb.SOM.maps)))){
      readr::write_csv(results,
                       file.path(path.maps.save, sprintf("best_map_%d.csv",
                                                         nb.SOM.maps)))
    }
    
    results.score.diagonal <- results[order(results$diag, decreasing=T),]
    results.score.score <- results[order(results$score, decreasing=T),]
    
    if(jaccard.index){
      
      clustering.consensus.outputs <- 
        readr::read_csv(file.path(path_save, 
                                  sprintf("clustering_consensus_%d_%s.csv",
                                          nb.SOM.maps, clustering.method)))
      
      global.clustering.matrix <- 
        readr::read_csv(file.path(path_save,
                                  sprintf("global_clustering_matrix_%d.csv",
                                          nb.SOM.maps)))
      
      jaccard.distances.all.clusters <- 
        prabclus::jaccard(t(global.clustering.matrix))
      
      sensibility.mean <- 
        AtlasSOMSensibility(clustering.consensus.outputs,
                                          jaccard.distances.all.clusters)
      
      specificity.mean <- 
        AtlasSOMSpecificty(clustering.consensus.outputs,
                                         jaccard.distances.all.clusters)
      
      df.jaccard.index <- data.frame(model=model.name,
                                     nb_cluster=nb.clusters,
                                     best_score=results.score.score[1,]$score,
                                     best_score_map=results.score.score[1,]$map,
                                     best_diag=results.score.diagonal[1,]$diag,
                                     best_diag_map=
                                       results.score.diagonal[1,]$map)
      
    } else{
      df.jaccard.index <- data.frame(model=model.name,
                                     nb_cluster=nb.clusters,
                                     sensibility=sensibility.mean,
                                     specificity=specificity.mean,
                                     best_score=results.score.score[1,]$score,
                                     best_score_map=results.score.score[1,]$map,
                                     best_diag=results.score.diagonal[1,]$diag,
                                     best_diag_map=
                                       results.score.diagonal[1,]$map)
      
    }
    
    if(ValidatePath(file.path(path.maps.save,
                              sprintf("df_jaccard_index_%d.csv",
                                      nb.SOM.maps)))){
      readr::write_csv(df.jaccard.index,
                       file.path(path.maps.save,
                                 sprintf("df_jaccard_index_%d.csv", 
                                         nb.SOM.maps)))
    }
  }
  
  if(ARI.score){
    AtlasSOMARIMapSelection(path.maps.baseline.cluster, 
                                          path.maps.save, 
                                          clustering.consensus.outputs, 
                                          nb.SOM.maps, 
                                          nb.maps.to.keep=nb.maps.to.keep)
  }
}
