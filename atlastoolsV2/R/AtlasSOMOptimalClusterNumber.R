#' @title Compute the best number of cluster.
#' @name AtlasSOMOptimalClusterNumber
#' @description Algorithm to find out the best k number of cluster to cut
#' hierarchical clustering tree. We wan also generate plot for decision help
#' with Elbow and silhouette scores.
#' @param return.best.nb.cluster (bool) if we want return the value of optimal 
#' cluster number computes with our algorithm.
#' @param data.som.codes matrix with factors codes of SOM map.
#' #' @param hierrarchical.cluster.analysis hierarchical clustering of data as 
#' data frame.
#' @param path path to save plot.
#' @param min.nb.cluster (int): minimum of cluster possible
#' @param max.nb.cluster (int): maximum of cluster possible
#' @param plot.graph (bool): to plot it or not
#' @param return.losses.list (bool): if return losses list or not
#' @param plot.Elbow (bool): if compute and plot Elbow scores to decision help 
#' regarding best number of cluster
#' @param plot.silhouette (bool): if compute and plot Silhouette scores to*
#'  decision help regarding best number of cluster
#' @return either the list of losses and the optimal number of cluster (k) or 
#' only k.
#' @export


AtlasSOMOptimalClusterNumber <- function(return.best.nb.cluster=T, 
                                         data.som.codes=NULL,
                                         hierrarchical.cluster.analysis=NULL,
                                         path=NULL, min.nb.cluster=2, 
                                         max.nb.cluster=20, plot.graph=T, 
                                         return.losses.list=FALSE,
                                         plot.Elbow=T, plot.silhouette=T){
  
  
  if(is.null(path)) path <- "reports/maps/"
  
  if(plot.Elbow){
    if(is.null(data.som.codes)) stop("To compute Elbow score, you need to 
                                     provide 'data.som.codes' matrix.")
    
    fn = "Optimal_number_cluster_Elbow"
    
    # compute elbow:
    p <- factoextra::fviz_nbclust(data.som.codes, 
                                  factoextra::hcut,
                                  K.max=max.nb.cluster, 
                                  method="wss")
    ggplot2::ggsave(file.path(path, paste0(fn, ".pdf")), plot = p)
  }
  
  if(plot.silhouette){
    if(is.null(data.som.codes)) stop("To compute Silhouette score, you need to 
                                     provide 'data.som.codes' matrix.")
    
    fn = "Optimal_number_cluster_silhouette"
    
    # compute silhouette:
    p <- factoextra::fviz_nbclust(data.som.codes,
                                  factoextra::hcut,
                                  K.max=max.nb.cluster,
                                  method="silhouette")
    # Save plot as PDF file
    ggplot2::ggsave(file.path(path, paste0(fn, ".pdf")), plot = p)
  }
  
  
  if(return.best.nb.cluster){
    if(is.null(hierrarchical.cluster.analysis)) stop("to get the optimal number
                                                     of cluster, you need to
                                                     provide the 'hierarchical
                                                     cluster analysis' matrix.")
    if (!inherits(hierrarchical.cluster.analysis, "hclust"))
      hierrarchical.cluster.analysis <- 
        as.hclust(hierrarchical.cluster.analysis)
    
    max.nb.cluster <- min(max.nb.cluster,
                          length(hierrarchical.cluster.analysis$height))
    inert.gain <- rev(hierrarchical.cluster.analysis$height)
    intra <- rev(cumsum(rev(inert.gain)))
    
    relative.loss <- intra[min.nb.cluster:(max.nb.cluster)] / 
      intra[(min.nb.cluster - 1):(max.nb.cluster - 1)]
    best <- which.min(relative.loss)
    optimal_k <- best + min.nb.cluster - 1
    names(relative.loss) <- min.nb.cluster:max.nb.cluster
    
    if (plot.graph) {
      x <- relative.loss
      x[best] <- NA
      best_2 <- which.min(x)
      pch <- rep(1, max.nb.cluster-min.nb.cluster+1)
      pch[best] <- 16  # chosen symbol to highlight graph
      pch[best_2] <- 21
      
      fn = "Optimal_number_cluster_relative_loss"
      pdf(file.path(path, paste0(fn, ".pdf")),
          width=7,
          height=7)
      plot(seq(min.nb.cluster, max.nb.cluster, by = 1),
           main=paste("Optimal k: ", optimal_k), relative.loss,
           pch=pch, bg="grey")
      abline(v=best, col="grey", lty=2)
      abline(v=best_2, col="grey", lty=2)
      dev.off()
    }
    if(return.losses.list) {
      return(c(relative.loss, optimal_k))
    }
    else {
      return(optimal_k)
    }
  }
}

