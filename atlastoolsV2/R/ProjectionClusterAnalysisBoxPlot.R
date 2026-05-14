#' @title Function to define box plot parameters and generate figure.
#' @name ProjectionClusterAnalysisBoxPlot
#' @description We initialize parameters for box l=plot figure and then we run
#' visualize function to generate box plot.
#' @param df.data Atlas data frame data.
#' @param df.result (dataframe) data frame with results for box plot values.
#' @param nb.cluster (int) nth cluster to analyse.
#' @param view (str) current view of data.
#' @param color.cluster (str) color.cluster for studied cluster.
#' @param path.result path to save result.
#' @param p.adj.threshold (bool) if we use adjust p value.
#' @param threshold p value threshold.
#' @param log2 (bool) if compute folder log2 change.
#' @export


ProjectionClusterAnalysisBoxPlot <- function(df.data, df.result, nb.cluster, 
                                             view, color.cluster, path.result, 
                                             p.adj.threshold=T, threshold=0.05,
                                             log2=F){
  
  ith.iteration <- 0
    
  for(r in 1:nrow(df.result)){
    
    ith.iteration <- ith.iteration + 1
    feature <- df.result[r,]$feature
    feature.name.figure <- gsub("/", "_", feature)

    if(p.adj.threshold){
      pv <- df.result[r,]$p.adj_wt
    } else {
      pv <- df.result[r,]$p.value_wt
    }
    
    if(pv <= threshold){
      
      fn <- file.path(path.result,
                      sprintf("%03d_clst%d_wilcox_%s__%s.pdf",
                              ith.iteration,
                              nb.cluster,
                              view,
                              feature.name.figure))
      
      if(!ValidatePath(fn)){
        # calculates the number of characters available before the path
        # becomes bad
        number.char.available <- 182 - 
          nchar(file.path(path.result,
                          sprintf("%03d_clst%d_wilcox_%s__",
                                  ith.iteration,
                                  nb.cluster,
                                  view)))
        
        # Cut the string at the "number.char.available"-th character:
        short.feature.name <- substr(feature.name.figure, 
                                     1,
                                     number.char.available - 9)
                                             
        fn <- file.path(path.result,
                        sprintf("%03d_clst%d_wilcox_%s__%s__cutname.pdf",
                                ith.iteration,
                                nb.cluster,
                                view,
                                short.feature.name))
      }
      
      if(sum(df.data[df.data$feature == feature & 
                     df.data$cluster.2 == sprintf("Cluster%d", 
                                                  nb.cluster), ]$value) == 0)
        df.data[df.data$value == 0,]$value <- 1
      
      if(sum(df.data[df.data$feature == feature &
                     df.data$cluster.2 == "other",]$value) == 0)
        df.data[df.data$value == 0,]$value <- 1
      
      if(p.adj.threshold){
        value.p <- df.result[r,]$p.adj_wt
      } else{
        value.p <- df.result[r,]$p.value_wt
      }
      
      BoxPlot(fn, 
                                  df.data, 
                                  color.cluster, 
                                  feature, 
                                  value.p,
                                  log2=log2)
    }
  }
}
