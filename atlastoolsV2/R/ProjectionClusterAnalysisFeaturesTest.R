#' @title Compute cluster features correlation test.
#' @name ProjectionClusterAnalysisFeaturesTest
#' @description We compute test about correlation between features and cluster.
#' @param data Atlas data frame data.
#' @param clusters clusters consensus as csv.
#' @param path.result path to the folder where the outputs are saved.
#' @param threshold p value threshold.
#' @param view pass a list of views to analyse. By default is "NULL"' (do 
#' analyse for all views).
#' @param log2 (bool) if compute folder log2 change.
#' @param p.adj.threshold (bool) if we use adjust p value.
#' @param nb.cluster (int) number of cluster.
#' @param all.feature.test (bool) if we compute all features test for cluster
#' analyse.
#' @param hierarchical.test (bool) if we compute hierarchical test for cluster
#' analysis.
#' @param plot.box.plot (bool) if plot box plot figures as pdf.
#' @param color.cluster color palette for each cluster.
#' @export


ProjectionClusterAnalysisFeaturesTest <- function(data, clusters, path.result,
                                                  threshold=0.05, view=NULL, 
                                                  log2=T, p.adj.threshold=T,
                                                  nb.cluster=5, 
                                                  all.feature.test=T, 
                                                  hierarchical.test=T, 
                                                  plot.box.plot=T,  
                                                  color.cluster=NULL){
  
  require(dplyr)
  
  .Log2fc <- function(inner.df.data, inner.item, inner.matrix.samples.clusters,
                      inner.matrix.samples.other.clusters, log2){
    
    mean.cluster = 
      mean(inner.df.data[inner.df.data$feature == inner.item & 
                           inner.df.data$sample %in% 
                           inner.matrix.samples.clusters,]$value)
    mean.other = 
      mean(inner.df.data[inner.df.data$feature == inner.item & 
                           inner.df.data$sample %in% 
                           inner.matrix.samples.other.clusters,]$value)
    
    if(log2){
      log2fc = log2(1 + mean.cluster) - log2(1 + mean.other)
    } else {
      log2fc = mean.cluster - mean.other
    }
    return(log2fc)
  }
  
  
  if(is.null(color.cluster))
    color.cluster <- get.plot.colors(n=nb.cluster)
  data <- data.frame(data)
  data <- data[data$view == view,]
  
  if(nrow(data) == 0) return()
  
  
  for(cl in unique(clusters$cluster)){
    
    path.result.cluster.directory <- file.path(path.result, 
                                               sprintf("cluster%d", cl))
    if(!dir.exists(path.result.cluster.directory)) 
      dir.create(path.result.cluster.directory, recursive=T)
    
    # matrix with sample that belong to the current analysed cluster:
    matrix.samples.clusters <- clusters[clusters$cluster == cl,]$sample
    # matrix with sample that do not belong to the current analysed cluster:
    matrix.samples.other.clusters <- clusters[!(clusters$cluster == cl),]$sample
    
    # add clusters distances values (from clusters df) in data frame:
    df.data <- as.data.frame(merge(data, clusters, by=c("sample")))
    
    #  Initialize cluster column for sample belong to current cluster analyse:
    df.data$cluster <- sprintf("Cluster%d", df.data$cluster)
    # Initialize cluster column for sample that do not belong to current cluster
    # analyse:
    df.data$cluster.2 <- ""
    
    if(nrow(df.data[df.data$sample %in% matrix.samples.clusters,]) == 0) next
    
    df.data[df.data$sample %in% matrix.samples.clusters,]$cluster.2 <-
      sprintf("Cluster%d", cl)
    
    df.data[df.data$sample %in% matrix.samples.other.clusters,]$cluster.2 <-
      "other"
    
    df.data <- df.data[!df.data$cluster.2 == "",]
    df.data$cluster.2 <- factor(df.data$cluster.2,
                                levels=c(sprintf("Cluster%d", cl), "other"))
    df.data$cluster <- factor(df.data$cluster,
                              levels=sort(unique(df.data$cluster)))
    df.data <- df.data[!is.na(df.data$value),]
    
    if(all.feature.test){
      
      path.result.cluster.directory.all.feature <- 
        file.path(path.result.cluster.directory, "All_features_test")
      if(!dir.exists(path.result.cluster.directory.all.feature)) 
        dir.create(path.result.cluster.directory.all.feature, recursive=T)
      
      result <- data.frame()
      
      for(item in unique(df.data$feature)){

        # Wilcoxon test: compare expression levels of those features between 
        # each pair of clusters:
        wt = coin::wilcox_test(value~cluster.2,
                               data=df.data[df.data$feature == item,])
        # Kruskal-Wallis test: identify features that are significantly 
        # different between the clusters:
        kt = coin::kruskal_test(value~cluster,
                                data=df.data[df.data$feature == item,])
        # fit linear models (regression):
        tt = summary(lm(value~cluster.2,
                        data=df.data[df.data$feature == item,]))
        
        log2fc <- .Log2fc(df.data, item, matrix.samples.clusters,
                          matrix.samples.other.clusters, log2)

        result <- rbind(result,
                        data.frame(feature=item,
                                   log2fc=log2fc,
                                   p.value_wt=coin::pvalue(wt),
                                   p.value_kt=coin::pvalue(kt),
                                   p.value_tt=tt$coefficients[2,4]))
      }
      
      result$p.adj_wt = p.adjust(result$p.value_wt, method = "BY")
      result$p.adj_kt = p.adjust(result$p.value_kt, method = "BY")
      result$p.adj_tt = p.adjust(result$p.value_tt, method = "BY")
      
      result <- result[order(result$p.value_wt),]
      
      if(ValidatePath(
        file.path(path.result.cluster.directory.all.feature,
                  sprintf("tests__%s_clst_%d.csv",
                          view, 
                          cl)))){
        readr::write_delim(result,
                           file.path(path.result.cluster.directory.all.feature,
                                     sprintf("tests__%s_clst_%d.csv",
                                             view, 
                                             cl)),
                           delim = ";")
      }
      
      if(plot.box.plot){
        ProjectionClusterAnalysisBoxPlot(
          df.data,
          result,
          cl, 
          view, 
          color.cluster[cl],
          path.result.cluster.directory.all.feature,
          p.adj.threshold=p.adj.threshold,
          threshold=threshold,
          log2=log2)
        }
    }
    
    if(hierarchical.test){
      
      path.result.cluster.directory.ht <- 
        file.path(path.result.cluster.directory, "Hierarchical_test")
      
      if(!dir.exists(path.result.cluster.directory.ht)) 
        dir.create(path.result.cluster.directory.ht, recursive=T)
      
      result.kt <- data.frame()
      
      for(item in unique(df.data$feature)){
        
        # Kruskal-Wallis test: identify features that are significantly 
        # different between the clusters:
        kt <- coin::kruskal_test(value~cluster,
                                 data=df.data[df.data$feature == item,])
        result.kt <- rbind(result.kt, data.frame(feature=item,
                                                 p.value_kt=coin::pvalue(kt)))
      }
      
      result.kt$p.adj_kt <- p.adjust(result.kt$p.value_kt, method = "BY")
      
      # Filter for significant features:
      significant_features <- result.kt %>% 
        filter(p.adj_kt <= threshold) %>%
        select(feature)
      
      result.significant.feature.wt <- data.frame()
      
      for(item in significant_features[[1]]){
        
        # Use the Wilcoxon test to compare the expression levels of significant
        # features between each pair of clusters:
        wt <- coin::wilcox_test(value~cluster.2,
                                data=df.data[df.data$feature == item,])
        
        log2fc <- .Log2fc(df.data, item, matrix.samples.clusters, 
                          matrix.samples.other.clusters, log2)
        
        result.significant.feature.wt <- 
          rbind(result.significant.feature.wt, 
                data.frame(feature=item,
                           log2fc=log2fc,
                           p.value_wt=coin::pvalue(wt)))
      }
      
      result.significant.feature.wt$p.adj_wt <-
        p.adjust(result.significant.feature.wt$p.value_wt, method = "BY")
      
      if(ValidatePath(file.path(path.result.cluster.directory.ht,
          sprintf("HT__%s_clst_%d.csv", view, cl)))){
        readr::write_delim(result.significant.feature.wt,
                           file.path(path.result.cluster.directory.ht,
                                     sprintf("HT__%s_clst_%d.csv", view, cl)),
                           delim = ";")
      }
      
      if(plot.box.plot){
        ProjectionClusterAnalysisBoxPlot(
          df.data,
          result.significant.feature.wt,
          cl, 
          view, 
          color.cluster[cl],
          path.result.cluster.directory.ht,
          p.adj.threshold=p.adj.threshold,
          threshold=threshold,
          log2=log2)
      }
    }
  }
}
