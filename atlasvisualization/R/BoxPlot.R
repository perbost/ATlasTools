#' @title Function to generate box plot cluster analysis.
#' @name BoxPlot
#' @description This function generate box plot figure filled in by group.
#' @param fn (str) path  with pdf name.
#' @param df.data (dataframe) data frame of feature values.
#' @param color.cluster (str) color of the cluster to plot.
#' @param feature (str) feature to analyse.
#' @param value.p (float) p value of the analyse.
#' @param log2 (bool) if use log2 scale in the figure.
#' @export


BoxPlot <- function(fn, df.data, color.cluster, feature, value.p,
                    log2=F){


  data <- df.data[df.data$feature == feature,]
  data$color.fill <- ifelse(data$cluster.2 == "other", "lightgray",
                            color.cluster)

  pdf(fn)

  gg = ggpubr::ggboxplot(data,
                         x="cluster.2",
                         y="value",
                         yscale=ifelse(log2, "log2", "none"),
                         fill="cluster.2",
                         palette=c(color.cluster, "lightgrey"),
                         xlab="Cluster",
                         ylab=feature,
                         outlier.shape=NA) +
    ggplot2::geom_jitter(width=0.1, height=0)

  gg = gg + ggplot2::annotate("text",
                              x=1.5,
                          y=max(df.data[df.data$feature == feature,]$value)*1.2,
                              label=sprintf("Wilcoxon test, p.value=%4.3f",
                                            value.p))
  print(gg)
  dev.off()
}
