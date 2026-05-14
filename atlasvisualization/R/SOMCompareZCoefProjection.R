#' @title Functino to compare coefficients projection.
#' @name SOMCompareZCoefProjection
#' @description
#' @param data.som
#' @param groups
#' @param clusters.csv
#' @param projected_new
#' @param nb.samples
#' @param path.plot.save
#' @param color.palette
#' @param title (str) figure title
#' @param as_pdf (bool) if generate and save pdf figure
#' @param as_png (bool) if generated and save png figure
#' @param width (float) width of the figure.
#' @param height (float) height of the figure.
#' @export


SOMCompareZCoefProjection <- function(data.som, groups, clusters.csv,
                                      projected_new, nb.samples, path.plot.save,
                                      color.palette=NULL,
                                      title="Z_proj_coef_compare_with_Zref",
                                      as.pdf=T, as.png=F, width=7, height=7){

  require(kohonen)

  if(is.null(color.palette))
    color.palette <- colorRampPalette(c("#0073C2FF","#EFC000FF","#CD534CFF"),
                                      interpolate="linear")




  sample.projection.merge <- merge(clusters.csv, projected_new, by="sample",
                                   all=FALSE)
  sample.projection.merge <- sample.projection.merge[, c("sample",
                                                         "code",
                                                         "node")]
  commun.nodes.percenatge <-
    sum(sample.projection.merge$node == sample.projection.merge$code) /
    nrow(sample.projection.merge) * 100

  main <- sprintf("Z proj coef compare with Z reference: %.2f%% common nodes",
                  commun.nodes.percenatge)


  fn = title

  if(as.pdf){
    pdf(file.path(path.plot.save, paste0(fn, ".pdf")),
        width=width,
        height=height)
  } else if (as.png){
    png(file.path(path.plot.save, paste0(fn, ".png")),
        width=600,
        height=350)
  }

  plot(data.som,
       type="property",
       property=groups,
       main=main,
       palette.name=color.palette)

  kohonen::add.cluster.boundaries(data.som, clustering=groups)

  for(ncr in data.som$unit.classif[1:nb.samples]){
    coord = data.som$grid$pts[ncr,]
    points(coord[1],
           coord[2],
           col ="black",
           bg="green",
           pch = 21,
           cex=2)
  }

  for(ncr in 1:nb.samples){
    coord = data.som$grid$pts[projected_new[ncr,]$node,]
    points(coord[1],
           coord[2],
           col ="black",
           bg="orange",
           pch = 21,
           cex=2)
  }

  dev.off()
}
