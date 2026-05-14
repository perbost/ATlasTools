#' @title Generate SOM map with different information about clustering
#' @name SomMapCluster
#' @description This function generates a figure representing a SOM map.
#' This function can add samples colored according to the cluster it belongs to.
#' It can plot also the cluster boundaries.
#'
#' About add pie chart: we need to pass a data frame with pie chart information.
#' We need to provide some information like proportions, p value, pie name in
#' the data frame.
#' TODO: define a specific format that the data for the pie chart will have to
#' respect (minimal information to have + tests).
#'
#' @param data.som (.rds) RSS map with information to plot
#' @param path.plot.save path to save figure
#' @param groups (default=NULL) matrix with clustering groups information
#' @param nb.clusters
#' @param add.boundaries (bool) (default=T) if we add cluster boundaries in plot
#' @param add.samples (bool) (default=F) if we add sample circle plot in the map
#' @param is.samples.colored (bool) (default=F) if we use colored sample
#' (according to the cluster which it belongs)
#' @param clusters (default=NULL) data frame with clustering information
#' @param as.pdf (bool) if generate and save pdf figure
#' @param as.png (bool) if generated and save png figure
#' @param type (str) type of SOM plot (by default is property)
#' @param property
#' @param title title of the figure
#' @param width width of the figure
#' @param height height of the figure.
#' @param main
#' @param color.palette
#' @param nb.colors (int) number of colors (equal of number of cluster)
#' @param nb.samples
#' @param add.pie
#' @param dt.projected
#' @param color.clinical.variable
#' @param color.bg
#' @param add.legend
#' @param fisher.table
#' @param legend
#' @param legend.txt
#' @param legend.fill
#' @param add.arrow
#' @param number.pies
#' @param list.p.value.test
#' @param list.titles
#' @param p.value.threshold
#' @param pie.labels
#' @param list.colors
#' @export


SomMapCluster <- function(data.som, path.plot.save, groups=NULL, nb.cluster=5,
                          add.boundaries=T, add.samples=F, is.samples.colored=F,
                          clusters=NULL, as.pdf=T, as.png=F, type="property",
                          property=NULL, title="Cluster", width=7, height=7,
                          main="No title", color.palette=NULL, nb.colors=20,
                          nb.samples=NULL, add.pie=F, dt.projected=NULL,
                          color.clinical.variable=NULL, color.bg=NULL,
                          add.legend=F, fisher.table=NULL, legend=F,
                          legend.txt=NULL, legend.fill=NULL, add.arrow=F,
                          number.pies=5, list.p.value.test=NULL,
                          list.titles=NULL, p.value.threshold=0.05,
                          pie.labels=NA, list.colors=NULL){

  require(kohonen)

  if(add.boundaries & is.null(groups))
    stop("Error: to plot clusters boundaries, you must provide the group
         information.")
  if(!(as.pdf | as.png))
    stop("Error: Choose a valid save format (pdf or png)")

  if(is.null(color.palette)){
    color.palette <- colorRampPalette(c("#0073C2FF","#EFC000FF","#CD534CFF"),
                                      interpolate="linear")
  }

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

  if(add.legend){
    # add marge for legend!
    par(mar = c(5, 4, 1.4, 0.2))
  }

  if(add.pie){

    # TODO: determine the number of pie with clinical data (or other) info
    # TODO: improve function to normalize pie utilization
    # For now, we use pie to plot cluster repartition

    if(is.null(dt.projected) && !is.null(clusters)){
      dt.clusters <- clusters
    } else dt.clusters <- dt.projected

    # Set up the layout regarding number of pie to plot:
    list.layout <- unlist(as.list(t(data.frame(column1 = rep(1, number.pies),
                                               column2 = 2:(number.pies + 1)))))
    layout(matrix(list.layout, nrow=2), heights=c(4, 1))

    # compute proportions values about datas for pie chart:
    column.data.pie <- dt.clusters$cluster
    proportions <- table(column.data.pie) / length(column.data.pie)
  }

  par(mar = c(0, 0, 0, 0))
  plot(data.som,
       type=type,
       property=property,
       ncolors=nb.colors,
       main=main,
       palette.name=color.palette)

  if(add.boundaries) kohonen::add.cluster.boundaries(data.som,
                                                     clustering=groups)

  if(add.samples){

    if(add.arrow){
      if(is.null(dt.projected)) stop("dt.projected is NULL")

      nrow <- nrow(dt.projected)

      points.base.noise <- rnorm(nrow, 0, 0.12)
      point.pit.noise <- rnorm(nrow, 0, 0.12)

      points(data.som$grid$pts[dt.projected[, ]$node,][, 'x'] + points.base.noise,
             data.som$grid$pts[dt.projected[, ]$node,][, 'y'] + points.base.noise,
             pch=21, col="black", cex=2, bg="red")

      points(data.som$grid$pts[dt.projected[, ]$code,][, 'x'] + points.base.noise,
             data.som$grid$pts[dt.projected[, ]$code,][, 'y'] + points.base.noise,
             pch=21, col="black", cex=2, bg="black")

      arrows(data.som$grid$pts[dt.projected[, ]$node,][, 'x'] + points.base.noise,
             data.som$grid$pts[dt.projected[, ]$node,][, 'y'] + points.base.noise,
             data.som$grid$pts[dt.projected[, ]$code,][, 'x'] + point.pit.noise,
             data.som$grid$pts[dt.projected[, ]$code,][, 'y'] + point.pit.noise,
             length = 0.1,
             code=2,
             col="white",
             lwd=2)
    } else{

      if(!is.null(dt.projected)){

        color.bg <- "white"
        if(is.samples.colored && "cluster" %in% names(dt.projected)){
          if(is.null(list.colors)) list.colors <- get.plot.colors(n=nb.cluster)
          color.bg <- list.colors[dt.projected$cluster]
        }

        nrow <- nrow(dt.projected)

        points(data.som$grid$pts[projected_atlas[,]$node,][, 'x'] + rnorm(nrow, 0, 0.12),
               data.som$grid$pts[projected_atlas[,]$node,][, 'y'] + rnorm(nrow, 0, 0.12),
               pch=21, col="black", cex=2, bg=color.bg)
      } else if(!is.null(clusters)){

        color.bg <- "white"
        if(is.samples.colored && "cluster" %in% names(clusters)){
          if(is.null(list.colors)) list.colors <-get.plot.colors(n=nb.cluster)
          color.bg <- list.colors[clusters$cluster]
        }

        nrow <- nrow(clusters)

        points(data.som$grid$pts[clusters[,]$code,][, 'x'] + rnorm(nrow, 0, 0.12),
               data.som$grid$pts[clusters[,]$code,][, 'y'] + rnorm(nrow, 0, 0.12),
               pch=21, col="black", cex=2, bg=color.bg)
      } else{
        color.bg <- "white"
        nrow <- nb.samples

        for(ncr in data.som$unit.classif[1:nb.samples]){

          coord = data.som$grid$pts[ncr,]
          if(is.samples.colored && !is.null(groups)){
            color.bg <- list.colors[groups[[ncr]]]
          }

          points(coord[1] + rnorm(1, 0, 0.12),
                 coord[2] + rnorm(1, 0, 0.12),
                 col ="black",
                 bg=color.bg,
                 pch = 21,
                 cex=2)
        }
      }
    }
  }

  if(add.legend)
    # TODO: add function to automatically adapt text and color legend for
    # clinical data for example.
    legend("topright", legend=lengend.text.cofo, fill=legend.color.cofo)

  if(add.pie){
    # plot pie chart then:
    par(mar = c(1, 1, 1, 1))
    if(is.null(list.colors)) list.colors <-get.plot.colors(n=nb.cluster)

    for(p in 1:number.pies){

      p.value.test <- list.p.value.test[p]
      pie.title <- list.titles[p]
      pie.subtitle <- sprintf("P value = %.4f", p.value.test)

      color <- "darkred"
      if(p.value.test < p.value.threshold) color <- "darkgreen"

      pie(c(proportions[p], 1-proportions[p]),
          col=c(list.colors[p], "white"),
          clockwise=TRUE,
          labels=pie.labels)
      # add pie chart title:
      title(main=pie.title)
      # add text below pie chart with font=3 (italic):
      mtext(pie.subtitle, side=1, font=3, cex=0.7, col=color)
    }
    # Reset the layout
    layout(1)
  }
  dev.off()
}
