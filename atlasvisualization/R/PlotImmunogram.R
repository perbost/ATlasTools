#' @title Function to plot Immunogram features regarding target clinical data.
#' @name PlotImmunogram
#' @description We define all radarchart parameters and we generate figure
#' with feautres selected for the target clinical data.
#' @param data (dataframe) data to generate radarchart.
#' @param views (list) list of views.
#' @param colors.view.banner (dataframe) colors associated to the view, to fill
#' in banner radarchert.
#' @param features_per_view (list) number of features associated to each view.
#' @param max.char.label (int) number of character before the line crossing
#' (default=10).)
#' @param title (str) figure title
#' @param circle.coord (list) coordonated of the circle to plot.
#' @param centerzero (bool) if axis radarchart are centered in zero (default=F).
#' @param line_style (int) type of the line: 1: normal, 2: bold, 3: italic
#' @param fill_color (list) parameter of the color to fill radarchart sample.
#' @param add.banner (bool) if we add banner with views labels (default=T).
#' @param plot.title (str) title of the figure.
#' @param path.plot.save (str) path to save the figure.
#' @param width (float) width of the figure.
#' @param height (float) height of the figure.
#' @param axistype (int) type of axis.
#' @param add.custom.label (bool) if we add banner with feature labels
#' (default=T).
#' @param labels.color (list) color for each feature text label.
#' @param labels.circle.coord (float) position of the circle on which we will
#' place the labels  (default=1.2).
#' @param labels.cex (float) numeric character expansion factor; multiplied by
#'  ("cex") yields the final character size. NULL and NA are equivalent to 1.0
#'  (default=0.8).
#' @param font (int) font type: 1: normal, 2: bold, 3: italic.
#' @param plot.marge (float) margin size to use in the figure (default=0).
#' @export


PlotImmunogram <- function(data, views, colors.view.banner, features_per_view,
                           colors=c("#0000FF","#FF0000", "#00AFBB"),
                           max.char.label=10, title=NULL,
                           circle.coord=c(1.42, 1.58, 1.85),
                           centerzero=TRUE, line_style=c(2,2,1),
                           fill_color = c(NA,NA,scales::alpha("#CCCCCC",0.75)),
                           add.banner=TRUE, plot.title="Untitled",
                           path.plot.save=NULL, width=7, height=7,
                           axistype=1, add.custom.label=TRUE, labels.color=NULL,
                           labels.circle.coord=0.5, labels.cex=0.8, font=1,
                           plot.marge=0){

  fn = plot.title
  pdf(file.path(path.plot.save, paste0(fn, ".pdf")),
      width=width,
      height=height)

  # Extend plotting region
  par(mar = c(0, 0, 0, 0) + plot.marge)

  # Plot the spider chart
  fmsb::radarchart(
    data,
    axistype=axistype,
    # Customize the polygon
    pcol=colors,
    plwd=2 , # Line width
    centerzero=centerzero,
    plty=line_style,
    pfcol=fill_color,
    # Customize the grid
    cglcol = "grey", cglty = 1, cglwd = 0.8,
    axislabcol = "grey",
    title=title,
    # Variable labels
    polarlabels = NULL,
    # vlcex = 0.7,
    vlabels = "",
    xlim = c(-2, 2),
    ylim = c(-2, 2)
  )

  if(add.banner){
    BannerWithLabels(colors.view.banner,
                     views,
                     features_per_view,
                     circle.coord)
  }

  if(add.custom.label){
    RadarChartCustomLabels(names(data),
                           labels.color,
                           labels.circle.coord,
                           labels.cex=labels.cex,
                           font=font)
  }
  dev.off()
}
