#' @title Function to plot custom labels around radarchart.
#' @name RadarChartCustomLabels
#' @description We define labels type and color to plot it around radarchart.
#' @param labels (list) list of string with label name to plot around radarchart
#' @param color.labels (list) color for each feature text label.
#' @param circle.coord (list) coordonated of the circle to plot.
#' @param labels.cex (float) numeric character expansion factor; multiplied by
#'  ("cex") yields the final character size. NULL and NA are equivalent to 1.0
#'  (default=0.8).
#' @param font (int) font type: 1: normal, 2: bold, 3: italic.
#' @export


RadarChartCustomLabels <- function(labels, color.labels,
                                   circle.coord=0.5, labels.cex=0.8, font=1){

  n <- length(labels)
  angles <- seq(from = pi / 2, by = 2 * pi / n, length.out = n)

  for (i in seq_along(labels)) {
    x <- circle.coord * cos(angles[i])
    y <- circle.coord * sin(angles[i])
    text(x, y, labels = WrapStrings(labels[i], max.char.label),
         col = color.labels$color[i],
         cex = labels.cex,
         font=font)
  }
}
