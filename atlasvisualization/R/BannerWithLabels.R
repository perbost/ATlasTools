#' @title Function to compute angles limit for view name in the immunogram
#' banner.
#' @name BannerWithLabels
#' @description We define the part of the banner to fill in (color) regarding
#'  the view label associated.
#' @param color_df (dataframe) data frame with th color to fill in banner
#' depending the view.
#' @param views.labels (list) names of the views in the immunogram.
#' @param features_per_view (list) number of features associated to each view.
#' @param circle.coord (list) coordonated of the circle to plot.
#' @export



BannerWithLabels <- function(color_df, views.labels, features_per_view,
                             circle.coord=c(1.42, 1.58, 1.85)){

  # Convert number of features per view into angles
  theta_segments <- (features_per_view / sum(features_per_view)) * 2 * pi
  offset.angle.feature <- ((1 / sum(features_per_view)) * 2 * pi)

  # radarchart start angle to plot figure at 12 but the banner start angle count
  # at 3. So we set an offset to begin at the same place and we add a small offset
  # to make label at the center of the banner view.
  offset.start <- (2 * pi) / 4
  offset <- offset.start - (offset.angle.feature / 2)

  # Calculate start and end for each segment
  theta_plot <- cumsum(c(0, theta_segments)) + offset

  i <- 1
  for (view in views.labels) {
    theta <- seq(theta_plot[i], theta_plot[i+1], length.out = 100)
    x_inner <- circle.coord[1] * cos(theta)
    y_inner <- circle.coord[1] * sin(theta)
    x_outer <- circle.coord[2] * cos(theta)
    y_outer <- circle.coord[2] * sin(theta)

    polygon(c(x_inner,
              rev(x_outer)),
            c(y_inner,
              rev(y_outer)),
            col = color_df$Color[color_df$view == view],
            border = NA)

    # Add text labels for each section
    mid_theta <- ((theta_plot[i] + theta_plot[i+1]) / 2)
    x_text <- circle.coord[3] * cos(mid_theta)
    y_text <- circle.coord[3] * sin(mid_theta)
    text(x = x_text, y = y_text, labels = view)
    i <- i + 1
  }
}
