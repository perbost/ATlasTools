#' @title Function to plot cumulative r2 variance.
#' @name CumulativeVarianceExplained
#' @description We plot curves with cumulative r2 variance depending of the
#' factor number.
#' @param r2.dt
#' @param path.save
#' @param as_pdf (bool) if generate and save pdf figure
#' @param as_png (bool) if generated and save png figure
#' @param width width of the figure
#' @param height height of the figure
#' @param file.name
#' @export

CumulativeVarianceExplained <- function(r2.dt, path.save, as.pdf=T, as.png=F,
                                        height=7, width=7,
                                        file.name="unnamed.pdf"){

  if(as.pdf){
    pdf(file.path(path.save, file.name),
        width=width,
        height=height)
  } else if(as.png){
    png(file.path(path.save, file.name),
        width=600,
        height=350)
  }

  gg= ggpubr::ggline(r2.dt, x="factor", y="cum_r2", color="view") +
    ggplot2::labs(x="Factor number", y="Cumulative variance explained (%)") +
    ggplot2::theme(legend.title=ggplot2::element_blank(),
                   legend.position="top",
                   axis.text=ggplot2::element_text(size=ggplot2::rel(0.8)))
  print(gg)

  dev.off()
}
