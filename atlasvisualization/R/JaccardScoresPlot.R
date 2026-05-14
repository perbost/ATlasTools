#' @title Plot a simple graph with specific Jaccard scores for multiple models.
#' @name JaccardScoresPlot
#' @description This function plot a simple graph with Jaccard scores for all
#' models passed in argument.
#' This is a function to compare different model to be take a decision of which
#' model are more better (according this score).
#' @param path_to_save path to save figure
#' @param jaccard_index_tot data frame with all Jaccard index saved
#' @param nb_cluster the number of cluster
#' @param score_type type of score to plot
#' @param plot_indexes (bool) if we plot Jaccard inexes scores (specificity and
#' sensitivity and random index)
#' @param as_pdf (bool) if generate and save pdf figure
#' @param as_png (bool) if generated and save png figure
#' @param width width of the figure
#' @param height height of the figure
#' @export
#'

JaccardScoresPlot <- function(path_to_save, jaccard_index_tot, nb_cluster,
                              score_type="noType", plot_indexes=F, as_pdf=T,
                              as_png=F, width=10, height=7){

  fn = paste0("Jaccard_", score_type, "_plot")

  if(as_pdf){
    pdf(file.path(path_to_save, paste0(fn, ".pdf")),
        width = width,
        height = height)
  } else if (as_png){
    png(file.path(path_to_save, paste0(fn, ".png")),
        width=600,
        height=350)
  }

  if(plot_indexes){
    gg <- ggplot2::ggplot(jaccard_index_tot) +
      ggplot2::geom_line(mapping=ggplot2::aes(x=nb_cluster,
                                              y=sensibility,
                                              color=model)) +
      ggplot2::geom_line(mapping=ggplot2::aes(x=nb_cluster,
                                              y=specificity,
                                              color=model)) +
      ggplot2::geom_line(mapping=ggplot2::aes(x=nb_cluster,
                                              y=random_index,
                                              color="red"),
                         size=2,
                         linetype = "dashed") +
      ggplot2::ylab("Sensitivity/Specificity") + ggplot2::theme_bw()
  }
  else{
    gg <- ggplot2::ggplot(jaccard_index_tot) +
      ggplot2::geom_line(mapping=ggplot2::aes(x=nb_cluster,
                                              y=score_type,
                                              color=model)) +
      ggplot2::ylab(score_type) + ggplot2::theme_bw()
  }
  print(gg)
  dev.off()
}
