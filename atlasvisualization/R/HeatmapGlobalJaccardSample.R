#' @title Heatmap (n_sample * n_sample) with Jaccard distances.
#' @name HeatmapGlobalJaccardSample
#' @description This function create a plot with heat map information.
#' This is a matrix (n_sample * n_sample) with whit color for small Jaccard
#' distance values and red for high distances.
#' @param jc_sample matrix with heatmap information from pheatmap library
#' @param nseed (int) number of generated maps
#' @param path_to_save path to save figure
#' @param as_pdf (bool) if generate and save pdf figure
#' @param as_png (bool) if generated and save png figure
#' @param file_name name of the file to generate
#' @param title title of the figure
#' @param width width of the figure
#' @param height height of the figure
#' @export
#' @example


HeatmapGlobalJaccardSample <- function(jc_sample, nseed, path_to_save, as_pdf=T,
                                       as_png=F,
                                       file_name="heatmap_global_jaccard_sample",
                                       width=15, height=13){

  fn = paste0(file_name, sprintf("_%d", nseed))

  if(as_pdf){
    pdf(file.path(path_to_save, paste0(fn, ".pdf")),
        width = width,
        height = height)
  } else if (as_png){
    png(file.path(path_to_save, paste0(fn, ".png")),
        width=600,
        height=350)
  }

  print(jc_sample)
  dev.off()
}
