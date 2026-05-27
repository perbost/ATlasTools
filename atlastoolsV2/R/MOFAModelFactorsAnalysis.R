#' @title Explore top factor correlated to features and plot figures.
#' @name MOFAModelFactorsAnalysis
#' @description We compute top features correlated to the different factors.
#' Then we analyse this correlation and plot figures.
#' @param path.model path to the trained model to load
#' @param path.model.factor path to the factor folder of the current model
#' @param keep.group (bool) If we want keep only samples of specific group
#' @param show_colnames (bool) If TRUE show sample names on heatmaps
#' @export


MOFAModelFactorsAnalysis <- function(path.model, path.model.factor,
                                     keep.group=NULL,
                                     show_colnames=T){

  model <- MOFA2::load_model(path.model)

  if(!is.null(keep.group)){
    model <- MOFA2::subset_groups(model, keep.group)
  }

  .ModelAnalysisPlot(model,
                                        path.save=path.model.factor,
                                        plot.variance.explained=T,
                                        file.name="Variance_explained.pdf")

  for(factor in 1:model@dimensions$K){

    path.factor <- file.path(path.model.factor, sprintf("Factor_%d", factor))
    if(dir.exists(path.factor)) unlink(path.factor, recursive=TRUE)
    dir.create(path.factor)

    for(view in names(model@data)){

      .ModelAnalysisPlot(model,
                                            path.save=path.factor,
                                            plot.top.weights=T,
                                            file.name=
                                              sprintf("top_weights_%s_%s.pdf",
                                                      view,
                                                      factor),
                                            factor=factor,
                                            view=view)

      .ModelAnalysisPlot(model,
                                            path.save=path.factor,
                                            plot.weights=T,
                                            file.name=
                                              sprintf("weights_%s_%s.pdf",
                                                      view,
                                                      factor),
                                            factor=factor,
                                            view=view,
                                            width=15,
                                            height=15,
                                            scale=T,
                                            abs=F,
                                            nfeatures=10)

      .ModelAnalysisPlot(model,
                                            path.save=path.factor,
                                            plot.heatmap=T,
                                            file.name=
                                              sprintf("Data_Heatmap_%s_%s.pdf",
                                                      view,
                                                      factor),
                                            factor=factor,
                                            view=view,
                                            height=10,
                                            scale=T,
                                            abs=F,
                                            nfeatures=10,
                                            show_colnames=show_colnames)
    }
  }
}
