#' @title
#' @name ModelAnalysisPlot
#' @description
#' @param MofaObject
#' @param path.save
#' @param plot.data.overview
#' @param plot.variance.explained
#' @param plot.top.weights
#' @param plot.weights
#' @param plot.heatmap
#' @param plot.VarianceByFactor
#' @param plot.GSEA.GO
#' @param plot.overallVarianceExplained_1
#' @param plot.overallVarianceExplained_2
#' @param plot.factorCorrelation
#' @param plot.factorClinicalCorrelation
#' @param plot.dataHeatmapDenoise
#' @param plot.models_comparaison_factors
#' @param plot.GSEA.Reactome
#' @param plot.factor_grid
#' @param plot.dataTopWeights
#' @param res.all.to.print
#' @param plot.dataDataVarianceExplained
#' @param width (float) width of the figure.
#' @param height (float) height of the figure.
#' @param not.denoise
#' @param features
#' @param as_pdf (bool) if generate and save pdf figure
#' @param as_png (bool) if generated and save png figure
#' @param scale
#' @param abs
#' @param show_colnames (bool) if TRUE show sample names on heatmaps
#' @param file.name
#' @param factor
#' @param view
#' @param nfeatures
#' @param path.model
#' @param clinical
#' @param models.compare
#' @param result
#' @param models.compare
#' @param additional.variance.loss
#' @export


ModelAnalysisPlot <- function(MofaObject=NULL, path.save=NULL,
                              plot.data.overview=F,
                              plot.variance.explained=F, plot.top.weights=F,
                              plot.weights=F, plot.heatmap=F,
                              plot.VarianceByFactor=F,plot.GSEA.GO=F,
                              plot.overallVarianceExplained_1=F,
                              plot.overallVarianceExplained_2=F,
                              plot.factorCorrelation=F,
                              plot.factorClinicalCorrelation=F,
                              plot.dataHeatmapDenoise=F,
                              plot.models_comparaison_factors=F,
                              plot.GSEA.Reactome=F, plot.factor_grid=F,
                              plot.dataTopWeights=F, res.all.to.print=NULL,
                              plot.dataDataVarianceExplained=F, width=7,
                              height=7, not.denoise=NULL, features=NULL,
                              as.pdf=T, as.png=F, scale=T, abs=F,
                              show_colnames=F,
                              file.name="unnamed.pdf", factor=NULL, view=NULL,
                              nfeatures=10, path.model=NULL, clinical=NULL,
                              models.compare=NULL, result=NULL,
                              additional.variance=F,
                              additional.variance.loss=F){

  if(as.pdf){
    pdf(file.path(path.save, file.name),
        width=width,
        height=height)
  } else if(as.png){
    png(file.path(path.save, file.name),
        width=600,
        height=350)
  }

  if(plot.data.overview) print(MOFA2::plot_data_overview(MofaObject))
  if(plot.variance.explained) print(MOFA2::plot_variance_explained(MofaObject,
                                                              x="view",
                                                              y="factor"))
  if(plot.top.weights) print(MOFA2::plot_top_weights(MofaObject,
                                                     view=view,
                                                     factor=factor,
                                                     nfeatures=nfeatures))
  if(plot.weights) print(MOFA2::plot_weights(MofaObject,
                                             view=view,
                                             factor=factor,
                                             nfeatures=nfeatures,
                                             scale=scale,
                                             abs=F))
  if(plot.weights) print(MOFA2::plot_weights(MofaObject,
                                             view=view,
                                             factor=factor,
                                             nfeatures=nfeatures,
                                             scale=scale,
                                             abs=F))
  if(plot.heatmap){
    groups <- tryCatch(
      MOFA2:::.check_and_get_groups(MofaObject, "all"),
      error=function(e) NULL
    )

    .safe_group_heatmap <- function(group_name=NULL, cluster_rows=TRUE) {
      heatmap_args <- list(
        object=MofaObject,
        view=view,
        factor=factor,
        features=nfeatures,
        cluster_rows=cluster_rows,
        cluster_cols=FALSE,
        show_rownames=TRUE,
        show_colnames=show_colnames
      )
      if(!is.null(group_name)) heatmap_args$groups <- group_name

      tryCatch(
        do.call(MOFA2::plot_data_heatmap, heatmap_args),
        error = function(e) {
          message(sprintf(
            "plot_data_heatmap failed for view=%s factor=%s group=%s (%s). Retrying with imputed data.",
            as.character(view),
            as.character(factor),
            ifelse(is.null(group_name), "all", as.character(group_name)),
            conditionMessage(e)
          ))

          MofaObjectImputed <- MOFA2::impute(MofaObject)
          heatmap_args$object <- MofaObjectImputed
          heatmap_args$imputed <- FALSE
          heatmap_args$cluster_rows <- FALSE

          tryCatch(
            do.call(MOFA2::plot_data_heatmap, heatmap_args),
            error = function(e2) {
              warning(sprintf(
                "Skipping heatmap for view=%s factor=%s group=%s after errors: %s | %s",
                as.character(view),
                as.character(factor),
                ifelse(is.null(group_name), "all", as.character(group_name)),
                conditionMessage(e),
                conditionMessage(e2)
              ))
              NULL
            }
          )
        }
      )
    }

    if(!is.null(groups) && length(groups) > 1){
      plotlist <- list()
      plotgroups <- c()

      for(g in groups){
        p <- .safe_group_heatmap(group_name=g, cluster_rows=FALSE)
        if(!is.null(p)){
          plotlist[[length(plotlist)+1]] <- p
          plotgroups <- c(plotgroups, g)
        }
      }

      if(length(plotlist) > 0){
        print(ggpubr::ggarrange(
          plotlist=plotlist,
          ncol=length(plotlist),
          nrow=1,
          labels=plotgroups
        ))
      } else {
        warning(sprintf(
          "Skipping all heatmaps for view=%s factor=%s (all groups failed).",
          as.character(view), as.character(factor)
        ))
      }
    } else {
      heatmap_plot <- .safe_group_heatmap(group_name=NULL, cluster_rows=TRUE)
      if(!is.null(heatmap_plot)) print(heatmap_plot)
    }
  }
  if(plot.VarianceByFactor){
    if(additional.variance){
      y.label="additional"
      y.title="Additional variance explained (%)"
    }
    else{
      y.label="max.value"
      y.title="Total variance explained (%)"
    }

    if(additional.variance.loss){
      ggline.custom <- ggpubr::ggline(result,
                                       x="nb_factor",
                                       y=y.label,
                                       numeric.x.axis = TRUE,
                                       color="view") +
        ggplot2::geom_smooth(data=result[!grepl("WES",  result$view) &
                                           result$nb_factor!=7,],
                             ggplot2::aes_string(x="nb_factor",  y=y.label),
                             color="red",
                             se=F)
    }
    else ggline.custom <- ggpubr::ggline(result,
                                          x="nb_factor",
                                          y=y.label,
                                          color="view")

    gg = ggline.custom +
      ggplot2::labs(x="Total number of factors", y=y.title) +
      ggplot2::theme(legend.title=ggplot2::element_blank(),
                     legend.position="top",
                     axis.text=ggplot2::element_text(size=ggplot2::rel(0.8)))
    print(gg)
  }
  if(plot.overallVarianceExplained_1)
    print(MOFA2::plot_variance_explained(MofaObject, plot_total=TRUE)[[2]])
  if(plot.overallVarianceExplained_2)
    print(MOFA2::plot_variance_explained(MofaObject, plot_total=TRUE)[[1]])
  if(plot.factorCorrelation) print(MOFA2::plot_factor_cor(MofaObject))
  if(plot.factorClinicalCorrelation)
    print(correlation_factor_clinical(MofaObject, clinical))
  if(plot.dataHeatmapDenoise){
     heatmap_denoise_plot <- tryCatch(
      MOFA2::plot_data_heatmap(MofaObject,
                               view=view,
                               factor=factor,
                               features=nfeatures,
                               denoise=T,
                               cluster_rows=TRUE,
                               cluster_cols=FALSE,
                               show_rownames=TRUE,
                               show_colnames=show_colnames),
      error = function(e) {
        message(sprintf(
          "plot_data_heatmap failed for view=%s factor=%s (%s). Retrying with imputed data.",
          as.character(view), as.character(factor), conditionMessage(e)
        ))
        tryCatch(
          MOFA2::plot_data_heatmap(MofaObject,
                                   view=view,
                                   factor=factor,
                                   features=nfeatures,
                                   denoise=T,
                                   imputed=FALSE,
                                   cluster_rows=FALSE,
                                   cluster_cols=FALSE,
                                   show_rownames=TRUE,
                                   show_colnames=show_colnames),
          error = function(e2) {
            warning(sprintf(
              "Skipping heatmap for view=%s factor=%s after errors: %s | %s",
              as.character(view),
              as.character(factor),
              conditionMessage(e),
              conditionMessage(e2)
            ))
            NULL
          }
        )
      }
    )

    if(!is.null(heatmap_denoise_plot)) print(heatmap_denoise_plot)
  }
  if(plot.dataTopWeights)
    print(MOFA2::plot_top_weights(MofaObject,
                                  view=view,
                                  factor=factor,
                                  nfeatures=nfeatures))
  if(plot.dataDataVarianceExplained)
    print(MOFA2::plot_variance_explained_per_feature(MofaObject,
                                                     view=view,
                                                     factor=factor,
                                                     features=features,
                                                     max_r2=100))
  if(plot.GSEA.GO) print(res.all.to.print)
  if(plot.GSEA.Reactome) print(res.all.to.print)
  if(plot.factor_grid){
    current_model <- tryCatch(get("MofaObject", envir=environment(), inherits=FALSE),
                              error=function(e) NULL)
    if(is.null(current_model)){
      warning("Skipping factor grid: MofaObject is missing.")
    } else {
      n_factors <- MOFA2::get_dimensions(current_model)$K
      pf <- do.call(MOFA2::plot_factors, list(object=current_model, factors=seq_len(n_factors)))
      print(pf)
    }
  }
  if(plot.models_comparaison_factors)
    print(MOFA2::compare_factors(models.compare))

  dev.off()
}
