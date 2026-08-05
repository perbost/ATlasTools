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
#' @param rotate_view_labels_45 (bool) if TRUE rotate MOFA2::plot_variance_explained view labels by 45 degrees.
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
                              additional.variance.loss=F,
                              rotate_view_labels_45=TRUE,
                              max_chars_labels=30) {

  wrap_feature_label <- function(label, max_chars=30) {
    if (is.na(label) || nchar(label) <= max_chars) {
      return(label)
    }

    tokens <- unlist(strsplit(label, "(?<=[ _])", perl=TRUE))
    if (length(tokens) == 0) {
      return(label)
    }

    lines <- character(0)
    current_line <- ""

    for (token in tokens) {
      token_trimmed <- trimws(token)
      candidate <- paste0(current_line, token)

      if (nchar(candidate) <= max_chars || current_line == "") {
        current_line <- candidate
      } else {
        lines <- c(lines, trimws(current_line))
        current_line <- token_trimmed
      }
    }

    if (nzchar(current_line)) {
      lines <- c(lines, trimws(current_line))
    }

    paste(lines, collapse="
")
  }

  wrap_feature_labels_in_plot <- function(plot_obj, max_chars=max_chars_labels) {
    if (!inherits(plot_obj, "ggplot")) {
      return(plot_obj)
    }

    label_wrapper <- function(values) vapply(values, wrap_feature_label, character(1), max_chars=max_chars)

    if (is.data.frame(plot_obj$data)) {
      for (col_name in intersect(c("feature", "features"), names(plot_obj$data))) {
        plot_obj$data[[col_name]] <- label_wrapper(as.character(plot_obj$data[[col_name]]))
      }
    }

    if (length(plot_obj$layers) > 0) {
      for (i in seq_along(plot_obj$layers)) {
        layer_data <- plot_obj$layers[[i]]$data
        if (is.data.frame(layer_data)) {
          for (col_name in intersect(c("feature", "features"), names(layer_data))) {
            layer_data[[col_name]] <- label_wrapper(as.character(layer_data[[col_name]]))
          }
          plot_obj$layers[[i]]$data <- layer_data
        }
      }
    }

    plot_obj +
      ggplot2::scale_x_discrete(labels=label_wrapper) +
      ggplot2::scale_y_discrete(labels=label_wrapper)
  }


  rotate_view_labels_in_plot <- function(plot_obj, rotate_labels=FALSE) {
    if (!isTRUE(rotate_labels) || !inherits(plot_obj, "ggplot")) {
      return(plot_obj)
    }

    plot_obj +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
      )
  }


  plot_data_heatmap_wrapped <- function(object, factor, view=1, groups="all",
                                        features=max_chars_labels, annotation_features=NULL,
                                        annotation_samples=NULL, transpose=FALSE,
                                        imputed=FALSE, denoise=FALSE,
                                        max.value=NULL, min.value=NULL, ...) {
    if (!is(object, "MOFA")) {
      stop("'object' has to be an instance of MOFA")
    }

    stopifnot(length(factor) == 1)
    stopifnot(length(view) == 1)

    groups <- MOFA2:::.check_and_get_groups(object, groups)
    factor <- MOFA2:::.check_and_get_factors(object, factor)
    view <- MOFA2:::.check_and_get_views(object, view)

    W <- do.call(rbind, MOFA2::get_weights(object, views=view, factors=factor, as.data.frame=FALSE))
    Z <- lapply(MOFA2::get_factors(object)[groups], function(z) as.matrix(z[, factor]))
    Z <- do.call(rbind, Z)[, 1]
    Z <- Z[!is.na(Z)]

    if (isTRUE(denoise)) {
      data <- predict(object, views=view, groups=groups)[[1]]
    } else if (isTRUE(imputed)) {
      data <- MOFA2::get_imputed_data(object, view, groups)[[1]]
    } else {
      data <- MOFA2::get_data(object, views=view, groups=groups)[[1]]
    }

    if (is(data, "list")) {
      data <- do.call(cbind, data)
    }

    if (is(features, "numeric")) {
      if (length(features) == 1) {
        features <- rownames(W)[tail(order(abs(W)), n=features)]
      } else {
        features <- rownames(W)[order(-abs(W))[features]]
      }
      features <- names(W[features, ])[order(W[features, ])]
    } else if (is(features, "character")) {
      stopifnot(all(features %in% MOFA2::features_names(object)[[view]]))
    } else {
      stop("Features need to be either a numeric or character vector")
    }

    data <- data[features, ]
    data <- data[, names(Z)]
    data <- data[, apply(data, 2, function(x) !all(is.na(x)))]

    order_samples <- names(sort(Z, decreasing=TRUE))
    order_samples <- order_samples[order_samples %in% colnames(data)]
    data <- data[, order_samples]

    if (!is.null(annotation_samples)) {
      if (is.data.frame(annotation_samples)) {
        message("'annotation_samples' provided as a data.frame, please make sure that the rownames match the sample names")
        if (any(!colnames(data) %in% rownames(annotation_samples))) {
          stop("There are rownames in annotation_samples that do not correspond to sample names in the model")
        }
        annotation_samples <- annotation_samples[colnames(data), , drop=FALSE]
      } else if (is.character(annotation_samples)) {
        stopifnot(annotation_samples %in% colnames(object@samples_metadata))
        tmp <- object@samples_metadata
        rownames(tmp) <- tmp$sample
        tmp$sample <- NULL
        tmp <- tmp[order_samples, , drop=FALSE]
        annotation_samples <- tmp[, annotation_samples, drop=FALSE]
        rownames(annotation_samples) <- rownames(tmp)
      } else {
        stop("Input format for 'annotation_samples' not recognised ")
      }

      foo <- sapply(annotation_samples, function(x) is.logical(x) || is.character(x))
      if (any(foo)) {
        annotation_samples[, which(foo)] <- lapply(annotation_samples[, which(foo), drop=FALSE], as.factor)
      }
    }

    if (!is.null(annotation_features)) {
      stop("'annotation_features' is currently not implemented")
    }

    if (transpose) {
      data <- t(data)
      if (!is.null(annotation_samples)) {
        annotation_features <- annotation_samples
        annotation_samples <- NULL
      }
      if (!is.null(annotation_features)) {
        annotation_samples <- annotation_features
        annotation_features <- NULL
      }
    }

    if (!is.null(max.value)) {
      data[data >= max.value] <- max.value
    }
    if (!is.null(min.value)) {
      data[data <= min.value] <- min.value
    }

    if (nrow(data) == 0 || ncol(data) == 0 || all(is.na(as.matrix(data)))) {
      stop("No heatmap data available after preprocessing")
    }

    rownames(data) <- vapply(rownames(data), wrap_feature_label, character(1), max_chars=max_chars_labels)

    pheatmap::pheatmap(
      data,
      annotation_row=annotation_features,
      annotation_col=annotation_samples,
      ...
    )
  }

  if(!is.null(path.save) && !dir.exists(path.save)) {
    dir.create(path.save, recursive = TRUE, showWarnings = FALSE)
  }

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
  if(plot.variance.explained) print(rotate_view_labels_in_plot(
    MOFA2::plot_variance_explained(MofaObject,
                                   x="view",
                                   y="factor"),
    rotate_labels=rotate_view_labels_45
  ))
  if(plot.top.weights) print(wrap_feature_labels_in_plot(
    MOFA2::plot_top_weights(MofaObject,
                            view=view,
                            factor=factor,
                            nfeatures=nfeatures)
  ))
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
        do.call(plot_data_heatmap_wrapped, heatmap_args),
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
            do.call(plot_data_heatmap_wrapped, heatmap_args),
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
        ggplot2::geom_smooth(data=result[!grepl("WES",  result$view),],
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
    use_denoise <- is.null(not.denoise) || !(view %in% not.denoise)

    heatmap_denoise_plot <- tryCatch(
      plot_data_heatmap_wrapped(MofaObject,
                               view=view,
                               factor=factor,
                               features=nfeatures,
                               denoise=use_denoise,
                               cluster_rows=TRUE,
                               cluster_cols=FALSE,
                               show_rownames=TRUE,
                               show_colnames=show_colnames),
      error = function(e) {
        fallback_label <- if (isTRUE(use_denoise)) "non-denoised data" else "raw data"
        message(sprintf(
          "plot_data_heatmap failed for view=%s factor=%s (%s). Retrying with %s.",
          as.character(view), as.character(factor), conditionMessage(e), fallback_label
        ))
        tryCatch(
          plot_data_heatmap_wrapped(MofaObject,
                                   view=view,
                                   factor=factor,
                                   features=nfeatures,
                                   denoise=FALSE,
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
    print(wrap_feature_labels_in_plot(
      MOFA2::plot_top_weights(MofaObject,
                              view=view,
                              factor=factor,
                              nfeatures=nfeatures)
    ))
  if(plot.dataDataVarianceExplained)
    print(wrap_feature_labels_in_plot(
      MOFA2::plot_variance_explained_per_feature(MofaObject,
                                                 view=view,
                                                 factor=factor,
                                                 features=features,
                                                 max_r2=100)
    ))
  if(plot.GSEA.GO) print(res.all.to.print)
  if(plot.GSEA.Reactome) print(res.all.to.print)
  if(plot.factor_grid){
    current_model <- tryCatch(get("MofaObject", envir=environment(), inherits=FALSE),
                              error=function(e) NULL)
    if(is.null(current_model)){
      warning("Skipping factor grid: MofaObject is missing.")
    } else {
      n_factors <- MOFA2::get_dimensions(current_model)$K
      factor_df <- MOFA2::get_factors(current_model, factors=seq_len(n_factors), as.data.frame=TRUE)
      duplicate_factor_rows <- factor_df %>%
        dplyr::count(sample, factor, name = "n") %>%
        dplyr::filter(.data$n > 1)

      if(nrow(duplicate_factor_rows) == 0){
        pf <- do.call(MOFA2::plot_factors, list(object=current_model, factors=seq_len(n_factors)))
        print(pf)
      } else {
        warning(sprintf(
          "Detected %d duplicated sample/factor rows; plotting deduplicated factor grid with mean factor values.",
          nrow(duplicate_factor_rows)
        ))

        factor_wide <- factor_df %>%
          dplyr::group_by(sample, factor) %>%
          dplyr::summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
          tidyr::pivot_wider(names_from = factor, values_from = value) %>%
          dplyr::arrange(sample)

        factor_matrix <- factor_wide %>%
          dplyr::select(-sample) %>%
          as.data.frame()

        graphics::pairs(
          factor_matrix,
          main = "MOFA factor grid (deduplicated)",
          pch = 16,
          cex = 0.6
        )
      }
    }
  }
  if(plot.models_comparaison_factors)
    print(MOFA2::compare_factors(models.compare))

  dev.off()
}
