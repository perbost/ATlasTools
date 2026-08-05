#' @title Correlate MOFA factors with covariates using linear models
#' @name correlation_factor_clinical
#' @description Similar to \\code{MOFA2::correlate_factors_with_covariates},
#' but p-values are computed from \\code{lm()} models instead of correlation tests.
#' @param object (MOFA) Trained MOFA object.
#' @param covariates (data.frame) Covariate table. Row names must match sample names.
#' @param factors (vector) Factors to test. Default is all factors.
#' @param adjust.method (str) P-value adjustment method passed to
#' \\code{stats::p.adjust}.
#' @param use.adjusted.p (bool) If TRUE, plot adjusted p-values, otherwise raw p-values.
#' @param use.estimate.sign (bool) If TRUE, use the sign of the estimate in the plotted score. If FALSE, use unsigned \code{-log10(p)}.
#' @param min.pvalue (float) Lower bound to avoid infinite \\code{-log10(p)}.
#' @param bool_export_table (bool) If TRUE, export and return the result table
#' instead of returning a plot.
#' @param path.save (str) Directory where the table is exported when
#' \\code{bool_export_table=TRUE}.
#' @param file.name (str) Output file name for table export.
#' @return A ggplot heatmap or a data.frame table when \\code{bool_export_table=TRUE}.
#' @export
correlation_factor_clinical <- function(
    object,
    covariates,
    factors = "all",
    adjust.method = "BH",
    use.adjusted.p = FALSE,
    use.estimate.sign = TRUE,
    min.pvalue = 1e-6,
    p.value.thr = 0.05,
    bool_export_table = FALSE,
    path.save = ".",
    file.name = "correlation_factor_clinical_lm.tsv"
) {
  if (!is(object, "MOFA")) {
    stop("'object' has to be an instance of MOFA")
  }

  if (is.null(covariates) || !is.data.frame(covariates)) {
    stop("'covariates' must be a data.frame with sample row names")
  }

  if (is.null(rownames(covariates))) {
    stop("'covariates' must have row names corresponding to sample names")
  }

  factor_df <- MOFA2::get_factors(object, factors = factors, as.data.frame = TRUE)
  factor_df <- factor_df %>%
    dplyr::group_by(sample, factor) %>%
    dplyr::summarise(value = mean(value, na.rm = TRUE), .groups = "drop")
  factor_wide <- tidyr::pivot_wider(
    factor_df,
    id_cols = "sample",
    names_from = "factor",
    values_from = "value"
  )
  if (!"sample" %in% colnames(covariates)) {
    covariates$sample <- rownames(covariates)
  } else {
    covariates$sample <- as.character(covariates$sample)
    missing_sample <- is.na(covariates$sample) | covariates$sample == ""
    if (any(missing_sample)) {
      covariates$sample[missing_sample] <- rownames(covariates)[missing_sample]
    }
  }
  dat <- merge(factor_wide, covariates, by = "sample")

  factor_names <- colnames(factor_wide)[colnames(factor_wide) != "sample"]
  covar_names <- setdiff(colnames(covariates), "sample")

  res_list <- list()
  idx <- 1
  for (f in factor_names) {
    for (cv in covar_names) {
      tmp <- dat[, c(f, cv)]
      colnames(tmp) <- c("factor_value", "covariate")
      tmp <- tmp[stats::complete.cases(tmp), , drop = FALSE]
      if (nrow(tmp) < 3) {
        p_val <- NA_real_
        estimate <- NA_real_
      } else {
        
        fit <- tryCatch(stats::lm(factor_value ~ covariate, data = tmp), error = function(e) NULL)
        if (is.null(fit)) {
          p_val <- NA_real_
          estimate <- NA_real_
        } else {
          sm <- summary(fit)$coefficients
          if (nrow(sm) < 2) {
            p_val <- NA_real_
            estimate <- NA_real_
          } else {
            p_val <- sm[2, 4]
            estimate <- sm[2, 1]
          }
        }
      }

      res_list[[idx]] <- data.frame(
        factor = f,
        covariate = cv,
        estimate = estimate,
        p.value = p_val,
        stringsAsFactors = FALSE
      )
      idx <- idx + 1
    }
  }

  res <- do.call(rbind, res_list)
  res$adj.p.value <- stats::p.adjust(res$p.value, method = adjust.method)

  p_used <- if (isTRUE(use.adjusted.p)) res$adj.p.value else res$p.value
  p_used[is.na(p_used)] <- 1
  p_used[p_used > p.value.thr] <- 1
  p_used <- pmax(p_used, min.pvalue)
  res$score <- -log10(p_used)
  if (isTRUE(use.estimate.sign)) {
    res$score <- res$score * sign(res$estimate)
  }

  if (isTRUE(bool_export_table)) {
    if (!dir.exists(path.save)) dir.create(path.save, recursive = TRUE)
    out_file <- file.path(path.save, file.name)
    utils::write.table(
      res,
      file = out_file,
      sep = "\t",
      quote = FALSE,
      row.names = FALSE
    )
    return(res)
  }
  fill_scale <- if (isTRUE(use.estimate.sign)) {
    ggplot2::scale_fill_gradient2(
      low = "#2166ac",
      mid = "white",
      high = "#b2182b",
      midpoint = 0,
      name = ifelse(use.adjusted.p, "sign(beta) * -log10(adj p)", "sign(beta) * -log10(p)")
    )
  } else {
    ggplot2::scale_fill_gradient(
      low = "white",
      high = "#b2182b",
      name = ifelse(use.adjusted.p, "-log10(adj p)", "-log10(p)")
    )
  }

  ggplot2::ggplot(res, ggplot2::aes(x = covariate, y = factor, fill = score)) +
    ggplot2::geom_tile(color = "lightgrey") +
    fill_scale +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      panel.grid = ggplot2::element_blank()
    ) +
    ggplot2::labs(x = "Covariates", y = "Factors")
}
