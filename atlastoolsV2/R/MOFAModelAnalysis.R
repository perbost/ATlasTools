#' @title Several MOFA model analysis to explain model capacity.
#' @name MOFAModelAnalysis
#' @description We explore different measure of model capacity to understand
#' how work this model and his performances.
#' @param model (MOFA) MOFA model trained containing weights to extract.
#' @param path.result path to save generated plot
#' @param Clinical (bool) to generate additional clinical-factor correlation
#' @param perform.GSEA (bool) to generate additional plot about GSEA
#' @param not.denoise (list) list of view that should not be denoised.
#' @param show_colnames (bool) if TRUE show sample names on heatmaps.
#' @export

MOFAModelAnalysis <- function(model, path.result, Clinical=NA, perform.GSEA=F,
                              not.denoise=NULL,
                              show_colnames=T,
                              fast.mode=TRUE,
                              gsea.nperm=10000,
                              gsea.collapse.nperm=500,
                              max.enrichment.plots=Inf){
  
  .AtlasLoadData("GO_c5", envir=environment())
  .AtlasLoadData("GO_reactome", envir=environment())

  if(isTRUE(fast.mode)){
    if(missing(gsea.nperm)) gsea.nperm <- 2000
    if(missing(gsea.collapse.nperm)) gsea.collapse.nperm <- 100
    if(missing(max.enrichment.plots)) max.enrichment.plots <- 30
  }
  
  .ModelAnalysisPlot(model,
                                        path.save=file.path(path.result,
                                                            "Analysis"),
                                        plot.overallVarianceExplained_1=T,
                                        file.name=
                                           "overall_variance_explained.pdf",
                                        width=10,
                                        height=4)
  
  .ModelAnalysisPlot(model,
                                        path.save=file.path(path.result,
                                                            "Analysis"),
                                        plot.factorCorrelation=T,
                                        file.name="factor_correlation.pdf",
                                        width=10,
                                        height=10)

  .ModelAnalysisPlot(model,
                                        path.save=file.path(path.result,
                                                            "Analysis"),
                                        plot.factor_grid=T,
                                        file.name=
                                           "factor_grid.pdf",
                                        width=MOFA2::get_dimensions(model)$K,
                                        height=MOFA2::get_dimensions(model)$K)                                     
  
  dt.r2 <- MOFAModelExtractCumulativeR2(model)
  CumulativeVarianceExplained(dt.r2,
                                                  path.save=
                                                    file.path(path.result,
                                                              "Analysis"),
                                                  file.name=
                                            "cumulative_variance_explained.pdf",
                                                  width=6,
                                                  height=6)
  
  if(!is.na(Clinical)){
    .ModelAnalysisPlot(model,
                                          path.save=file.path(path.result,
                                                              "Analysis"),
                                          plot.factorClinicalCorrelation=T,
                                          file.name=
                                           "factor_clinical_correlation.pdf",
                                          width=12,
                                          height=10,
                                          clinical=Clinical)
  }
  
  for(view in names(MOFA2::get_dimensions(model)$D)){
    for(factor in 1:MOFA2::get_dimensions(model)$K){
      
      path_save <- file.path(path.result, sprintf("Factor_%i", factor))
      
      .ModelAnalysisPlot(
        model,
        path.save=path_save,
        plot.dataHeatmapDenoise=T,
        file.name=
          sprintf("%s_f%02d_Data_Heatmap_denoise.pdf",
                  view,
                  factor),
        width=10,
        height=10,
        view=view,
        factor=factor,
        nfeatures=10,
        not.denoise=not.denoise,
        show_colnames=show_colnames
        )
      
      .ModelAnalysisPlot(model,
                                            path.save=path_save,
                                            plot.dataTopWeights=T,
                                            file.name=
                                               sprintf(
                                               "%s_f%02d_top_weights.pdf",
                                                       view=view,
                                                       factor=factor),
                                            nfeatures=10,
                                            view=view,
                                            factor=factor,)

      W <- MOFA2::get_weights(model,
                              factors=factor,
                              views=view,
                              as.data.frame=TRUE)
      W <- W[with(W, order(-abs(value))), ]
      W <- as.data.frame(dplyr::top_n(dplyr::group_by(W, factor),
                                      n=10,
                                      wt=abs(value)))
      
      .ModelAnalysisPlot(model,
                                            path.save=path_save,
                                            plot.dataDataVarianceExplained=T,
                                            file.name=
                                              sprintf(
                                             "%s_f%02d_variance_explained.pdf",
                                                  view=view,
                                                  factor=factor),
                                            nfeatures=10,
                                            view=view,
                                            factor=1,
                                            features=
                                              rev(as.character(W$feature)))
    
      if(perform.GSEA){
        if(grepl("RNA", view, ignore.case=TRUE)){
          W <- MOFA2::get_weights(model,
                                  factors=factor,
                                  views=view,
                                  as.data.frame=TRUE)
          W$value <- W$value/max(abs(W$value))
          W$sign <- ifelse(W$value > 0, "+", "-")
          W <- W[with(W, order(-value)), ]
          W$gene = read.table(text=as.character(W$feature),sep="_")$V1
          
          gene_list = W$value
          names(gene_list) = W$gene
          
          res_all_GO_c5 = MOFAModelGSEACalcul(gene_list,
                                                            GO_c5,
                                                            pval=0.05,
                                                            name='C5_all',
                                                            nperm.fgsea=gsea.nperm,
                                                            nperm.collapse=gsea.collapse.nperm)
          
          res_all_GO_reactome = MOFAModelGSEACalcul(gene_list,
                                                                  GO_reactome,
                                                                  pval=0.05,
                                                            name="C2_Reactome",
                                                            nperm.fgsea=gsea.nperm,
                                                            nperm.collapse=gsea.collapse.nperm)

          path_gsea <- file.path(path_save, "GSEA")
          if(!dir.exists(path_gsea)) dir.create(path_gsea, recursive=TRUE)

          sanitize_for_export <- function(df){
            if(is.null(df)) return(df)
            for(col in names(df)){
              if(is.list(df[[col]])){
                df[[col]] <- vapply(df[[col]], function(x){
                  if(length(x) == 0 || all(is.na(x))) return("")
                  paste(as.character(x), collapse=",")
                }, character(1))
              }
            }
            df
          }

          fgsea_all_c5 <- sanitize_for_export(res_all_GO_c5$fgsea_all)
          fgsea_all_reactome <- sanitize_for_export(res_all_GO_reactome$fgsea_all)
          sig_c5 <- sanitize_for_export(res_all_GO_c5$Results)
          sig_reactome <- sanitize_for_export(res_all_GO_reactome$Results)

          prefix <- sprintf("%s_f%02d", view, factor)
          utils::write.table(fgsea_all_c5,
                             file=file.path(path_gsea,
                                            sprintf("%s_C5_all_fgsea_all.tsv", prefix)),
                             sep="\t", quote=FALSE, row.names=FALSE)
          utils::write.table(res_all_GO_c5$gene_stats,
                             file=file.path(path_gsea,
                                            sprintf("%s_C5_all_gene_stats.tsv", prefix)),
                             sep="\t", quote=FALSE, row.names=FALSE)
          utils::write.table(fgsea_all_reactome,
                             file=file.path(path_gsea,
                                            sprintf("%s_C2_Reactome_fgsea_all.tsv", prefix)),
                             sep="\t", quote=FALSE, row.names=FALSE)
          utils::write.table(res_all_GO_reactome$gene_stats,
                             file=file.path(path_gsea,
                                            sprintf("%s_C2_Reactome_gene_stats.tsv", prefix)),
                             sep="\t", quote=FALSE, row.names=FALSE)
          utils::write.table(sig_c5,
                             file=file.path(path_gsea,
                                            sprintf("%s_C5_all_significant.tsv", prefix)),
                             sep="\t", quote=FALSE, row.names=FALSE)
          utils::write.table(sig_reactome,
                             file=file.path(path_gsea,
                                            sprintf("%s_C2_Reactome_significant.tsv", prefix)),
                             sep="\t", quote=FALSE, row.names=FALSE)

          if(nrow(res_all_GO_c5[[1]])>0){
            .ModelAnalysisPlot(model,
                                                  path.save=path_save,
                                                  plot.GSEA.GO=T,
                                                  file.name=
                                                    sprintf(
                                                      "%s_f%02d_GSEA_GO.pdf",
                                                            view=view,
                                                            factor=factor),
                                                  width=15,
                                                  height=7,
                                                  res.all.to.print=
                                                    res_all_GO_c5[[2]])

            n_plot_c5 <- min(nrow(res_all_GO_c5$Results), max.enrichment.plots)
            for(i in seq_len(n_plot_c5)){
              pathway_id <- res_all_GO_c5$Results$pathway_id[i]
              pathway_label <- gsub("[^A-Za-z0-9_\\-]", "_", res_all_GO_c5$Results$pathway[i])
              pp <- fgsea::plotEnrichment(GO_c5[[pathway_id]], gene_list) +
                ggplot2::labs(title=sprintf("%s - %s", prefix, res_all_GO_c5$Results$pathway[i]))
              ggplot2::ggsave(filename=file.path(path_gsea,
                                                 sprintf("%s_C5_all_%03d_%s_enrichment.pdf", prefix, i, pathway_label)),
                              plot=pp, width=8, height=5)
            }
          }
          if(nrow(res_all_GO_reactome[[1]])>0){
            .ModelAnalysisPlot(
              path.save=path_save,
              plot.GSEA.Reactome=T,
              file.name=sprintf("%s_f%02d_GSEA_Reactome.pdf",
                                view=view,
                                factor=factor),
              width=15,
              height=7,
              res.all.to.print=res_all_GO_reactome[[2]])

            n_plot_reactome <- min(nrow(res_all_GO_reactome$Results), max.enrichment.plots)
            for(i in seq_len(n_plot_reactome)){
              pathway_id <- res_all_GO_reactome$Results$pathway_id[i]
              pathway_label <- gsub("[^A-Za-z0-9_\\-]", "_", res_all_GO_reactome$Results$pathway[i])
              pp <- fgsea::plotEnrichment(GO_reactome[[pathway_id]], gene_list) +
                ggplot2::labs(title=sprintf("%s - %s", prefix, res_all_GO_reactome$Results$pathway[i]))
              ggplot2::ggsave(filename=file.path(path_gsea,
                                                 sprintf("%s_C2_Reactome_%03d_%s_enrichment.pdf", prefix, i, pathway_label)),
                              plot=pp, width=8, height=5)
            }
          }
        }
      }
    }
  }
}
