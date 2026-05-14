#' @title Several MOFA model analysis to explain model capacity.
#' @name MOFAModelAnalysis
#' @description We explore different measure of model capacity to understand
#' how work this model and his performances.
#' @param model (MOFA) MOFA model trained containing weights to extract.
#' @param path.result path to save generated plot
#' @param Clinical (bool) to generate additional clinical-factor correlation
#' @param perform.GSEA (bool) to generate additional plot about GSEA
#' @param not.denoise (list) list of view that should not be denoised.
#' @param list.view.GSEA (list) we generate GSEA outputs only for feature in
#' this list.
#' @export


MOFAModelAnalysis <- function(model, path.result, Clinical=NA, perform.GSEA=F,
                              not.denoise=NULL, list.view.GSEA=NULL){
  
  .AtlasLoadData("GO_c5", envir=environment())
  .AtlasLoadData("GO_reactome", envir=environment())
  
  MofaModelAnalysis(model, 
                                        path.save=file.path(path.result,
                                                            "Analysis"), 
                                        plot.overallVarianceExplained_1=T,
                                        file.name=
                                           "overall_variance_explained.pdf",
                                        width=10,
                                        height=4)
  
  MofaModelAnalysis(model, 
                                        path.save=file.path(path.result,
                                                            "Analysis"),  
                                        plot.factorCorrelation=T,
                                        file.name="factor_correlation.pdf",
                                        width=10,
                                        height=10)
  
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
    MofaModelAnalysis(model, 
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
      
      MofaModelAnalysis(
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
        not.denoise=not.denoise
        )
      
      MofaModelAnalysis(model, 
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
      
      MofaModelAnalysis(model, 
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
        if(view %in% list.view.GSEA){
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
                                                            name='C5_all')
          
          res_all_GO_reactome = MOFAModelGSEACalcul(gene_list,
                                                                  GO_reactome, 
                                                                  pval=0.05,
                                                            name="C2_Reactome")
          
          if(nrow(res_all_GO_c5[[1]])>0){
            MofaModelAnalysis(model, 
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
          }
          if(nrow(res_all_GO_reactome[[1]])>0){
            MofaModelAnalysis(
              path.save=path_save, 
              plot.GSEA.Reactome=T,
              file.name=sprintf("%s_f%02d_GSEA_Reactome.pdf",
                                view=view,
                                factor=factor),
              width=15,
              height=7,
              res.all.to.print=res_all_GO_reactome[[2]])
          }
        }
      }
    }
  }
}
