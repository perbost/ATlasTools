#' @title Function title to complete
#' @name MOFAModelGSEACalcul
#' @description Function description to complete
#' @param gene_list
#' @param myGO
#' @param pval
#' @param name
#' @export
#' @return output


MOFAModelGSEACalcul <- function(gene_list, myGO, pval, name=NULL,
                              nperm.fgsea=10000, nperm.collapse=500){
  
  require(dplyr)
  
  set.seed(54321)
  if(any( duplicated(names(gene_list)))){
    warning("Duplicates in gene names")
    gene_list = gene_list[!duplicated(names(gene_list))]
  }
  if(!all( order(gene_list, decreasing = TRUE) == 1:length(gene_list))){
    warning("Gene list not sorted")
    gene_list = sort(gene_list, decreasing = TRUE)
  }

  fgRes_all <- fgsea::fgsea(pathways = myGO,
                            stats = gene_list,
                            minSize=15,
                            maxSize=600,
                            nperm=nperm.fgsea) %>%
    as.data.frame()

  fgRes <- fgRes_all %>%
    dplyr::filter(padj < !!pval)
  
  ## Filter FGSEA by using gage results. Must be significant and in same
  # direction to keep
  gaRes = gage::gage(gene_list, gsets=myGO, same.dir=TRUE, set.size =c(15,600))
  
  ups = as.data.frame(gaRes$greater) %>%
    tibble::rownames_to_column("Pathway") %>%
    dplyr::filter(!is.na(p.geomean) & q.val < pval ) %>%
    dplyr::select("Pathway")
  
  downs = as.data.frame(gaRes$less) %>%
    tibble::rownames_to_column("Pathway") %>%
    dplyr::filter(!is.na(p.geomean) & q.val < pval ) %>%
    dplyr::select("Pathway")
  
  keepups = fgRes[fgRes$NES > 0 & !is.na(match(fgRes$pathway, ups$Pathway)), ]
  keepdowns =
    fgRes[fgRes$NES < 0 & !is.na(match(fgRes$pathway, downs$Pathway)), ]
  
  ### Collapse redundant pathways
  Up = MOFAModelCollapsePathway(keepups,
                                              pathways = myGO,
                                              stats = gene_list,
                                              nperm = nperm.collapse,
                                              pval.threshold = 0.05)
  Down = MOFAModelCollapsePathway(keepdowns,
                                                myGO,
                                                gene_list,
                                                nperm = nperm.collapse,
                                                pval.threshold = 0.05)
  
  fgRes = fgRes[ !is.na(match(fgRes$pathway,
                              c( Up$mainPathways, Down$mainPathways))), ] %>%
    arrange(desc(NES))
  fgRes$pathway_id = fgRes$pathway
  fgRes$pathway = stringr::str_replace(fgRes$pathway, "GO_" , "")
  
  fgRes$Enrichment = ifelse(fgRes$NES > 0, "Positive", "Negative")
  filtRes = rbind(head(fgRes, n = 10),
                  tail(fgRes, n = 10 ))
  g = ggplot2::ggplot(filtRes, ggplot2::aes(reorder(pathway, NES), NES)) +
    ggplot2::geom_segment(ggplot2::aes(reorder(pathway, NES),
                                       xend=pathway, y=0, yend=NES)) +
    ggplot2::geom_point( size=5, ggplot2::aes( fill = Enrichment),
                shape=21, stroke=2) +
    ggplot2::scale_fill_manual(values = c("Positive" = "dodgerblue",
                                 "Negative" = "firebrick") ) +
    ggplot2::coord_flip() +
    ggplot2::labs(x="Pathway", y="Normalized Enrichment Score",
         title=sprintf("GSEA - %s",name)) +
    ggplot2::theme_bw()

  output = list("Results" = fgRes,
                "Plot" = g,
                "fgsea_all" = fgRes_all,
                "gene_stats" = data.frame(gene=names(gene_list), stat=as.numeric(gene_list), stringsAsFactors=FALSE),
                "gage_greater" = as.data.frame(gaRes$greater),
                "gage_less" = as.data.frame(gaRes$less))
  return(output)
}
