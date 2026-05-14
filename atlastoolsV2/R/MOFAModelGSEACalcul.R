#' @title Function title to complete
#' @name MOFAModelGSEACalcul
#' @description Function description to complete
#' @param gene_list
#' @param myGO
#' @param pval
#' @param name
#' @export
#' @return output


MOFAModelGSEACalcul <- function(gene_list, myGO, pval, name=NULL){
  
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
  
  fgRes <- fgsea::fgsea(pathways = myGO, 
                        stats = gene_list,
                        minSize=15,
                        maxSize=600,
                        nperm=10000) %>% 
    as.data.frame() %>% 
    dplyr::filter(padj < !!pval)
  #print(dim(fgRes))
  
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
  
  #print(dim(rbind(ups,downs)))
  keepups = fgRes[fgRes$NES > 0 & !is.na(match(fgRes$pathway, ups$Pathway)), ]
  keepdowns = 
    fgRes[fgRes$NES < 0 & !is.na(match(fgRes$pathway, downs$Pathway)), ]
  
  ### Collapse redundant pathways
  Up = MOFAModelCollapsePathway(keepups,
                                              pathways = myGO, 
                                              stats = gene_list, 
                                              nperm = 500,
                                              pval.threshold = 0.05)
  Down = MOFAModelCollapsePathway(keepdowns,
                                                myGO, 
                                                gene_list, 
                                                nperm = 500,
                                                pval.threshold = 0.05) 
  
  fgRes = fgRes[ !is.na(match(fgRes$pathway, 
                              c( Up$mainPathways, Down$mainPathways))), ] %>% 
    arrange(desc(NES))
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
  
  output = list("Results" = fgRes, "Plot" = g)
  return(output)
}
