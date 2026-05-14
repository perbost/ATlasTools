#' @title Function title to complete
#' @name MOFAModelCollapsePathway
#' @description Function description to complete
#' @param fgseaRes
#' @param pathways
#' @param stats
#' @param pval.threshold
#' @param nperm description
#' @param gseaParam description
#' @export
#' @return list



MOFAModelCollapsePathway <- function(fgseaRes, pathways, stats, 
                                     pval.threshold=0.05,
                                     nperm=10/pval.threshold, gseaParam=1){
    
  universe <- names(stats)
  pathways <- pathways[fgseaRes$pathway]
  pathways <- lapply(pathways, intersect, universe)
  parentPathways <- setNames(rep(NA, length(pathways)), names(pathways))
  for (i in seq_along(pathways)) {
    p <- names(pathways)[i]
    if (!is.na(parentPathways[p])) {
      next
    }
    pathwaysToCheck <- setdiff(names(which(is.na(parentPathways))), 
                               p)
    pathwaysUp <- fgseaRes[fgseaRes$pathway %in% pathwaysToCheck & 
                             fgseaRes$ES >= 0, "pathway"]
    pathwaysDown <- fgseaRes[fgseaRes$pathway %in% pathwaysToCheck & 
                               fgseaRes$ES < 0, "pathway"]
    if (length(pathwaysToCheck) == 0) {
      break
    }
    minPval <- setNames(rep(1, length(pathwaysToCheck)), 
                        pathwaysToCheck)
    u1 <- setdiff(universe, pathways[[p]])
    fgseaResUp1 <- fgsea::fgseaSimple(pathways=pathways[pathwaysUp], 
                                      stats = stats[u1],
                                      nperm = nperm,
                                      maxSize = length(u1) - 1, 
                                      nproc = 1, 
                                      gseaParam = gseaParam,
                                      scoreType = "pos")
    fgseaResDown1 <- fgsea::fgseaSimple(pathways = pathways[pathwaysDown], 
                                        stats = stats[u1],
                                        nperm = nperm, 
                                        maxSize = length(u1) - 1, 
                                        nproc = 1, 
                                        gseaParam = gseaParam, 
                                        scoreType = "neg")
    
    fgseaRes1 <- data.table::rbindlist(list(fgseaResUp1, fgseaResDown1), 
                           use.names = TRUE)
    minPval[fgseaRes1$pathway] <- pmin(minPval[fgseaRes1$pathway], 
                                       fgseaRes1$pval)
    u2 <- pathways[[p]]
    fgseaResUp2 <- fgsea::fgseaSimple(pathways = pathways[pathwaysUp], 
                                      stats = stats[u2], 
                                      nperm = nperm, 
                                      maxSize = length(u2) - 1,
                                      nproc = 1, 
                                      gseaParam = gseaParam,
                                      scoreType = "pos")
    fgseaResDown2 <- fgsea::fgseaSimple(pathways = pathways[pathwaysDown], 
                                        stats = stats[u2], 
                                        nperm = nperm,
                                        maxSize = length(u2) - 1, 
                                        nproc = 1,
                                        gseaParam = gseaParam, 
                                        scoreType = "neg")
    
    fgseaRes2 <- data.table::rbindlist(list(fgseaResUp2, fgseaResDown2), 
                           use.names = TRUE)
    
    minPval[fgseaRes2$pathway] <- pmin(minPval[fgseaRes2$pathway], 
                                       fgseaRes2$pval)
    
    parentPathways[names(which(minPval > pval.threshold))] <- p
  }
  return(list(mainPathways = names(which(is.na(parentPathways))), 
              parentPathways = parentPathways))
}