#' @title Project Clinical data passed in argument in SOM map.
#' @name ProjectionSOMData
#' @description This function project clinical data (as data frame) in SOM map
#' loaded.
#' @param projected data frame of the data to project on the map.
#' @param map current SOM map to use.
#' @param path.directory path to save figure projection.
#' @param dt.Atlas.clinical clinical data as data frame.
#' @param list.clinical.projection (list) list of clinical data to project on
#' the map.
#' @param groups data frame with cluster value of each nodes of the SOM map.
#' @param nb.samples (int) number of samples.
#' @export


ProjectionSOMData <- function(projected, map, path.directory, dt.Atlas.clinical,
                              list.clinical.projection, groups, nb.samples){
  
  path.projection.clinical <- file.path(path.directory, "Clinical_Projection")
  if(!dir.exists(path.projection.clinical)) dir.create(path.projection.clinical)
  
  path.fisher.table <- file.path(path.projection.clinical, "fisher_test.csv")
  
  if(dir.exists(path.fisher.table)){
    result.fisher <- read.csv(path.fisher.table)
  } else result.fisher <- data.frame()
  
  for(clinical.variable in list.clinical.projection){
    
    projected$clinical.variable <- projected[, clinical.variable]
    color.projected.clinical.variable <- paste0("color.", clinical.variable)
    colors.bg <- projected[color.projected.clinical.variable]
    
    if(nrow(projected[!is.na(projected$clinical.variable) & 
                      projected$clinical.variable==-1,])>0){
      projected[!is.na(projected$clinical.variable) &
                  projected$clinical.variable==-1,]$clinvar <- NA
    }
    
    fisher.table <- stats::ftable(projected$cluster,
                                  projected$clinical.variable)
    fisher.pv <- stats::fisher.test(fisher.table, 
                                    simulate.p.value=TRUE)$p.value
    result.fisher <- 
      rbind(result.fisher,
            data.frame(clinical.variable=clinical.variable,
                       fisher.test.pv=fisher.pv))
    
    plot.main <- sprintf("%s projection (Fisher p.value: %.3f)", 
                         clinical.variable, 
                         fisher.pv)
    
    legend.fill <- c("gray", "blue","red")
    legend.txt <- c(as.expression(bquote(bold("NA"))),
                           as.expression(bquote(bold("F"))),
                           as.expression(bquote(bold("M"))),
                           as.expression(bquote(bold("M"))))
    
    SomMapCluster(map, 
                                      path.projection.clinical,
                                      groups=groups,
                                      type="property",
                                      property=groups,
                                      title= sprintf("%s_projection",
                                                     clinical.variable), 
                                      main=plot.main,
                                      dt.projected=projected,
                                      add.samples=T,
                                      plot.pie=T,
                                      color.clinical.variable=colors.bg,
                                      fisher.table=fisher.table,
                                      legend=T,
                                      legend.txt=legend.txt,
                                      legend.fill=legend.fill,
                                      nb.samples=nb.samples)
  }
  if(ValidatePath(path.fisher.table)){
    readr::write_csv(result.fisher, path.fisher.table)
  }
}
