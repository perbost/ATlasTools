#' @title Extract R2 explained variance from MOFA model.
#' @name MOFAModelExtractR2
#' @description We extract R2 explained variance from all MOFA models with 
#' different number of factor. Then, we plot additional explained variance by 
#' factor.
#' @return result.additional (data frame) additional explained variance by 
#' factor.
#' @export


MOFAModelExtractR2 <- function(path.model){
  
  require(dplyr)
  
  result = data.frame()
  
  directory.list <- list.dirs(path=path.model,
                              full.names=TRUE, 
                              recursive=FALSE)
  
  for(dir in directory.list){
    
    if(file.exists(file.path(dir, "model.hdf5"))){
      
      model <- MOFA2::load_model(file.path(dir, "model.hdf5"))
      
      r2 <- model@cache$variance_explained$r2_per_factor[[1]]
      r2[r2<0]=0
      r2_total <- model@cache$variance_explained$r2_total[[1]]
      
      r2_m_df <- reshape2::melt(lapply(r2_total,
                                       function(x) lapply(x, function(z) z)), 
                                varnames = c("view", "group"), 
                                value.name = "R2")
      
      colnames(r2_m_df)[(ncol(r2_m_df) - 1):ncol(r2_m_df)] <- c("group",
                                                                "view")
      r2.dt <- r2 %>% as.data.table %>% 
        .[,factor:=as.factor(1:model@dimensions$K)] %>%
        melt(id.vars=c("factor"), variable.name="view", value.name = "r2") %>%
        .[,cum_r2:=cumsum(r2), by="view"]
      
      r2.dt.max <- r2.dt %>% dplyr::group_by(view) %>% 
        summarise(max.value = max(cum_r2))
      
      r2.dt.max$nb_factor <- max(as.numeric(as.character(r2.dt$factor)))
      
      result = rbind(result,r2.dt.max)
    }
  }
  
  MofaModelAnalysis(path.save=path.model, 
                                        plot.VarianceByFactor=T,
                                        file.name="Variance_by_factors.pdf",
                                        result=result)
  
  result2 <- result
  result2$nb_factor <- result2$nb_factor + 1
  result.additional <- merge(result, 
                             result2,by=c("view", "nb_factor"), 
                             all.x=T)
  result.additional[is.na(result.additional$max.value.y),]$max.value.y <- 0
  result.additional$additional <-
    result.additional$max.value.x - result.additional$max.value.y
  
  return(result.additional)
}
