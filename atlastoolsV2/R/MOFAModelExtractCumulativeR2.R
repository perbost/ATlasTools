#' @title This function extract explained variance of MOFA model.
#' @name MOFAModelExtractCumulativeR2
#' @description This function extract explained variance of the MOFA model 
#' and compute the cumulative variance through factors.
#' @param model MOFA model to extract variance.
#' @export


MOFAModelExtractCumulativeR2 <- function(model){
  
  require(dplyr)
  require(reshape2)
  require(tidyr)
  
  if(length(names(model@cache$variance_explained$r2_per_factor)) == 1){
    r2 <- model@cache$variance_explained$r2_per_factor[[1]]
    r2[r2<0] = 0
    r2_total <- model@cache$variance_explained$r2_total[[1]]
  } else {
    init=TRUE
    for(group in names(model@cache$variance_explained$r2_per_factor)){
      
      dt.temp <- model@cache$variance_explained$r2_per_factor[[group]]
      colnames(dt.temp) <- paste(colnames(dt.temp), group, sep="_")
      
      dt.tot.temp <- model@cache$variance_explained$r2_total[[group]]
      names(dt.tot.temp) <- paste(names(dt.tot.temp), group, sep="_")
      
      if(init){
        r2 <- dt.temp
        r2_total <- dt.tot.temp
        init=FALSE
      } else {
        r2 <- cbind(r2, dt.temp)
        r2_total <- c(r2_total, dt.tot.temp)
      }
    }
    r2[r2<0] = 0
  }
  
  df.m.r2 <- reshape2::melt(lapply(r2_total,
                                   function(x) lapply(x, function(z) z)),
                            varnames=c("view", "group"), 
                            value.name="R2")
  
  colnames(df.m.r2)[(ncol(df.m.r2) - 1):ncol(df.m.r2)] <- c("group",  "view")
  
  df.r2 <- r2 %>%
    as.data.frame() %>%
    mutate(factor = as.factor(1:model@dimensions$K)) %>%
    pivot_longer(cols = -factor, names_to = "view", values_to = "r2") %>%
    group_by(view) %>%
    mutate(cum_r2 = cumsum(r2)) %>%
    ungroup()
  
  df.r2.max <- df.r2 %>% dplyr::group_by(view) %>%
    summarise(max.value = max(cum_r2))
  
  r2.coef <- merge(df.r2.max, df.m.r2, by=c("view"))
  r2.coef$coef = r2.coef$R2 / r2.coef$max.value
  df.r2 <- merge(df.r2, r2.coef, by=c("view"))
  df.r2$cum_r2 <- df.r2$cum_r2 * df.r2$coef
  
  return(df.r2)
}
