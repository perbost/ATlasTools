#' @title Plot and save Jaccard analysis to compare models
#' @name SOMJaccardAnalysis
#' @description This function generate different plot and data frame to compare
#' models based on Jaccard scores analysis.
#' @param path.result path to save jaccard index data frame.
#' @param jaccard.index.total (csv) data frame with different jaccard indexes.
#' @param list.nb.clusters (list) list of cluster number to make analysis
#' @export


SOMJaccardAnalysis <- function(path.result, jaccard.index.total,
                               list.nb.clusters){

  list.models <- c(model.name)
  
  path.model.jaccard.index.total <- file.path(path.result,
                                              "df_jaccard_index_total.csv")
  
  if(!file.exists(path.model.jaccard.index.total)){
    
    df.jaccard.index.total <- data.frame()
    
    for(model in list.models){
      for(nb.factor in list.factor){
        
        model.name.factor <- file.path(model, sprintf("Multi_%03d", nb.factor))
        
        for(nb.cluster in list.nb.clusters){
          
          path.result <- file.path(path.maps,
                                   "2_Consensus_analysis",
                                   model.name.factor,
                                   sprintf("Cluster_%02d", nb.cluster))
          
          jaccard_index <- readr::read_csv(file.path(path.result,
                                                     sprintf("df_jaccard_index_%02d.csv",
                                                             nb.SOM.maps)))
          
          df.jaccard.index.total <- rbind(df.jaccard.index.total, jaccard_index)
        }
      }
    }
    readr::write_csv(df.jaccard.index.total, path.model.jaccard.index.total)
  } else{
    df.jaccard.index.total <- readr::read_csv(path.model.jaccard.index.total)
  }
  
  jaccard.random <- readr::read_csv("data/jaccard_random.csv")
  
  jaccard.index.total <- merge(jaccard.index.total,
                               jaccard.random,
                               by=c("nb_cluster"))
  
  jaccard.index.total$Exclusivity <- 
    (1 -jaccard.index.total$specificity) / (1 -jaccard.index.total$random_index)
  
  jaccard.index.total$Congruence <- 
    jaccard.index.total$sensibility / jaccard.index.total$random_index
  
  jaccard.index.total$stability <- 
    jaccard.index.total$Exclusivity * jaccard.index.total$Congruence

  JaccardScoresPlot(path.result, 
                                        jaccard.index.total, 
                                        list.nb.clusters,
                                        score_type="indexes", 
                                        plot_indexes=T)

  JaccardScoresPlot(path.result, 
                                        jaccard.index.total, 
                                        list.nb.clusters, 
                                        score_type="Exclusivity")

  JaccardScoresPlot(path.result, 
                                        jaccard.index.total, 
                                        list.nb.clusters, 
                                        score_type="Congruence")
  
  JaccardScoresPlot(path.result,
                                        jaccard.index.total, 
                                        list.nb.clusters,
                                        score_type="Stability")
  
  if(ValidatePath(file.path(path.result, 
                            "df_jaccard_index_total_with_random.csv"))){
    readr::write_csv2(jaccard.index.total, 
                      file.path(path.result, 
                                "df_jaccard_index_total_with_random.csv"))
  }
}
