#' @title Copy n first best map for current model based on concordance to 
#' consensus score
#' @name SOMSelectAndCopyBestMaps
#' @description We order all best map based on concordance score and we copy
#' the "n.first.map" in target folder.
#' In this function we also save a data frame with best mode consensus.
#' If we use quantization error, we sort best map first by 'over_cluster' value,
#' then by concordance to consensus score and then by quantization error.
#' In this case (use.quantization.error=T) we use precision digit to sort value.
#' @param model.consensus consensus clustering for current model
#' @param path.result path to the result folder
#' @param path.maps the path to maps
#' @param path.best.maps path to the folder with best map saved
#' @param n.first.map (int) the n first map to save in the target result folder
#' @param use.quantization.error (bool) if TRUE, we take into count the 
#' quantization error of the map. Quantization error measures how well the SOM 
#' represents the input data.
#' @param concordance.precision is the precision to sort best map for 
#' concordance score, i.e. we round all concordance values with this precision 
#' (To round a value to n digits after the decimal point, by default is 2) and
#' for all same score we sort regarding the quantization error.
#' @param file.name (str) name of the data frame to save with fit scores.
#' @param clustering.method (str) clustering method used.
#' @export


SOMSelectAndCopyBestMaps <- function(model.consensus, path.result,
                                     path.maps, path.best.maps, n.first.map=10,
                                     use.quantization.error=T, 
                                     concordance.precision=2,
                                     file.name="best_model_consensus.csv",
                                     clustering.method=
                                       "hierarchical_clustering"){

  if(use.quantization.error){
    best.model.consensus <- 
      model.consensus[order(model.consensus$over.cluster,
                            -round(model.consensus$concordance_to_consensus, 
                                   digits=concordance.precision),
                            model.consensus$errors.quantization),]
    
  }else{
    best.model.consensus <- 
      model.consensus[order(model.consensus$over.cluster,
                            -model.consensus$concordance_to_consensus),]
  }
  
  if(ValidatePath(file.path(path.result, file.name))){
    readr::write_csv(best.model.consensus, 
                     file.path(path.result, file.name))
  }
  
  for(m in seq(1, n.first.map)){
    map.original <- file.path(path.maps, 
                              "1_Baseline_analysis",
                              paste(best.model.consensus[m,c("model", 
                                                         "nb.clusters",
                                                         "map")], 
                                collapse = "/"))

    path.best.maps.nth <- 
      file.path(path.maps, 
                "4_Best_maps_consensus",
                paste(best.model.consensus[m,c("model", "nb.clusters", "map")], 
                      collapse = "/"))
    
    if(!file.exists(path.best.maps.nth)) {
      # If not, create the directory
      dir.create(path.best.maps.nth)
    }
    
    # Copy all files in the source directory and the clustering method folder:
    directory.content <- list.files(map.original, full.names=TRUE)
    is.file <- file.info(directory.content)$isdir == FALSE
    
    files <- directory.content[is.file]
    files.to.copy <- c(files, file.path(map.original, clustering.method))

    file.copy(from=files.to.copy,
              to=path.best.maps.nth, 
              overwrite=TRUE,
              recursive=TRUE)
  }
}
