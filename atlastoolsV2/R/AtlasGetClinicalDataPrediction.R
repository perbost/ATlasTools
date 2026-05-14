#' @title Load or compute target clinical data prediction probabilities.
#' @name AtlasGetClinicalDataPrediction
#' @description Function to check if clinical data are already compute and
#' saved. If not, compute it and add result to existing data frame result, if
#' exist or create it. If yes, load and return results.
#' @param file.path (path) path to load and/or save data frame clinical data
#' prediction probabilities result.
#' @param df.codes (data frame) factor vector for each node of the map.
#' @param list.name.clinical.data (list) string list of target clinical data.
#' @param df.clinical.data (data frame) if exist, data frame with clinical 
#' data probabilities prediction already computed.
#' @return data frame with clinical prediction probabilities
#' @export


AtlasGetClinicalDataPrediction <- function(file.path, df.codes=NULL,
                                           list.name.clinical.data=NULL,
                                           df.clinical.data=NULL){
  
  .computeAndAddClinicalDataPrediction <- function(df, names, df.clinical.data,
                                                   df.codes){
    
    for(str in names){
      # compute vector with current clinical data probability
      result <- AtlasComputeClinicalDataPrediction(df.clinical.data,
                                                   df.codes,
                                                   str)
      # Add the new column
      df[[str]] <- result
    }
    return(df)
  }
  
  # Check if the CSV file exists with the clinical data probabilities
  if(file.exists(file.path)){
    # Load the existing CSV file
    df <- readr::read_csv(file.path)
    
    # Check if clinical data name exist as column in the df
    if(!all(list.name.clinical.data %in% colnames(df))) {
      
      list.missing.name.clinical.data <- setdiff(list.name.clinical.data,
                                                 colnames(df))
      
      df <-
        .computeAndAddClinicalDataPrediction(df,
                                             list.missing.name.clinical.data,
                                             df.clinical.data,
                                             df.codes)
      
      # Save the modified data frame back to the CSV
      write.csv(df, file.path, row.names = FALSE)
      elements <- unlist(list.missing.name.clinical.data)
      str.element <- paste(elements, collapse = ", ")
      message("Modification csv file containing prediction probabilities for
              clinical data: Column(s) ", str.element, " added and file saved.")
    }else {
      elements <- unlist(list.name.clinical.data)
      str.element <- paste(elements, collapse = ", ")
      message("CSV file containing prediction probabilities for
              clinical data: Column(s) ", str.element,
              " already exists. Nothing changes.")
      
    }
    return(df)
    
  }else{
    # Create a new dataframe with the code column and the new column(s)
    df <- data.frame(code=rownames(df.codes))
    df <- .computeAndAddClinicalDataPrediction(df,
                                               list.name.clinical.data,
                                               df.clinical.data,
                                               df.codes)
    
    # Save the dataframe as a new CSV file
    write_csv(df, file.path)
    elements <- unlist(list.name.clinical.data)
    str.element <- paste(elements, collapse = ", ")
    message("Creation CSV file containing prediction probabilities for
            clinical data: Column(s) ", str.element)
    return(df)
  }
}