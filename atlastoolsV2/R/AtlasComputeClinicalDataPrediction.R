#' @title Build regression model to predict clinical data outcomes from 
#' node's factors
#' @name AtlasComputeClinicalDataPrediction
#' @description We build logistic or linear regression model depending of the
#' clinical data outcomes nature (binary, categorical or continue).
#' From this trained model, we predict probability of each map's node to predict
#' clinical data.
#' @param df.clinical.data (data frame) clinical data outcomes for each sample.
#' @param factors (data frame) factor vector for each node of the map.
#' @param str.clinical.data (list) string list of the clinical data to compute 
#' prediction probabilities.
#' @return predicted probability data frame
#' @export


AtlasComputeClinicalDataPrediction <- function(df.clinical.data, factors,
                                               str.clinical.data){
  
  require(dplyr)
  
  .is_binary <- function(x) {
    length(unique(x)) == 2
  }
  
  .input_model_format <- function(df.train, df.pred){
    
    # Combine datas
    data <- cbind(df.train, df.pred)
    
    # Convert to a dataframe and name the response variable as "outcome"
    data <- as.data.frame(data)
    colnames(data)[1] <- "outcome"
    return(data)
  }
  
  # retains only those rows where all specified columns have values different
  # from -1.
  # After filtering the rows, selects only the columns listed in
  # 'str.clinical.data'.
  df1 <- df.clinical.data %>%
    filter_at(str.clinical.data, all_vars(. != -1)) %>%
    select(all_of(str.clinical.data))
  
  # select real sample factors data:
  real.sample.code <- factors[rownames(factors) %in%
                                paste0("V", rownames(df1)), ]
  
  # check if the clinical data is a binary outcome (two different issues)
  if (!.is_binary(df1)) {
    message("Clinical data not binary, we use linear regression model to predict
            other node output")
    
    df1 <- (df1 - min(df1)) / (max(df1) - min(df1))
    dataset <- .input_model_format(df1, real.sample.code)
    
    # Fit a linear model
    linear_model <- stats::lm(outcome ~ ., data = dataset)
    
    # Make predictions on new data
    predicted_probabilities <- stats::predict(linear_model,
                                              newdata = as.data.frame(factors),
                                              type = "response")
    } else{
      message("Clinical data are binary, we use logistic regression model to 
      predict other node output")
      
      dataset <- .input_model_format(df1, real.sample.code)
      
      # Build a logistic regression model
      logistic_model <- stats::glm(outcome ~ .,
                                   data = dataset,
                                   family = "quasibinomial")
      
      # Make predictions
      predicted_probabilities <-  stats::predict(logistic_model,
                                                 newdata=as.data.frame(factors),
                                                 type = "response")
  }

  # Return the predicted values as a dataframe
  return(predicted_probabilities)
}
