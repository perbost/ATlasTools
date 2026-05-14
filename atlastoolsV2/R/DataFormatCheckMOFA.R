#' @title Data format check for MOFA algorithm.
#' @name DataFormatCheckMOFA
#' @description We test data have good format to be used in MOFA
#' algorithm. 
#' The following tests are performed: 
#' 1. we test that the datarame has the following 5 columns: “sample”, 
#' “feature”, “view”, “value”, “group”.
#' 2. we test that all "feature name + view name" are unique.
#' 3. We test that the values across views are centered reduce 
#' (with a epsilon of 1e-4).
#' 4. we test that the feature name across a view are not duplicated.
#' 5. We test that they have at least 2 values per views. 
#' @param data (data frame) data to check
#' @param epsilon (float) by default: 1e-4, is the epsilon.
#' @return (bool) True if all tests passed.
#' @export


DataFormatCheckMOFA <- function(data,  epsilon=1e-4){

  # Function to check overlapping feature names
  .FeatureNameOverlappingCheck <- function(views) {
    for (i in 1:(length(views) - 1)) {
      for (j in (i + 1):length(views)) {
        if (any(views[[i]]$feature %in% views[[j]]$feature)) {
          return(TRUE)
        }
      }
    }
    return(FALSE)
  }

  # TEST 1:#
  # Check for data are right shape and columns are rights names:
  right_columns_name <- "“sample”, “feature”, “view”, “value”, “group”"
  test_that("TEST 1: columns name of data are right", {
    expect_equal(ncol(data), 5)
    expect_equal(paste(dQuote(colnames(data)), collapse = ", "),
                 right_columns_name)
  })

  # TEST 2:#
  # For MOFA, the names sample + feature must be unique.
  # Check for sample feature couple in long format df are unique:
  sample_feature <- paste(data$sample,
                          data$feature,
                          sep="__")
  test_that("TEST 2: sample names (sample + feature + view) are unique", {
    expect_equal(length(unique(sample_feature)), nrow(data))
  })

  # TEST 3:#
  # Check for values across views are centered reduce:
  # Grouping the data by view and calculating means and standard deviations
  df_means_sd <- data %>%
    group_by(view) %>%
    summarise(mean_value = mean(value, na.rm = TRUE), sd_value = sd(value,
                                                                    na.rm = 
                                                                      TRUE))

  test_that("TEST 3: Values are centered reduce and scaled per view", {
    expect_true(all(df_means_sd$mean_value < epsilon & 
                      df_means_sd$mean_value > -epsilon))
    expect_true(all(df_means_sd$sd_value < 1 + epsilon |
                      df_means_sd$sd_value > 1 - epsilon))
    
    # Print a warning for values that do not meet the condition
    if (!all(df_means_sd$sd_value < 1 + epsilon | 
             df_means_sd$sd_value > 1 - epsilon)) {
      non_reduced_view <-
        df_means_sd$view[!(df_means_sd$sd_value < 1 + epsilon |
                             df_means_sd$sd_value > 1 - epsilon)]
      warning(paste("Warning: the view values which are not reduced are:",
                    paste(non_reduced_view, collapse = ", ")))
    }
    if (!all(df_means_sd$mean_value < epsilon & 
             df_means_sd$mean_value > -epsilon)) {
      non_centered_view <- 
        df_means_sd$view[!(df_means_sd$mean_value < epsilon & 
                             df_means_sd$mean_value > -epsilon)]
      warning(paste("Warning: the view values which are not centered are:",
                    paste(non_centered_view, collapse = ", ")))
    }
  })

  # TEST 4:#
  # feature names are not duplicated across different views:
  # Split data frame based on view:
  views <- split(data, data$view)
  is_overlap <- .FeatureNameOverlappingCheck(views)
  test_that("TEST 4: feature names are not duplicated across different views", {
    expect_false(is_overlap)
  })

  # TEST 5:#
  # check for number of feature per view are at least 2:
  test_that("TEST 5: At least 2 unique feature per view", {
    for(v in unique(data$view)) {
      subset_df <- data %>% filter(view == v)
      num_unique <- length(unique(subset_df$feature))
      test_result <- num_unique >= 2

      expect_true(test_result,
                  info = paste("The number of unique features for view", v,
                               "is not equal or superior to 2"))
    }
  })
}
