#' @title High level function to define and plot immunogram based on clinical
#' data.
#' @name AtlasImmunogram
#' @description Circle.coord explication: c(inner, outer, text): 
#'  -inner: (float) x and y coordinate to start circle banner.
#'  -outer: (float) x and y coordinate to end circle banner 
#'  (difference between inner and outer define the thickness of the banner). 
#'  -text: (float) text coordinate (x, y) around of the banner.
#'  #' Filter selected features by treshold or range: - by threshold: 
#'  choose a value and we keep only features with correlation > threshold. 
#'  - by range: we choose a range limit (int) and we keep only the n most 
#'  correlated features all views combined.
#' @param df.results.correlation (dataframe) data frame with results of all 
#' correlation between features and target clinical data.
#' @param df.clinical.data.prediction.probability (dataframe) data frame with
#' probability prediction of each map nodes regarding the target clinical data.
#' @param clinical.data (str) target clinical data name.
#' @param method (str) correlation method to select features.
#' @param n.feature (int) number of features for each view to plot in the 
#' immunogram.
#' @param return.immunogram.values (bool) if True, return immunogram values and
#' don't generate and save the Immunogram.
#' @param color_df (dataframe) data frame with the list of views and the defined
#' colors associated for each view.
#' @param banner.start (float) position to start the banner.
#' @param banner.thickness (float) thickness of the banner.
#' @param banner.texte (float) position of the text around the banner.
#' @param plot.title (str) title of the figure.
#' @param path.plot.save (str) path to save the figure.
#' @param width (float) width of the figure.
#' @param height (float) height of the figure.
#' @param sample.to.plot (list) list of node names for which to generate the 
#' immunogram. 
#' @param plot.max.min (bool) if we generate figure with max and min of the 
#' datas in the immunogram (default=F).
#' @param centerzero (bool) if axis radarchart are centered in zero (default=F).
#' @param axistype (int) type of axis.
#' @param feature.selection.threshold (float) Threshold to select features. We
#' keep only features with correlation bigger that this threshold. If NULL, we 
#' don't use threshold.
#' @param feature.selection.limit (int) NUmber of feature to keep for the 
#' immunogram. We keep the best features correlated in the defined limit. If
#' NULL, we don't use this limit.
#' @param labels.circle.coord (float) position of the circle on which we will 
#' place the labels  (default=1.2).
#' @param labels.cex (float) numeric character expansion factor; multiplied by
#'  ("cex") yields the final character size. NULL and NA are equivalent to 1.0
#'  (default=0.8).
#' @param add.custom.label (bool) if we add custom labels around the figure
#'  (default=T).
#' @param font (int) font type: 1: normal, 2: bold, 3: italic.
#' @param plot.marge (float) margin size to use in the figure (default=0).
#' @export



AtlasImmunogram <- function(df.results.correlation, 
                            df.clinical.data.prediction.probability,
                            clinical.data, method, n.feature,
                            return.immunogram.values=FALSE,
                            color_df=NULL, banner.start=1.7,
                            banner.thickness=0.16, banner.texte=2.1,
                            plot.title="Untitled",
                            path.plot.save=NULL, 
                            width=7, height=7, sample.to.plot=NULL,
                            plot.max.min=FALSE, centerzero=FALSE, 
                            axistype=1, feature.selection.threshold=NULL, 
                            feature.selection.limit=NULL, 
                            labels.circle.coord=1.2, labels.cex=0.8,
                            add.custom.label=TRUE, font=1, plot.marge=0) {
  
  require(dplyr)
  require(purrr)
  

  .feature_select_by_range <- function(list.df, method, n) {
    # Filter each data frame in the list based on the combined feature range
    combined_df <- dplyr::bind_rows(list.df)
    # Sort and filter to keep top 'n' rows
    top_n_df <- combined_df %>%
      arrange(desc(!!sym(method))) %>%
      slice_head(n = n)
    # Filter each data frame in list
    list.df <- purrr::map(list.df, ~ .x %>% 
                       filter(feature %in% top_n_df$feature))
    return(list.df)
  }
  
  .feature_select_by_threshold <- function(list.df, method, threshold) {
    # Filter each data frame in the list based on the threshold
    list.df <- lapply(list.df, function(df) {
      df %>% filter(!!sym(method) > threshold)
    })
    return(list.df)
  }
  
  .get_sign <- function(df, method) {
    df %>%
      transmute(feature, sign = sign(!!sym(method)))
  }
  
  .get.labels.color <- function(label, selected.features, method){
    
    df.sign <- selected.features %>%
      map_df(~ .get_sign(.x, method),)
    
    # fill in color data frame depending of correlation sign (red for negative
    # correlation, black neither)
    # Create an empty dataframe to store colors
    color.labels <- data.frame(label=label, color=NA)
    
    # Fill in the colors based on the sign value in df
    color.labels$color[df.sign$sign == 1] <- "black"
    color.labels$color[df.sign$sign == -1] <- "red"
    
    return(color.labels)
  }
  
  
  selected.features <- AtlasFeatureSelection(
    df.results.correlation,
    clinical.data,
    method,
    n=n.feature)
  
  if(!is.null(feature.selection.limit)){
    selected.features <- .feature_select_by_range(selected.features,
                                                  method,
                                                  feature.selection.limit)
  }

  if(!is.null(threshold)){
    selected.features <- .feature_select_by_threshold(selected.features, 
                                                      method, 
                                                      threshold)
  }
  
  # Remove empty data frames
  selected.features <- purrr::discard(selected.features, ~ nrow(.x) == 0)
  
  
  # Define the variable ranges: maximum and minimum for the clinical data
  max_min <- 
    AtlasDefineMaxMinClinicalData(selected.features,
                                                clinical.data,
                                        df.clinical.data.prediction.probability,
                                        mapdata)
  
  # define a vector with the view name for each selected features
  # we need that to construct the circle banner around the radar chart
  selected.features.views.vector <-
    unlist(lapply(names(selected.features), function(name){
      rep(name, times = nrow(selected.features[[name]]))}))
  
  # create a dataframe with the correlation sign associated to the view and 
  # features name and define color based on the sign
  labels.color <- .get.labels.color(names(max_min),
                                    selected.features,
                                    method)
  
  # list of unique view and a list with the number of feature for each view
  table.frequence.view <- table(selected.features.views.vector)
  views <- unique(selected.features.views.vector)
  
  features_per_view <- list()
  for(v in views) {
    # Get the value corresponding to the string from the first row of the 
    # dataframe
    value <- as.integer(table.frequence.view[v])
    # Append the value to the list
    features_per_view <- c(features_per_view, value)
  }
  features_per_view <- unlist(features_per_view)

  if(is.null(color_df)){
    color_df <- data.frame(
      view = c("IHC", "IHC_canonic", "RNAseq", "WES", "NGS_WES", "NS_IO360"),
      Color = c("#F60000", "#01468A", "#42B646", "#0198B2", "#925E9D",
                "#DB632D")
    )
  }
  
  if(return.immunogram.values){
    warning("By choosing to return immunogram values, the immunogram has not
    been generated and saved. \n
    To do this, call the 'plot.Immunogram' function from 'AtlasVisualization'
            package. \n
            With this option, the function return a list the elements you need
            to plot immunogram.")
    list.immunogram <- list(max_min = max_min,
                            views = views,
                            color_df = color_df,
                            features_per_view = features_per_view,
                            circle.coord = c(banner.start,
                                             banner.start + banner.thickness,
                                             banner.texte))
    return(list.immunogram)
  }else{

    if(plot.max.min){
      # 3 rows are the minimum to plot a radar chart: The first two columns
      # define the variable range (min and max) and the following columns are 
      # samples to plot
      PlotImmunogram(max_min,
                                         views,
                                         color_df,
                                         features_per_view,
                                         colors=c("#0000FF","#FF0000"),
                                         max.char.label=15,
                                         title = paste(plot.title, "(Max Min)"),
                                         circle.coord=c(banner.start,
                                                        banner.start + 
                                                          banner.thickness,
                                                        banner.texte),
                                         line_style=c(2,2),
                                         fill_color = c(NA,NA),
                                         centerzero=centerzero,
                                         path.plot.save=path.plot.save,
                                         plot.title=plot.title,
                                         width=width,
                                         height=height,
                                         axistype=axistype,
                                         add.custom.label=add.custom.label, 
                                         labels.color=labels.color,
                                        labels.circle.coord=labels.circle.coord, 
                                         labels.cex=labels.cex,
                                         font=font,
                                        plot.marge=plot.marge)
    }
    if(!is.null(sample.to.plot)){
      
      samples <- data.frame()
      vname <- sample.to.plot
      
      for(code in vname){
        sample_list = c()
        for(view in names(selected.features)){
          df.view <- selected.features[[view]]
          for(feature in df.view$feature){
            # weighted by sign of correlation score to plot rigth direction
            sample_list = c(sample_list,
                            sign(df.view[df.view$feature == feature, method]) *
                              mapdata[[view]][feature, code])
          }
        }
        names(sample_list) <- 1:length(sample_list)
        samples <- rbind(samples, data.frame(t(sample_list)))
      }
      
      colnames(samples) <- colnames(max_min)
      rownames(samples) <- vname
      
      for(sample in sample.to.plot){
        
        data.sample <- samples[sample,]
        df <- rbind(max_min, data.sample)
        plot.title.node = paste0(plot.title, "_node_", sample)
        
        # Create the radar charts
        PlotImmunogram(df,
                                           views,
                                           color_df,
                                           features_per_view,
                                           colors=
                                             c("#0000FF","#FF0000", "#00AFBB"),
                                           max.char.label=15,
                                           title = plot.title.node,
                                           circle.coord=c(banner.start,
                                                          banner.start + 
                                                            banner.thickness,
                                                          banner.texte),
                                           line_style=c(2,2,1),
                                           fill_color = 
                                             c(NA,NA,scales::alpha("#00AFBB",
                                                                   0.75)),
                                           centerzero=centerzero,
                                           path.plot.save=path.plot.save,
                                           plot.title=plot.title.node,
                                           width=width,
                                           height=height,
                                           axistype=axistype,
                                           add.custom.label=add.custom.label, 
                                           labels.color=labels.color,
                                           labels.circle.coord=
                                             labels.circle.coord, 
                                           labels.cex=labels.cex,
                                           font=font,
                                           plot.marge=plot.marge)
      }
    }
  }
}
