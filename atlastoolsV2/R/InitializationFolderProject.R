#' @title Create project architecture folder for atlastoolsv2 project.
#' @name InitializationFolderProject
#' @description This function create multiple folder to run Atlas projects and
#' save outputs.
#' The following folders are created (if not exist):
#' data, data/external, data/processed, data/raw.
#' models.
#' reports, reports/scripts_outputs, reports/figures, reports/seed,
#' reports/maps, reports/maps/1_Baseline_analysis, 
#' reports/maps/2_Consensus_analysis, reports/maps/3_Best_maps,
#' reports/maps/4_Best_maps_consensus, reports/maps/5_Selected_maps.
#' @export


InitializationFolderProject <- function(){
  
  data_folder <- "data"
  data_external_folder <- "data/external"
  data_processed_folder <- "data/processed"
  data_raw_folder <- "data/raw"
  
  model_folder <- "models"
  
  reports_folder <- "reports"
  reports_figures_folder <- "reports/figures"
  reports_seed_folder <- "reports/seed"
  reports_maps_folder <- "reports/maps"
  reports_maps_1_folder <- "reports/maps/1_Baseline_analysis"
  reports_maps_2_folder <- "reports/maps/2_Consensus_analysis"
  reports_maps_3_folder <- "reports/maps/3_Best_maps"
  reports_maps_4_folder <- "reports/maps/4_Best_maps_consensus"
  reports_maps_5_folder <- "reports/maps/5_Selected_maps"
  
  src_folder <- "src"
  
  folder_to_create_list <- list(data_folder, 
                                data_external_folder, 
                                data_processed_folder,
                                data_raw_folder,
                                model_folder,
                                reports_folder,
                                reports_figures_folder,
                                reports_seed_folder,
                                reports_maps_folder,
                                reports_maps_1_folder,
                                reports_maps_2_folder,
                                reports_maps_3_folder,
                                reports_maps_4_folder,
                                reports_maps_5_folder,
                                src_folder)
  
  for(folder in folder_to_create_list){
    if(!dir.exists(folder)) dir.create(folder, recursive=T)
  }
}
