#' Write predictions to image metadata
#' 
#' @description -UNDER DEVELOPMENT- write model outputs to image metadata
#' 
#' @param pred_df results for a given image
#' @param model_version model version so we know which tags to write
#' @param review_threshold review threshold for accepting predictions
#' 
#' @import exiftoolr
#' @import dplyr
#' 
#' @export
write_metadata_tags <- function(pred_df, model_version, review_threshold){
  
  # insert catch for installing exiftool if needed
  tryCatch( { exiftoolr::exif_version() }, error = function(e) {exiftoolr::install_exiftool()})
  
  # define path config file
  config_file = fs::path_package("extdata", "cameratrapdetector_metadata.cfg", package = "CameraTrapDetectoR")
  
  # use model version to define tags
  if(model_version == "species_v1"){
    pred_class <- " -XMP-ctd:CTD_SpV1_PredictedClass="
    pred_count <- " -XMP-ctd:CTD_SpV1_PredictedCount="
    pred_conf <- " -XMP-ctd:CTD_SpV1_ConfidenceScore="
    review_stat <- " -XMP-ctd:CTD_SpV1_ReviewStatus="
  }
  if(model_version == "species_v2"){
    pred_class <- " -XMP-ctd:CTD_SpV2_PredictedClass="
    pred_count <- " -XMP-ctd:CTD_SpV2_PredictedCount="
    pred_conf <- " -XMP-ctd:CTD_SpV2_ConfidenceScore="
    review_stat <- " -XMP-ctd:CTD_SpV2_ReviewStatus="
  }
  if(model_version == "family_v1"){
    pred_class <- " -XMP-ctd:CTD_FamV1_PredictedClass="
    pred_count <- " -XMP-ctd:CTD_FamV1_PredictedCount="
    pred_conf <- " -XMP-ctd:CTD_FamV1_ConfidenceScore="
    review_stat <- " -XMP-ctd:CTD_FamV1_ReviewStatus="
  }
  if(model_version == "family_v2"){
    pred_class <- " -XMP-ctd:CTD_FamV2_PredictedClass="
    pred_count <- " -XMP-ctd:CTD_FamV2_PredictedCount="
    pred_conf <- " -XMP-ctd:CTD_FamV2_ConfidenceScore="
    review_stat <- " -XMP-ctd:CTD_FamV2_ReviewStatus="
  }
  if(model_version == "general_v1"){
    pred_class <- " -XMP-ctd:CTD_GenV1_PredictedClass="
    pred_count <- " -XMP-ctd:CTD_GenV1_PredictedCount="
    pred_conf <- " -XMP-ctd:CTD_GenV1_ConfidenceScore="
    review_stat <- " -XMP-ctd:CTD_GenV1_ReviewStatus="
  }
  if(model_version == "general_v2"){
    pred_class <- " -XMP-ctd:CTD_GenV2_PredictedClass="
    pred_count <- " -XMP-ctd:CTD_GenV2_PredictedCount="
    pred_conf <- " -XMP-ctd:CTD_GenV2_ConfidenceScore="
    review_stat <- " -XMP-ctd:CTD_GenV2_ReviewStatus="
  }
  if(model_version == "pig_only_v1"){
    pred_class <- " -XMP-ctd:CTD_PigV1_PredictedClass="
    pred_count <- " -XMP-ctd:CTD_PigV1_PredictedCount="
    pred_conf <- " -XMP-ctd:CTD_PigV1_ConfidenceScore="
    review_stat <- " -XMP-ctd:CTD_PigV1_ReviewStatus="
  }
  if(model_version == "pig_only_v2"){
    pred_class <- " -XMP-ctd:CTD_PigV2_PredictedClass="
    pred_count <- " -XMP-ctd:CTD_PigV2_PredictedCount="
    pred_conf <- " -XMP-ctd:CTD_PigV2_ConfidenceScore="
    review_stat <- " -XMP-ctd:CTD_PigV2_ReviewStatus="
  }
  
  # use write_output function to generate counts
  preds <- write_output(pred_df) 
  
  # extract highest conf predictions with class count from pred_df
  preds <- dplyr::group_by(preds, filename)
  filtr_preds <- dplyr::filter(preds, confidence_score == max(confidence_score))
  
  # set review status
  filtr_preds <- dplyr::mutate(filtr_preds, 
                               review_stat = dplyr::if_else(confidence_score > review_threshold, 
                                                            "Prediction Accepted", "Prediction Under Review"))
  
  # create metadata args
  
  ## single image
  if(length(filtr_preds==1)){
    exifcall <- c(paste0(pred_class, filtr_preds$prediction),
                  paste0(pred_count, as.character(filtr_preds$count)),
                  paste0(pred_conf, as.character(round(filtr_preds$confidence_score, 3))),
                  paste0(review_stat, filtr_preds$review_stat), " -q", " -overwrite_original")
    
    # write metadata
    exiftoolr::exif_call(args = exifcall, config_file = config_file, path = filtr_preds$filename)
  }
  
  ## Add option to batch write tags to multiple files
  # if(length(filtr_preds > 1)){
  #   # loop through filtr_preds rows to create list of args 
  #   
  #   # write metadata tags
  # }
  # 
  

}
