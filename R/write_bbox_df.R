#' separate function to write bboxes to df
#' 
#' @description helper function for `deploy_model` to create bbox df for saving in checkpoint, csv
#' 
#' @param predictions_list list of current image predictions
#' @param bboxes existing predicted boxes df
#'
#' @import dplyr
#'
#' @export
write_bbox_df <- function(predictions_list, bboxes) {
  
  # convert list into dataframe
  bbox_df <- do.call(dplyr::bind_rows, predictions_list)
  
  # round bbox coordinates to the third decimal place
  bbox_df <- dplyr::mutate(bbox_df, dplyr::across(x_center:YMax, ~round(., 3)))
  
  # combine new bboxes with any existing results
  bbox_df <- unique(rbind(bbox_df, bboxes))
  
  #return bbox df
  return(bbox_df)
  
}#END
