#' Aggregate counts
#' 
#' @description aggregate counts for detections of the same class in a single image. Helper function for `deploy_model`
#' 
#' @details This function removes bounding box coordinates from the model output and
#' aggregates multiple detections of the same class per image into a classwise prediction count.
#' If multiple animals of the same class are detected, the lowest confidence score will be retained to 
#' reflect a conservative uncertainty estimate of the count.
#' 
#' @import dplyr
#'
#' @param predictions_list list of predictions from model
#' 
#' @returns df with score threshold applied
#' 
#' @export
#'
aggregate_counts <- function(predictions_list){
  
  # convert list into dataframe
  df <- do.call(dplyr::bind_rows, predictions_list)
  
  # get prediction counts for each image, join to df
  pred_counts <- dplyr::count(df, filename)
  df <- dplyr::left_join(df, pred_counts, by = "filename")
  
  # separate images with single predictions and images with multiple predictions
  single_preds <- dplyr::filter(df, n == 1)
  single_preds <- dplyr::rename(single_preds, prediction_count = n)
  multiple_preds <- dplyr::filter(df, n > 1)
  
  # get classwise counts by image
  class_counts <- dplyr::group_by(multiple_preds, filename)
  class_counts <- dplyr::count(class_counts, prediction)
  class_counts <- dplyr::rename(class_counts,
                                prediction_count = n)
  
  # format images with multiple detections
  if(nrow(multiple_preds) > 0) {
    # join class counts
    multiple_preds <- dplyr::left_join(multiple_preds, class_counts,
                                       by = c("filename", "prediction"))
    multiple_preds <- dplyr::select(multiple_preds, !n)
    
    # aggregate class counts
    multiple_preds <- dplyr::group_by(multiple_preds, filename, prediction)
    multiple_preds <- dplyr::filter(multiple_preds, confidence_score == min(confidence_score))
  }
  
  # combine formatted dfs
  df_out <- dplyr::bind_rows(single_preds, multiple_preds)
  
  # format count for empty predictions
  df_out <- dplyr::mutate(df_out, prediction_count = dplyr::if_else(prediction == "Empty", 0, prediction_count))
  
  # reorder columns and drop bounding boxes, sort rows
  df_out <- df_out[,c('filename', 'prediction', 'confidence_score', 'prediction_count')]
  df_out <- dplyr::arrange(df_out, filename)
  
  return(df_out)
}
