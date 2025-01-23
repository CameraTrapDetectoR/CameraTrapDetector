#' eval_one_image
#' 
#' @description Evaluate a single image in the model. Helper function for `deploy_model`
#'
#' @param input torch tensor. formatted image to pass to the model
#' @param filename string. full path to image file
#' @param label_encoder dict. class label dictionary
#' @param score_threshold numeric. arg passed from `deploy_model` to filter low confidence predictions
#' @param overlap_correction boolean. arg passed from `deploy_model` to apply non-maximum suppression
#' @param overlap_threshold numeric. arg passed from `deploy_model`, IoU threshold for NMS
#' @param location
#' @param possible_labels
#'
#' @return pred_df data frame of formatted predictions for the image
#' @export
#'
#' @examples
eval_one_image <- function(input, filename, label_encoder, score_threshold,
                           overlap_correction, overlap_threshold,
                           location, possible_labels, model, model_version) {
  
  # deploy the model. suppressing warnings here, because it is not important
  defaultW <- getOption("warn")
  output <- suppressMessages({model(input)})
  options(warn = defaultW)
  
  # format output
  pred_df <- decode_output(output, label_encoder, model_version)
  
  # address overlapping detections
  if(overlap_threshold){
    if(nrow(pred_df) > 1){
      pred_df <- nms(pred_df, overlap_threshold, classwise = F)
    }
  }

  
  # evaluate predictions using possible species
  if(is.null(location)==FALSE){
    pred_df<-smart_relabel(pred_df, possible_labels, label_encoder)
    pred_df<-pred_df[pred_df$prediction %in% possible_labels$label,]
  }
  
  # remove predictions below the score threshold
  pred_df <- dplyr::filter(pred_df, confidence_score >= score_threshold)
  
  # when there is no predicted bounding box, create a relevant pred_df
  if(nrow(pred_df) < 1) {
    pred_df <- data.frame(x_center = 0, y_center = 0, box_w = 0, box_h = 0,
                          XMin = 0, YMin = 0, XMax = 0, YMax = 0,
                          confidence_score = 1, prediction = "Empty")
  }
  
  # add full filepath to prediction
  pred_df$filename <- rep(filename, nrow(pred_df))
  
  return(pred_df)
}
