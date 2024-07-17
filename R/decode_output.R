#' Extract relevant model output  
#' 
#' @description Converts the output from the neural network into a format that 
#' can be used in reporting results. Helper function for `deploy_model`
#' 
#' @param output this is a subset of the list output from the neural net
#' @param label_encoder passed from deploy model function
#' 
#' @returns a dataframe of model output for an image that can be interpreted in R
#' 
#' @import dplyr
#' 
#' @export
decode_output <- function(
    output, # this is the list output from the neural net
    label_encoder
){
  # subset the output for the part we want
  preds <- output[[2]][[1]]
  boxes <- as.matrix(preds$boxes)
  img_labels <- as.matrix(preds$labels)
  scores <- as.matrix(preds$scores)
  
  pred_df <- data.frame('boxes' = boxes,
                        'confidence_score' = scores,
                        'label' = img_labels)
  
  # assign column names
  colnames(pred_df)[1:4] <- c('XMin', 'YMin', 'XMax', 'YMax')
  
  # normalize bboxes - will need to change this if image size changes in model training!
  pred_df <- dplyr::mutate(pred_df, dplyr::across(c(XMin, XMax), ~ ./408))
  pred_df <- dplyr::mutate(pred_df, dplyr::across(c(YMin, YMax), ~ ./307))
  
  # check to ensure YMax and YMin are returned as expected - if not then reorder columns
  if(all((pred_df$YMax - pred_df$YMin)<0)){
    # rename switching ymax and ymin
    colnames(pred_df)[1:4] <- c('XMin', 'YMax', 'XMax', 'YMin')
    # reorder columns
    pred_df <- pred_df[,c('XMin', 'YMin', 'XMax', 'YMax', 'confidence_score', 'label')]
  }
  
  # the predicted y coordinates from the model assume that the y axis 
  # starts in the upper left hand corner of the image, but this is not how
  # plots are made in R, so I need to inverse this value
  pred_df$YMin <- 1-pred_df$YMin
  pred_df$YMax <- 1-pred_df$YMax
  
  # get name of label
  pred_df <- merge(pred_df, label_encoder, by.x="label", by.y="encoder")
  
  # rename prediction
  colnames(pred_df)[colnames(pred_df) == "label.y"] = "prediction"
  
  # filter columns
  pred_df <- pred_df[,c('XMin', 'YMin', 'XMax', 'YMax', 'confidence_score', 'prediction')]
  
  # account for no predictions
  if(nrow(pred_df)==0){
    pred_df <- dplyr::add_row(pred_df,
                              XMin = 0, YMin = 0, 
                              XMax = 1, YMax = 1, confidence_score = 1,
                              prediction = "Empty")
  }

  return(pred_df)
}