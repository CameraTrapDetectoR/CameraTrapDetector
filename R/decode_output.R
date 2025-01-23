#' Extract relevant model output  
#' 
#' @description Converts the output from the neural network into a format that 
#' can be used in reporting results. Helper function for `deploy_model`
#' 
#' @param output this is a subset of the list output from the neural net
#' @param label_encoder passed from deploy model function
#' @param model_version since output >= V3 is different
#' 
#' @returns a dataframe of model output for an image that can be interpreted in R
#' 
#' @import dplyr
#' 
#' @export
decode_output <- function(
    output, # this is the list output from the neural net
    label_encoder,
    model_version
){
  ## process V3 results
  if(stringr::str_sub(model_version, -2, -1) == "v3") {
    # format output as df
    output_df <- as.data.frame(t(round(as.matrix(output[1]), 2)))
    
    # filter down to boxes with predictions
    output_df <- dplyr::filter(output_df, !(dplyr::if_all(c(V5:V68), ~ . == 0)))

    # rename columns
    output_colnames <- c("x_center", "y_center", "box_w", "box_h",
                         label_encoder$label)
    colnames(output_df) <- output_colnames
    
    # remove classes with no predictions
    output_df <- dplyr::select(output_df, c(x_center, y_center, box_w, box_h,
                                            where(~ any(. != 0))))
    
    # normalize bbox coordinates
    output_df <- dplyr::mutate(output_df, dplyr::across(x_center:box_h, ~ ./640))

    # INTERNAL NOTE: review boxes with multiple classifications for out-sample misclassifications
    
    # pivot df to have a column label, confidence score
    output_df <- tryCatch(output_df <- tidyr::pivot_longer(output_df, !(x_center:box_h), 
                                     names_to = "prediction", 
                                     values_to = "confidence_score"),
                          error = function(e) output_df <- data.frame(x_center = 0, y_center = 0,
                                                                      box_w = 0, box_h = 0, 
                                                                      prediction = "Empty",
                                                                      confidence_score = 1))
    pred_df <- dplyr::filter(output_df, confidence_score > 0)
    
    # select classification with highest confidence for each box
    pred_df <- dplyr::group_by(pred_df, x_center, y_center, box_w, box_h)
    pred_df <- dplyr::filter(pred_df, confidence_score == max(confidence_score))
    pred_df <- dplyr::ungroup(pred_df)
    
    # add normalized (xmin,ymin,xmax,ymax) format coordinates
    pred_df <- dplyr::mutate(pred_df, XMin = x_center - (box_w / 2),
                             YMin = y_center - (box_h / 2),
                             XMax = x_center + (box_w / 2),
                             YMax = y_center + (box_h / 2))
    pred_df <- dplyr::mutate(pred_df, dplyr::across(c(XMin:YMin), 
                                                   ~ dplyr::if_else(. < 0, 0, .)))
    pred_df <- dplyr::mutate(pred_df, dplyr::across(c(XMax:YMax), 
                                                    ~ dplyr::if_else(. > 1, 1, .)))
    
    # reorder columns
    pred_df <- dplyr::relocate(pred_df, confidence_score, prediction, .after = last_col())
    
    # apply classwise non-maximum suppression at high threshold
    if(nrow(pred_df) > 1){
      pred_df <- nms(pred_df, threshold = 0.95, classwise = F)
    }
    
    # account for no predictions
    if(nrow(pred_df)==0){
      pred_df <- dplyr::add_row(pred_df, x_center = 0, y_center = 0, box_w = 0, 
                                box_h = 0, XMin = 0, YMin = 0, XMax = 0,
                                YMax = 0, confidence_score = 1,
                                prediction = "Empty")
    }
  }
  
  ## process results V1-V2
  else {
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
    
    # add YOLO format bbox coordinates
    pred_df <- dplyr::mutate(pred_df, x_center = (XMin + XMax)/2,
                             y_center = (YMin + YMax)/2, 
                             box_w = XMax - XMin, box_h = YMax - YMin)
    
    # the predicted y coordinates from the model assume that the y axis 
    # starts in the upper left hand corner of the image, but this is not how
    # plots are made in R, so I need to inverse this value
    pred_df$YMin <- 1-pred_df$YMax
    pred_df$YMax <- 1-pred_df$YMin
    
    # get name of label
    pred_df <- merge(pred_df, label_encoder, by.x="label", by.y="encoder")
    
    # rename prediction
    colnames(pred_df)[colnames(pred_df) == "label.y"] = "prediction"
    
    # filter columns
    pred_df <- pred_df[,c('x_center', 'y_center', 'box_w', 'box_h',
                          'XMin', 'YMin', 'XMax', 'YMax', 'confidence_score', 'prediction')]
    
    # account for no predictions
    if(nrow(pred_df)==0){
      pred_df <- dplyr::add_row(pred_df, x_center = 0, y_center = 0,
                                box_w = 0, box_h = 0, XMin = 0, YMin = 0, 
                                XMax = 0, YMax = 0, confidence_score = 1,
                                prediction = "Empty")
    }
    
  }
  
  return(pred_df)
}