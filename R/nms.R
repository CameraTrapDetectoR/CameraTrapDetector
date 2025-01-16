#' Non-maximum suppression
#' 
#' @description apply non-maximum suppression to raw predictions. 
#' Adapted from the torchvision nms function: https://github.com/pytorch/vision/blob/master/torchvision/ops/boxes.py
#' 
#' @param df prediction df from `decode_output` 
#' @param threshold IoU threshold for suppressing overlapping boxes
#' @param classwise boolean. Apply nms across classes (FALSE) or just within class (TRUE)
#' 
#' @return 
#' @export
#'
#' @examples
nms <- function(df, threshold, classwise) {
  
  # perform agnostic NMS
  if(!classwise){
    
    # sort df by confidence score
    df <- dplyr::arrange(df, dplyr::desc(confidence_score))
    
    # get unique combinations of box pairs
    box_pairs <- as.data.frame(t(unique(combn(as.numeric(rownames(df)), 2))))
    box_pairs <- dplyr::mutate(box_pairs, IoU = 0)
    
    # get IoU for each box pair
    for(i in 1:nrow(box_pairs)) {
      box_pairs$IoU[i] <- box_iou(df[box_pairs$V1[i],], df[box_pairs$V2[i],])
    }
    
    # filter out pairs above threshold
    keep_pairs <- dplyr::filter(box_pairs, IoU <= threshold)
    
    # filter to boxes below IoU threshold
    keep_ids <- unique(c(1, keep_pairs$V1, keep_pairs$V2))
    
    # filter df 
    nms_df <- df[keep_ids, ]
  }
  
  # perform NMS within class
  if(classwise){
    df <- dplyr::arrange(df, prediction, dplyr::desc(confidence_score))
  }
  
  return(nms_df)
}