#' Box IoU
#' 
#' @description Calculates the intersection over union (IoU) two bounding boxes. 
#' Run when the user specifies an overlap correction; helper function for `find_unique_sets`
#' 
#' @param a Bounding box that will be compared with b
#' @param b Second bounding box to compare with a
#' 
#' @returns overlap area of `a` and `b`
#'
#' @export
#' 

box_iou <- function(a,b){
  
  # calculate intersection area
  dx = min(a$XMax, b$XMax) - max(a$XMin, b$XMin)
  dy = min(a$YMax, b$YMax) - max(a$YMin, b$YMin)
  box_intersect <- dx*dy
  
  # calculate box union
  area.a <- (a$XMax - a$XMin) * (a$YMax - a$YMin)
  area.b <- (b$XMax - b$XMin) * (b$YMax - b$YMin)
  box_union <- area.a + area.b - box_intersect
  
  # include small delta in union to avoid dividing by 0
  box_iou <- box_intersect / (box_union + 0.00001)
  
  return(box_iou)
}
