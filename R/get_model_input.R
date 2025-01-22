#' get_model_input
#' 
#' @description load image and format to pass to model
#'
#' @param filename full path to image filename
#' @param model_version model version; input differs for V3
#'
#' @return list of two tensors image and target
#' @export
#'
#' @examples
get_model_input <- function(filename, model_version) {
  
  # load image file
  img <- magick::image_read(filename)
  
  # resize image
  if(stringr::str_sub(model_version, -2, -1) == "v3") {
    img <- magick::image_scale(img, paste0(640, 'x', 640, '!'))
  } else {
    img <- magick::image_scale(img, paste0(408, 'x', 307, '!'))
  }
  
  # convert to tensor
  img_tensor <- tryCatch(img_tensor <- torchvision::transform_to_tensor(img), 
                         error = function(e) 
                        data.frame(x_center=NA, y_center=NA, box_w=NA, box_h=NA,
                                   XMin = NA, YMin = NA, XMax=NA, YMax=NA,
                                   confidence_score = NA, prediction = 'image_error', 
                                   number_predictions = 0, filename = filename))
  
  if(is.data.frame(img_tensor)){
    # format error as dataframe compatible with model outputs
    # nn_input <- data.frame(XMin = NA, YMin = NA, XMax=NA, YMax=NA,
    #                        confidence_score = NA, prediction = 'image_error', 
    #                        number_predictions = 0, filename = filename)
    nn_input <- img_tensor
  } 
  if(stringr::str_ends(model_version, pattern = "v3") & !is.data.frame(img_tensor)){
    
    nn_input <- torch::torch_unsqueeze(img_tensor, 1)
  } else {
    # create a dummy target - Faster R-CNN only
    target <- torch::torch_rand(3, 307, 408)
    
    # combine tensor and target into the object that will get passed to network 
    nn_input <- list(img_tensor, target)
    
  }
  
  return(nn_input)
}
