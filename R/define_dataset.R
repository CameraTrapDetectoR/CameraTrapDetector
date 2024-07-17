#' Define image dataset 
#' 
#' @description Create list of image files from data_dir. Helper function for `deploy_model`
#' 
#' @param data_dir data directory
#' @param recursive boolean
#' @param file_extensions file extensions
#' 
#' @returns list of image paths for reading into model
#' 
#' @export
define_dataset <- function(data_dir,
                    recursive = TRUE,
                    file_extensions = c(".jpg", ".JPG"))
  {
  
  # check that datadir exists
  if(!fs::dir_exists(data_dir)){
    stop(paste0("data_dir: ", data_dir, " does not exist! Please specify
                an path that exists on your computer. "))
  }
  
  # list all files in data_dir
  file_list <- fs::dir_ls(data_dir, recursive = recursive,
                          regexp = paste0(file_extensions, collapse="|"), ignore.case = TRUE)
  
  # remove any image files inside prediction folders
  file_list <- file_list[!stringr::str_detect(file_list, "predictions")]
  
  if(length(file_list) < 1){
    stop(paste0("There are no files in your data_dir with the specified file extensions."))
  }
  
  return(file_list)
  
}