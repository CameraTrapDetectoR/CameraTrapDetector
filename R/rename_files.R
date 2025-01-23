#' Rename Files
#' 
#' @description rename files in an image directory
#' 
#' @details 
#' Cameras often default to identical naming conventions, which can create issues
#' during image processing if multiple different image files in different folders
#' have the same file name. This function renames all the images in a folder
#' by appending user-provided category ids to each file name. This function does 
#' NOT search recursively through sub-folders of a directory and must be deployed 
#' on each folder separately, although iteration could easily automate this process.
#' 
#' @param img_dir character string; full path to image folder
#' @param project_id character string; name of project or survey
#' @param site_id character string; name of site
#' @param check_date character string; date images were collected
#' @param camera_id character string; name of camera
#' 
#' @import dplyr
#' @import stringr
#' 
#' @export
rename_files <- function(img_dir, 
                         project_id="", 
                         site_id="", 
                         check_date="", 
                         camera_id="") {
  
  # get list of all jpg filepaths in img_dir
  df <- data.frame(filepath_old = list.files(img_dir, pattern = ".jpg", ignore.case = T,
                                             full.names = T, recursive = F)) 
  
  # extract old filenames
  df <- dplyr::mutate(df, image_id_old = stringr::str_split_i(filepath_old, "/", -1))
  
  # make new filenames
  df <- dplyr::mutate(df, image_id_new = paste(project_id, site_id, camera_id, image_id_old, sep="_"))
  df <- dplyr::mutate(df, filepath_new = fs::path(img_dir, image_id_new))
  
  # print starting message
  cat(paste0("Renaming all images in ", img_dir, 
               ".\n  If your image directory is large, this may take a little time.\n"))
  
  
  # initiate progress bar to track renaming
  # pb <- txtProgressBar(min = 0, max = nrow(df))
  
  # rename files
  # for(i in 1:nrow(df)){
  #   setTxtProgressBar(pb, i)
  #   file.rename(df$filepath_old[i], df$filepath_new[i])
  # }
  fs::move(df$filepath_old, df$filepath_new)
  
  # close progress bar
  # close(pb)
  
  # print message
  cat("All done!\n")

} #END

