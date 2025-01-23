#' Organize image files into new directory structure sortd by class
#' 
#' @description make copies of image files into a new directory with a folder for each output class
#' 
#' @details this function takes your output results from deploy_model and sorts images into a new 
#' directory based on output class, with a separate folder for each class. If multiple classes are 
#' detected in a single image, copies of that image will be placed in every folder corresponding 
#' to a predicted detection. Also returns a df of the deploy_model output with a new column for 
#' updated filepath. Optionally removes original images. Optionally puts all images with detections 
#' below a certain threshold into a separate folder for manual review.
#' 
#' @param results df of output from deploy_model. Currently only supports 'long' prediction format
#' @param output_dir absolute path to directory in which to put sorted image folders
#' @param review_threshold images with detections below this threshold will be moved into a folder
#' titled "Manual_Review". Accepts values 0.01 - 1; default = NA.
#' @param count_subdir boolean. Create subfolders inside each class directory for individual counts.
#' @param remove_originals boolean. Delete original image files. Default = FALSE
#' 
#' @returns Image directory with images sorted by predicted class, score_threshold, count. 
#' Also returns a data frame of model_predictions that includes new/updated location for images.
#' 
#' @examples
#' data(preds)
#' # separate predictions with confidence score below 0.9 for manual review
#' results <- sort_images(preds, "./sorted_images/", 0.9)
#' 
#' @export
sort_images <- function(results = NULL, output_dir = NULL,
                        review_threshold = 0, count_subdir = FALSE, 
                        remove_originals = FALSE){
  
  # define output_dir; create it if it doesn't exist
  if(!fs::dir_exists(output_dir)){
    fs::dir_create(output_dir)
  }
  
  # create folders within output dir based on results classes
  classes <- unique(results$prediction)
  fs::dir_create(output_dir, classes)
    
  # make manual review folders
  if(review_threshold > 0){
    fs::dir_create(output_dir, classes, "manual_review")
  }
    
  # make count folders
  if(count_subdir){
    # filter results to a given class
    res_class <- results[results$class == class[i]]
    # get unique counts for that class
    counts <- sort(unique(res_class$count))
    fs::dir_create(output_dir, classes, counts)
  }

  # create new image name
  results$filename <- fs::path(results$filename)
  results <- dplyr::mutate(results,
                           new_name = paste(stringr::str_split_i(filename, "/", -2),
                                            stringr::str_split_i(filename, "/", -1), sep="_"))
  
  # loop through results and transfer files 
  for(i in 1:nrow(results)){
    
    # define prediction info
    pred <- results[i, ]
    old_loc <- pred$filename
    class <- pred$prediction
    conf <- pred$confidence_score
    count <- pred$count
    
    # create new image name 
    img_id <- utils::tail(strsplit(old_loc, "\\\\")[[1]], n = 2)
    img_name <- paste(img_id[1], img_id[2], sep="_")
    
    
    # create full path for new image loc/name
    if(conf < review_threshold){
      new_loc <- paste(output_dir, class, "manual_review", img_name, sep="/")
    } else if(count_subdir){
      new_loc <- paste(output_dir, class, count, img_name, sep="/")
    }
    else{
      new_loc <- paste(output_dir, class, img_name, sep="/")
    }
    
    # copy image to new location
    fs::file_move(old_loc, new_loc)
    
    # update new filename column
    new_filename[i] <- new_loc
    
    # create metadata tags
    # exif_tags <- write_exif_tags(class, conf, count, review_threshold)
    # exifr::exiftool_call(args = exif_tags, fnames = new_loc)
  }
  
  # add new_filename to results
  results_df <- cbind(results, new_filename)
  
  # rename col for old filename
  colnames(results_df)[colnames(results_df) == "filename"] = "old_filename"
  
  # perform action on original image based on user input
  if(remove_originals){
    file.remove(old_filename)
  }
  
  print("All images transferred")
  
  return(results_df)
}
