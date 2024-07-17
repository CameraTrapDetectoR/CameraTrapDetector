#' Extract sequence and metadata info and assign info to image filename
#' 
#' @description create df of metadata and sequence information for all images in user image directory
#' 
#' @details This function extracts metadata and generates sequence information for your image dataset. 
#' You may either specify a path to your image directory or a character vector listing absolute paths 
#' to all images in your dataset. If your image metadata includes camera and sequence information, 
#' these will be returned. If you provide a string vector of camera ids represented in your directory, 
#' sequence length and wait time between triggers, camera and sequence identifiers based on directory
#' and timestamp information will also be returned. This function utility depends on either the 
#' sufficient metadata fields in each image or reasonable directory organization in order to generate 
#' accurate sequence information. Sequences with different burst lengths must  be run through the function 
#' separately if relying on `cam_prefix` and image timestamp to generate sequence information.    
#' 
#' @param data_dir absolute path to image directory or list of absolute path of image file names. If you specify a 
#' directory, the function will search recursively through this directory for all jpeg files. 
#' @param burst_length number of images per sequence burst.
#' @param wait_time time between bursts in seconds. If your cameras have a 10-minute wait time 
#' between triggers, your wait time is 600. 
#' @param cam_prefix character vector, where each value corresponds to a unique name 
#' for each camera in your dataset as it appears in the absolute path. The function will use
#' this identifier if your camera metadata do not contain serial numbers. 
#' 
#' @rawNamespace import(lubridate, except = show)
#' @import stringr
#' @import dplyr
#' 
#' @examples
#' data(preds)
#' seq_preds <- generate_sequences(preds$filename, c("example_set"), 5, 300)
#' 
#' @export
generate_sequences <- function(data_dir = NULL, 
                             cam_prefix = NULL,
                             burst_length = 1, 
                             wait_time = 0) {
  
  # -- load operators so we can use the dataset function
  load_operators()
  
  # -- Get list of files
  # make sure data dir is entered
  if(length(data_dir) == 0) {
    stop("Cannot find directory or files. Please enter properly formatted `data_dir`.\n")
  }
  
  # get file list from directory
  if(length(data_dir) == 1){
    if(fs::dir_exists(data_dir)){
      file_list <- dataset(data_dir, recursive=TRUE, file_extensions=".jpg")
    }
  }
  
  # if data_dir is a list of files, keep only unique image files
  if(length(data_dir) > 1){
    # keep actual files, remove duplicates
    file_list <- unique(data_dir[fs::file_exists(data_dir)])
    # keep only jpg files
    file_list <- file_list[which(!is.na(stringr::str_match(file_list, stringr::regex(".jpg", ignore_case = TRUE))))]
    if(length(file_list) == 0){
      stop("Cannot find any valid image files. Please enter properly formatted `data_dir`.\n")
    }
  }
  
  # -- Extract metadata
  meta_df <- extract_metadata(file_list)
  
  # -- Create directory-based camera IDs
  
  # check cam_prefix argument
  if(is.null(cam_prefix)){
    stop("Please enter at least one string for camera ID.\n")
  }
  
  # remove duplicate camera ids
  cam_prefix <- unique(cam_prefix)
  
  # coerce cam_prefix to character
  cam_prefix <- as.character(cam_prefix)
  
  # create new column for dir-based camera id
  meta_df$CameraId <- NA
  
  # loop through values of camera id
  for(i in 1:length(cam_prefix)) {
    # comb filepaths for matches to each camera id
    id <- data.frame(id_match = stringr::str_match(meta_df$FilePath, cam_prefix[i]))
    
    # add matches to df
    meta_df$CameraId <- ifelse(is.na(meta_df$CameraId), id$id_match, meta_df$CameraId)
  }
  
  # -- Create timestamp-based sequence ids
  if(burst_length > 1) {
    
    # convert timestamp from character to date
    meta_df$TimeStamp <- lubridate::ymd_hms(meta_df$TimeStamp)
    
    # create empty column to add sequence id
    meta_df$SequenceId <- NA
    
    # sort obs by camera, timestamp
    meta_df$TimeDiff <- NA
    meta_df <- meta_df[with(meta_df, order(CameraId, TimeStamp)), ]
    
    # calculate time between obs
    for(i in 2:nrow(meta_df)){
      meta_df$TimeDiff[i] <- as.numeric(lubridate::dseconds(
        lubridate::as.duration(meta_df$TimeStamp[i] - meta_df$TimeStamp[i-1])))
    }
    
    # randomly assign starting sequence id
    seq_no <- floor(stats::runif(1, 100, 1001))
    
    # create vector to hold ids
    meta_df$SequenceId <- NA
    
    # manually set first id
    meta_df$SequenceId[1] <- paste0(meta_df$CameraId[1], "_SEQ", seq_no)
    
    # loop through rest of df and update sequence ids
    for(i in 2:nrow(meta_df)){
      
      # update sequence id based on wait time, burst length, cam id
      if(meta_df$TimeDiff[i] > wait_time | 
         (meta_df$CameraId[i] != meta_df$CameraId[i-1]) | 
         (i %% burst_length == 1)) {
        seq_no <- seq_no + 1
      } 
      
      #set sequence id
      meta_df$SequenceId[i] <- paste0(meta_df$CameraId[i], "_SEQ", seq_no)
    }
  }
  
  # -- Remove TimeDiff column
  seq_df <- dplyr::select(meta_df, !TimeDiff)
  
  return(seq_df)
}
#END