# Verify Args -------------------------------------------------
#' @export
verify_args <- function(arg_list) {
  
  #-- Check arguments provided 
  
  # check data_dir is a real directory
  if(!fs::is_dir(data_dir)) {
    stop(paste0("data_dir is not a true directory. 
                Please re-enter the full path to your image directory."))
  }
  
  # check model_type
  models_available <- c('general', 'general_v1', 'general_v2',
                        'species', 'species_v1', 'species_v2', 'species_v3',
                        'family', 'family_v1', 'family_v2',
                        'pig_only', 'pig_only_v1')
  if(!arg_list$model_type %in% models_available) {
    stop(paste0("model_type must be one of the available options: ",
                list(models_available)))
  }
  
  # define model version
  arg_list$model_version <- arg_list$model_type
  if(arg_list$model_version %in% c('general', 'family')){
    arg_list$model_version <- paste(arg_list$model_version, "v2", sep="_")
  }
  if(arg_list$model_version %in% c('species')){
    arg_list$model_version <- paste(arg_list$model_version, "v3", sep="_")
  }
  
  # check ext types
  acceptable_exts <- c(".jpg", ".png", ".tif", ".pdf",
                       ".JPG", ".PNG", ".TIF", ".PDF")
  extension_test <- arg_list$file_extensions %in% acceptable_exts
  if(!all(extension_test)){
    stop(paste0(c("One or more of the `file_extensions` specified is not an accepted format. Please choose one of the accepted formats: \n",
                  acceptable_exts), collapse = " "))
  }
  
  # test overlap_threshold
  if (arg_list$overlap_threshold < 0 | arg_list$overlap_threshold > 1){
    stop("overlap_threshold must be between [0, 1]")
  }
  
  # test score_threshold
  if (arg_list$score_threshold < 0 | arg_list$score_threshold > 1){
    stop("score_threshold must be between [0, 1]")
  }
  
  # test checkpoint frequency
  if (arg_list$checkpoint_frequency <= 0) {
    stop("checkpoint frequency must be a positive integer.")
  }
  if (arg_list$checkpoint_frequency %% 1 != 0) {
    stop("checkpoint frequency must be a positive integer.")
  }
  
  # test review_threshold
  if (arg_list$review_threshold < 0 | arg_list$review_threshold > 1){
    stop("review_threshold must be between [0, 1]")
  }
  
  # check location arguments
  if (!is.na(arg_list$latitude)) {
    if (arg_list$latitude < -90 | arg_list$latitude > 90){
      stop("latitude must be between -90 and 90")
    } 
  }
  if (!is.na(arg_list$longitude)) {
    if (arg_list$longitude < -180 | arg_list$latitude > 180) {
      stop("longitude must be between -180 and 180")
    }
  }
  if (is.na(arg_list$latitude) & !is.na(arg_list$longitude)){
    stop("invalid location; please include both latitude and longitude or leave both blank")
  }
  
  if (!is.na(arg_list$latitude) & is.na(arg_list$longitude)){
    stop("invalid location; please include both latitude and longitude or leave both blank")
  }
  
  # test lty 
  lty_options <- 1:6
  if(!arg_list$lty %in% lty_options){
    stop("invalid lty option selected. Please select an integer from 1-6")
  }
  
  # test color
  tryCatch({grDevices::col2rgb(arg_list$col)}, error=function(e) {
    print('col value entered is not a valid value')})
  
  # test lwd
  if (arg_list$lwd <= 0){
    stop("lwd value must be greater than 0")
  }
  
  return(arg_list)
}

# Write Args to File ----------------------
#' @export
write_args <- function(arg_list, output_dir) {
  # add updated output dir to arg list1
  arg_list$output_dir <- fs::path_norm(output_dir)
  
  # convert multiple file extensions to single string
  arg_list$file_extensions <- paste(arg_list$file_extensions, collapse = ", ")
  
  # convert to data frame
  arg_df <- arg_df <- t(as.data.frame(arg_list))
  
  # write args to txt file
  write.table(arg_df, file = fs::path(output_dir, "arguments", ext = "txt"), 
              sep = ": ", col.names = F)
}




# Load checkpoint --------------------------

#' @export
load_args <- function(output_dir){
  # read in arguments
  arg_file <- read.table(fs::path(output_dir, "arguments", ext = "txt"), 
                          sep = ":", col.names = c("arg", "value"))
  
  # convert args to data frame
  arg_df <- tidyr::pivot_wider(arg_file, names_from = arg, values_from = value)
  
  # format whitespace
  arg_df <- dplyr::mutate(arg_df, dplyr::across(everything(), ~stringr::str_trim(.)))
  
  # convert col types
  arg_df <- dplyr::mutate(arg_df, dplyr::across(c(recurse, make_plots, sample50,
                                                    write_bbox_csv, overlap_correction, get_metadata,
                                                    write_metadata), ~as.logical(.)))
  arg_df <- suppressWarnings(dplyr::mutate(arg_df, dplyr::across(c(score_threshold, overlap_threshold,
                                                    review_threshold, checkpoint_frequency,
                                                    latitude, longitude, h, w, lty, lwd), ~as.numeric(.))))
  
  # separate multiple values into a vector
  arg_df$file_extensions <- stringr::str_split(arg_df$file_extensions, pattern = ", ")
  
  # convert to list
  arg_list <- as.list(arg_df)
  arg_list$file_extensions <- unlist(arg_list$file_extensions)
  
  return(arg_list)
  }

#' @export
chkpt_df <- function(output_dir, model_version, typo){
  # check for saved results files
  chkpt_path <- fs::dir_ls(output_dir, regexp = paste(model_version, typo, sep = "_"), ignore.case = TRUE)
  if(length(chkpt_path) == 0){
    return(NULL)
  } else {
    df <- do.call(rbind, lapply(chkpt_path, utils::read.csv))
    df <- unique(df)
    return(df)
  }
}

#' @export
update_img_list <- function(results, model_version, file_list){
      
    # extract filenames 
    results_files <- unique(fs::path_real(results$filename))
    
    # filter predictions out of file list
    file_list <- file_list[!file_list %in% results_files]
    
    # remove any prediction plots
    file_list <- file_list[stringr::str_detect(file_list, "predictions", negate = T)]
    
    # exit function if all images have already been run
    if(length(file_list) == 0){
      stop(print(paste0("All images in your chosen directory have already been run on the ", model_version, " model. 
                        \nTo run the same model on the same images with different hyperparameters, please reset those parameters and leave the <output_dir> argument blank. 
                        \nOtherwise, please choose another model or image directory.")))
    } else{
      # print message
      cat(paste0("Saved model results are loaded. Model will run only on images in not already in saved results."))
    }
  return(file_list)
  }

# Save checkpoint ---------------------------------

save_checkpoint <- function(predictions_list, score_threshold,
                            bboxes, output_dir, model_version,
                            get_metadata, write_bbox_csv, results, final) {
 
   # filter df by score_threshold
  full_df <- apply_score_threshold(predictions_list, score_threshold)
  
  # write bounding box file
  if(write_bbox_csv){
    bbox_df <- write_bbox_df(full_df, bboxes, score_threshold)
    if(final){
      utils::write.csv(bbox_df, fs::path(output_dir, paste(model_version, "predicted_bboxes", sep="_"), ext="csv"), 
                       row.names=FALSE)
      # remove checkpoint file
      if(file.exists(fs::path(output_dir, paste(model_version, "predicted_bboxes_checkpoint.csv", sep="_")))){
        file.remove(fs::path(output_dir, paste(model_version, "predicted_bboxes_checkpoint.csv", sep="_")))
      }
    } else{
      utils::write.csv(bbox_df, fs::path(output_dir, paste(model_version, "predicted_bboxes_checkpoint", sep="_"), ext="csv"), 
                       row.names=FALSE)
    }

  }
  
  # convert to output format
  df_out <- write_output(full_df)
  
  # extract metadata if requested
  if(get_metadata){
    meta_df <- extract_metadata(df_out$filename)
    # remove all NA columns
    meta_df <- remove_na(meta_df)
    # join metadata to results
    df_out <- dplyr::left_join(df_out, meta_df, 
                               dplyr::join_by(filename), 
                               suffix = c("", ".y"), keep=FALSE)
    # remove duplicates
    df_out <- dplyr::select(df_out, -ends_with(".y"))
  }
  
  # cat previous results if they exists
  if(!is.null(results)){
    df_out <- unique(dplyr::bind_rows(results, df_out))
  }
  
  # save predictions to csv
  if(final){
    utils::write.csv(df_out, fs::path(output_dir, paste(model_version, 'model_predictions', sep="_"), ext="csv"), row.names=FALSE)
    # remove checkpoint file
    if(file.exists(fs::path(output_dir, paste(model_version, "model_predictions_checkpoint.csv", sep="_")))){
      file.remove(fs::path(output_dir, paste(model_version, "model_predictions_checkpoint.csv", sep="_")))
    }
  } else{
    utils::write.csv(df_out, fs::path(output_dir, paste(model_version, 'model_predictions_checkpoint', sep="_"), ext="csv"), row.names=FALSE)
  }
  
  
  return(df_out)
}

# Set output directory ------------------------------
#' @export
set_output_dir <- function(data_dir, model_version, recurse, make_plots){
  
  # make new output dir
  datenow <- format(Sys.Date(), "%Y%m%d")
  now <- unclass(as.POSIXlt(Sys.time()))
  current_time <- paste0("_", datenow, "_", sprintf("%02d", now$hour), 
                         sprintf("%02d", now$min), 
                         sprintf("%02d", round(now$sec)))
  output_dir <- fs::path(data_dir, paste0("predictions_", model_version, current_time))
  fs::dir_create(output_dir)
  
  # make recursive directories if needed
  if(recurse && make_plots) {
    rec_dirs <- fs::dir_ls(data_dir, type = "directory")
    rec_dirs <- rec_dirs[stringr::str_detect(rec_dirs, "prediction", negate = T)]
    fs::dir_create(stringr::str_replace_all(rec_dirs, data_dir, output_dir))
    }
  
  return(output_dir)
}


# Encode labels -----------------------------
#' @export
encode_labels <- function(folder) {
  # load label encoder
  label_encoder <- utils::read.table(fs::path(folder, "label_encoder", ext="txt"), 
                                     sep = ":", col.names = c("label", "encoder"))
  
  # standardize label format
  label_encoder <- dplyr::mutate(label_encoder,
                                 label = gsub("'", "", label),
                                 label = gsub(" ", "_", label),
                                 label = gsub("-", "_", label),
                                 label = tools::toTitleCase(label))
  
  return(label_encoder)
}

# Make possible labels ----------------------
#' @export
encode_locations <- function(location, model_type, label_encoder) {
  #Load species extent data
  # extent.data <- species.extent.data
  
  #Get possible labels based on model class
  possible.labels <- get_possible_species(location, species.extent.data, model_type)
  
  # filter out possible labels not in labels
  possible.labels <- possible.labels[possible.labels$label %in% label_encoder$label,]
  
  # print message
  cat(paste0("\nIdentified ", nrow(possible.labels), " taxa out of ", nrow(label_encoder), " possible taxa.\n"))
  
  return(possible.labels)
}


