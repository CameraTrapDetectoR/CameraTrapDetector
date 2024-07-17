#' Deploy model on camera trap images
#' 
#' @description This function deploys a model trained to identify and count the objects
#' in camera trap images. 
#' 
#' This function deploys a customized computer vision model to detect and 
#' classify objects in camera trap images. The function will find all indicated image
#' files in the user-specified directory and deploy the user-specified model on these
#' images. 
#' 
#' @details 
#' CameraTrapDetectoR contains four types of models: the 'general' classifies mammals, 
#' birds, humans, and vehicles; the 'family' model classifies taxonomic families; 
#' the 'species' model classifies taxonomic species; the 'pig_only' model classifies
#' wild pigs. The most recent version of a given model will be deployed unless a
#' previous version is expressly specified, i.e. \code{model_type='species_v1'}.
#' Each model type and version) is trained independently to give the user the 
#' option to run multiple model types and/or versions on the same dataset sequentially, 
#' and treat each set of predictions as an independent observer.
#' 
#' Model version details, including class-wise results on an out-of-sample test
#' data set, are available on the CameraTrapDetectoR github repository.
#' 
#' @returns a data frame of model predictions, with predicted number of individuals
#' within each detected class for each image in the data directory. This data frame 
#' is automatically saved as a .csv file in the output directory, along with a .txt
#' file of all arguments used in that instance of the `deploy_model` function. 
#' 
#' If the user specifies, a .csv of bounding box coordinates is also automatically 
#' saved in the output directory.If the user requests image plots with bounding boxes,
#' these plotted image copies will be saved as .png files in the output directory. 
#' 
#' @param data_dir Absolute path to the folder containing your images
#' @param output_dir You can specify absolute path to output. Default is `NULL`,
#'  and creates a folder within your data_dir. If you are resuming evaluation from a checkpoint,
#'  put the  full path to the desired prediction directory here. 
#'  All previous model arguments will also be loaded and do not need to be specified again.
#' @param recursive boolean. Do you have images in subfolders within your
#'  data_dir that you want to analyze, if so, set to TRUE. If you only want to 
#'  analyze images within your data_dir and not within sub-folders, set to FALSE.
#' @param model_type Options are 'general', 'species', 'family', or 'pig_only' 
#'  with appropriate version appended with an underscore. For example, to use the version 1 species model,
#'  list `model_type` as "species_v1". Specifying a model type without a version appended will default to
#'  the latest version of that model available for your package vesion.
#'  A full list of available models and details is available 
#'  on the CameraTrapDetectoR Github wiki.
#' @param file_extensions The types of extensions on your image files. Case insensitive; enter as a string.
#' Accepts the following file types: ".jpg", ".png", ".tif", ".pdf". Default is ".jpg"
#' @param make_plots boolean. Do you want to make plots of the images with
#'  their predicted bounding boxes?
#' @param sample50 boolean. Do you want to run the model only on a subset of 
#'  50 images from your dataset? This is a good idea if you are experimenting 
#'  with settings. 
#' @param write_bbox_csv boolean. Do you want to create a csv with all of the 
#'  information on predicted bounding boxes? This csv will include all bounding boxes,
#'  even those with low probability. 
#' @param score_threshold Confidence threshold for using a bounding box, accepts 
#'  values from 0-1. A lower number will produce more bboxes (it will be less
#'  stringent in deciding to make a bbox). A higher number will produce fewer
#'  bboxes (it will be more stringent).
#' @param overlap_correction boolean. Should overlapping detections be
#' evaluated for overlap and highest confidence detection be returned
#' @param overlap_threshold Overlap threshold used when determining if bounding box
#' detections are to be considered a single detection. Accepts values from 0-1
#' representing the proportion of bounding box overlap.
#' @param get_metadata boolean. Collect metadata for each image.
#' @param write_metadata boolean. Write prediction info to image metadata
#' @param review_threshold Confidence threshold to accept predictions when writing image metadata 
#' @param checkpoint_frequency Number of images to run between saving a checkpoint. Default is 10;
#' can be any positive integer; if it is larger than the size of your dataset, no checkpoints will be saved,
#' @param latitude image location latitude. Use only if all images in the model run come from the same location.
#' @param longitude image location longitude. Use only if all images in the model run come from the same location.
#' @param h The image height (in pixels) for the annotated plot. Only used if
#'  \code{make_plots=TRUE}. 
#' @param w The image width (in pixels) for the annotated plot.
#' @param lty line type for bbox plot. See \code{?plot} for details
#' @param lwd line width for bbox plot. See \code{?plot} for details
#' @param col line color for bbox plot. See \code{?plot} for details
#' 
#' @examples
#' # path to sample images shipped with the package
#' data_dir <- get_samples() 
#' 
#' # specify only the data_dir, except all other default args
#' df_gen <- deploy_model(data_dir = data_dir)
#' 
#' # run species model at varying confidence score thresholds
#' scores <- c(0.3, 0.5, 0.7)
#' for(i in 1:length(scores)){
#' deploy_model(data_dir = data_dir, model_type = 'species', score_threshold = scores[i])
#' }
#' 
#' # loop through each model
#' data(models)
#' for(i in 1:nrow(models)){
#' deploy_model(data_dir = data_dir, model_type = models$model_name[i])
#' }
#'  
#' @import torch
#' @import torchvision
#' @import torchvisionlib
#' @import magick
#' @import dplyr
#' 
#' @export
deploy_model <- function(
    data_dir = NULL,
    output_dir = NULL,
    model_type = 'species',
    recursive = TRUE,
    file_extensions = c(".jpg"),
    make_plots = TRUE,
    sample50 = FALSE, 
    write_bbox_csv = FALSE, 
    score_threshold = 0.6,
    overlap_correction = TRUE,
    overlap_threshold = 0.7,
    get_metadata = TRUE,
    write_metadata = FALSE,
    review_threshold = 0.8,
    checkpoint_frequency = 10,
    latitude = NA,
    longitude = NA,
    h=307,
    w=408,
    lty=1,
    lwd=2, 
    col='red')
{
  
  #-- Check arguments provided 
  
  # compile args into list
  arg_list <- list(
    data_dir = fs::path_norm(data_dir),
    output_dir = output_dir,
    model_type = model_type,
    recursive = recursive,
    file_extensions = file_extensions,
    make_plots = make_plots,
    sample50 = sample50, 
    write_bbox_csv = write_bbox_csv, 
    score_threshold = score_threshold,
    overlap_correction = overlap_correction,
    overlap_threshold = overlap_threshold,
    get_metadata = get_metadata,
    write_metadata = write_metadata,
    review_threshold = review_threshold,
    checkpoint_frequency = checkpoint_frequency,
    latitude = latitude,
    longitude = longitude,
    h=h,
    w=w,
    lty=lty,
    lwd=lwd, 
    col=col
  )
  
  # pass through checks
  arg_list <- verify_args(arg_list)
  
  # look for model arguments in output dir
  arg_list <- suppressWarnings(tryCatch(arg_list <- load_args(output_dir), 
                                        error = function(e) arg_list))
  
  # from here on, call each argument from the list for consistency

  
  #########
  #-- Prep data
  
  # load inputs
  file_list <- define_dataset(arg_list$data_dir, arg_list$recursive, arg_list$file_extensions)
  
  # take random sample if sample50=TRUE  
  if(arg_list$sample50==TRUE && length(file_list) > 50){
    file_list <- sample(file_list, 50)
  }
  
  # set placeholder for predicted bboxes
  if(arg_list$write_bbox_csv==TRUE){
    bboxes <- NULL
  }
  
  #######
  #-- Load checkpoint
  
  # set placeholder for saved results
  results <- NULL
  
  # load checkpoint
  if(!is.null(output_dir)){
    
    # load saved results
    results <- chkpt_df(output_dir, arg_list$model_version, "model_predictions")
    
    # update file list
    if(length(results) > 0) {
      file_list <- update_img_list(results, arg_list$model_version, file_list)
    }
    
    # load saved bboxes
    if(arg_list$write_bbox_csv==TRUE){
      bboxes <- chkpt_df(output_dir, arg_list$model_version, "predicted_boxes")
    }
  }
  
  # set output directory
  if(is.null(output_dir)){
    output_dir <- set_output_dir(data_dir, arg_list$model_version, arg_list$recursive, arg_list$make_plots)
  }
  
  #############
  #-- Load model
  
  # download model files
  folder <- download_models(models=arg_list$model_version)
  
  # load label encoder
  label_encoder <- encode_labels(folder)
  
  # load model
  model <- weight_loader(folder)
  model$eval()

  
  # Write Arguments to File
  write_args(arg_list, output_dir)
  
  #define location-restricted labels
  if (is.na(arg_list$latitude) & is.na(arg_list$longitude)) {
    location <- NULL
  } else {
    location <- data.frame(longitude=arg_list$longitude, latitude=arg_list$latitude)
  }
  
  if(is.null(location) == FALSE){
    possible_labels <- encode_locations(location, arg_list$model_type, label_encoder)
  }
  
  ############
  #-- Make predictions for each image
  
  # empty list to hold predictions from loop
  predictions_list <- list()
  
  # add progress bar
  cat(paste0("\nDeploying model on ", length(file_list), " images. Two warnings will appear; ignore these. 
             \nResults files are saved every ", arg_list$checkpoint_frequency, " images in: ", fs::path_norm(output_dir), "\n"))
  if(make_plots){
    cat(paste0("During deployment, you can optionally view predicted bounding boxes as they are produced."))
  }
  pb = utils::txtProgressBar(min = 0, max = length(file_list), initial = 0,
                             style=3, char="*")  
  
  # explicitly load torch packages into environment
  suppressWarnings(suppressPackageStartupMessages({
    library(torch)
    library(torchvision)
    library(torchvisionlib)
  }))

  # loop over each image
  toc <- Sys.time()
  torch::with_no_grad({
    for(i in 1:length(file_list)){
      
      # define filename
      # filename <- normalizePath(file_list[i], winslash = "/")
      filename <- fs::path_norm(file_list[i])
      
      # load image and convert to model input
      input <- get_model_input(filename)
      
      # handle any errors, else run the model
      if(is.data.frame(input)) {
        predictions_list[[i]] <- pred_df
      } else {
        
        # deploy the model on the image
        pred_df <- eval_one_image(input, filename, label_encoder, 
                                  arg_list$overlap_correction, arg_list$overlap_threshold,
                                  location, possible_labels, model)
        
        # add prediction df to list
        predictions_list[[i]] <- pred_df
        
        # make plots
        if(arg_list$make_plots==TRUE){
          # subset by score threshold for plotting
          pred_df_plot <- pred_df[pred_df$confidence_score >= arg_list$score_threshold, ]
          
          # plot predictions
          plot_img_bbox(filename, pred_df_plot, arg_list$output_dir, arg_list$data_dir, 
                        arg_list$col, arg_list$lty, arg_list$lwd, arg_list$w, arg_list$h)
        }
        
        # write metadata tags
        if(arg_list$write_metadata){
          write_metadata_tags(pred_df = pred_df, model_version = arg_list$model_version, 
                              review_threshold = arg_list$review_threshold)
        }
      }
 
      # save checkpoint
      if (i %% arg_list$checkpoint_frequency == 0) {
        
        df_out <- save_checkpoint(predictions_list, arg_list$score_threshold,
                                  bboxes, output_dir, arg_list$model_version,
                                  arg_list$get_metadata, arg_list$write_bbox_csv, results, final=F)

        cat(paste0("\nResults saved for ", i, " images.\n"))
      }
    
    # update progress bar
    utils::setTxtProgressBar(pb,i) 
      
    }# end for loop
    
  })
  
  tic <- Sys.time()
  runtime <- difftime(tic, toc, units="secs")
  timeper <- runtime/length(file_list)
  cat(paste0("\nInference time of: ", round(timeper, 3), " seconds per image."))
  
  
  #-- Make Output Files
  
  df_out <- save_checkpoint(predictions_list, arg_list$score_threshold,
                            bboxes, output_dir, arg_list$model_version,
                            arg_list$get_metadata, arg_list$write_bbox_csv, results, final=T)

  
  cat(paste0("\nOutput can be found at: \n", fs::path_norm(output_dir), "\n",
             "The number of animals predicted in each category in each image is in the file: ", arg_list$model_version, "_model_predictions.csv\n"))
  
  
  # return output dataframe
  return(df_out)

}
#### --- END
