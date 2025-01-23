#' Camelot bulk import
#' 
#' @description Format CameraTrapDetectoR results for bulk import into Camelot
#' image tagging software: https://camelotproject.org/
#' 
#' @param df output of CameraTrapDetectoR `deploy_model` function. The file_path column must match the local
#' paths to your images. If you did not extract image metadata while running the model, we will do that here.
#' @param project_df name of dataframe with required camelot fields. 
#' @param Camera_Name required column in project_df corresponding to unique camera name, with one row per camera. This marker must correspond to a character substring
#' of the filename in your CameraTrapDetectoR model output.
#' @param score_threshold recode any predictions below score threshold as empty.
#' Takes values between 0 and 1. Default is not to adjust any predictions
#' @param output_dir character string pointing to directory where you want the .csv file saved.
#' If left blank, the function will just return the dataframe to your R environment
#' 
#' @details The user may include the following information in \code{project_df}:  
#'      `Latitude` : column corresponding to camera latitude. If not included, we will create a dummy column that will allow uploading to Camelot but cannot be used in analysis.
#'      `Longitude` : column corresponding to camera longitude. If not included, we will create a dummy column that will allow uploading to Camelot but cannot be used in analysis.
#'      `TrapStation` : column corresponding to camera trap station name, with a maximum two cameras per trap station. This field is useful for multiple perspectives of the same view. 
#'      If not provided, we will create a dummy column defaulting to `Camera_Name`.
#'      `StartDate` : column corresponding to start date for each camera. If not provided, we will extract this info from image metadata.
#'      `EndDate` : column corresponding to end date for each camera. If not provided, we will extract this info from image metadata.
#' 
#' @returns data frame formatted for import to Camelot, preserving all information from both dataframes in one file.
#' 
#' @import dplyr
#' @import lubridate
#' @import stringr
#' @import data.table
#' 
#' @export
camelot_bulk_import <- function(df, project_df, Camera_Name = "Camera_Name", 
                                score_threshold=0, output_dir = "None"){

  
  # confirm df is output from CTD - R or cl versions
      # it has a prediction column, file_path column, count col, confidence 
  ctd_cols <- c("filename", "prediction", "confidence") 
  if(!all(ctd_cols %in% colnames(df))) {
    stop(paste0("Cannot find necessary columns in your dataframe pointing to CameraTrapDetectoR results.  
             Please check that your dataframe has columns for the full filepath to your images, 
             model predictions, and confidence scores.
             The column names must be: filename, prediction, confidence.\n"))
  }
  
  # check df for metadata; if it does not have any, extract it
  if(!("timestamp" %in% stringr::str_to_lower(colnames(df)))){
    print("We cannot find image metadata in your CameraTrapDetectoR results.\n
          Please be patient while we extract it now, if we can.\n
          The filepath in your data must match the local filepaths to your images.")
    
    # get list of unique full filenames
    img_files <- unique(df$filename)
    
    # extract metadata
    tryCatch( { meta_df <- extract_metadata(img_files) }, error = function(e) {
      stop(paste0("The file paths in your prediction data do not match image files on your machine.
                  Please edit this column and try again."))
      })
    
    
    # join metadata to df
    df <- dplyr::left_join(df, meta_df, by = "filename")
  }
  
  # format timestamp as recommended by Camelot
  df$timestamp <- lubridate::ymd_hms(df$timestamp)
  
  # verify camera name column is in project_df
  if(!Camera_Name %in% colnames(project_df)){
    stop(paste0(Camera_Name, " does not correctly name a column in your project_df.
                  Please edit this field and try again."))
  }
  
  # extract camera name as separate column in df
  cameras <- unique(project_df[,Camera_Name])
  df <- dplyr::mutate(df, Camera_Name = stringr::str_extract(filename, stringr::str_c(cameras, collapse="|")))
  
  # rename camera name col in project_df for easier downstream formatting
  project_df <- dplyr::rename(project_df, Camera_Name = tidyselect::all_of(Camera_Name))
  
  # search for lat/long columns in project_df
  lat_cols <- c("lat", "latitude")
  long_cols <- c("long", "longitude")

  
  # add dummy columns if none found
  if(!(any(long_cols %in% stringr::str_to_lower(colnames(project_df))) && 
       any(lat_cols %in% stringr::str_to_lower(colnames(project_df))))) {

    cat("No variables for lat/long detected in your project df; creating dummy variables.\n Note: these coordinates are not usable in analysis.")
    
    # create dummy vars 
    project_df <- dplyr::mutate(project_df, Latitude = 40.7826, Longitude = -73.9656)
  }
  
  
  # make sure loc columns are numeric and all present
  latcol <- colnames(project_df)[which(stringr::str_to_lower(colnames(project_df)) %in% lat_cols)]
  project_df[, latcol] <- suppressWarnings(as.numeric(iconv(project_df[, latcol], from = 'UTF-8', to = 'ASCII//TRANSLIT')))
  missing_lat <- project_df$Camera_Name[which(is.na(project_df[,latcol]))]
  missing_long <- project_df$Camera_Name[which(is.na(project_df[,longcol]))]
  if(length(missing_lat) + length(missing_long) > 0){
    stop(paste0("Your project df is missing latitude coordinates for ", missing_lat, "
                and longitude coordinates for ", missing_long, ". Please fix it."))
  }
  
  # add start/end dates to project df
  startend_times <- df %>%
    dplyr::group_by(Camera_Name) %>%
    dplyr::summarize(StartDate = min(timestamp, na.rm=T), EndDate = max(timestamp, na.rm=T))
  
  # rename our columns if project_df already has columns of the same name
  if("StartDate" %in% colnames(project_df)) {
    startend_times <- dplyr::rename(StartDate_from_Metadata = StartDate)
  }
  if("EndDate" %in% colnames(project_df)) {
    startend_times <- dplyr::rename(EndDate_from_Metadata = EndDate)
  }
  
  project_df <- dplyr::left_join(project_df, startend_times, by = "Camera_Name")
  
  # add trap station column
  if(!"TrapStation" %in% colnames(project_df)) {
    project_df <- dplyr::mutate(project_df, TrapStation = Camera_Name)
  }
  
  # join project df to df
  camelot_df <- dplyr::left_join(df, project_df, by = "Camera_Name")
  
  # make sure empties are standardized
  camelot_df <- dplyr::mutate(camelot_df,
                              prediction = if_else(prediction == "empty", "Empty", prediction))
  
  # perform score threshold filtering
  if(score_threshold > 0){
    camelot_df <- dplyr::mutate(camelot_df,
                                prediction = if_else(confidence < score_threshold, "Empty", prediction))
  }
  
  # save file if requested
  if(output_dir != "None"){
    if(!fs::dir_exists(output_dir)){
      tryCatch( {fs::dir_create(output_dir)}, error = function(e) {
        stop(paste0(output_dir, " is not, nor can it be made, into a valid directory."))
      })
    }
    data.table::fwrite(camelot_df, fs::path(output_dir, "camelot_bulk_import", ext="csv"))
  }
  
  return(camelot_df)

}
