#' Aggregate predictions by sequence
#' 
#' @description Apply sequence id to results from the `deploy_model` function to 
#' aggregate predictions to the sequence level. 
#' 
#' @returns Returns a dataframe of predictions filtered down to the highest 
#' confidence prediction per class per sequence. 
#' 
#' @details If all predictions for a given sequence are "Empty", the first image
#' prediction in the sequence will be retained. If a sequence has non-"empty" 
#' predictions, all "Empty" predictions from that sequence will be removed. 
#' The function requires model output from the `deploy_model` function and sequence
#' information about the dataset. Sequences may be provided by the user or from the 
#' `generate_sequences` or `extract_metadata` functions. 
#' 
#' @param preds dataframe of predictions provided by `deploy_model`
#' @param sequences sequence information for the dataset included in "preds". 
#' Must be either a column in preds, or a dataframe with corresponding image paths 
#' sequence ids. A separate dataframe must have two columns: a column containing the absolute path
#' to images in the dataset that matches the corresponding column in "preds", and a
#' column of sequence ids corresponding to each image. Such a dataframe can be created
#' using the function `generate_sequences` and selecting the appropriate columns.
#' 
#' @import dplyr
#' 
#' 
#' @export
#' 
aggregate_predictions <- function(preds = NULL,
                                  sequences = NULL){
  # -- Warnings
  
  # warning if receiving the wrong type of input:
  if(!is.data.frame(preds)){
    stop(paste0(preds, " must be in data.frame format. 
                \nUse as.data.frame(", deparse(substitute(preds)), ") to format your predictions.\n"))
  }
  
  # identify column with filename/filepath
  file_path <- colnames(preds)[grepl("File", colnames(preds), ignore.case = TRUE)][1]
  # send error message if no matches
  if(length(file_path) == 0){
    stop(paste0("Cannot find column of absolute paths to image files in ", deparse(substitute(preds)), ". 
              \nPlease ensure this column contains the string 'file' .\n"))
  }
  
  # identify predictions column
  pred_col <- colnames(preds)[grepl("pred", colnames(preds), ignore.case = TRUE)][1]
  # send error message if no matches
  if(length(pred_col) == 0){
    stop(paste0("Cannot find prediction column in ", deparse(substitute(preds)), ". 
              \nPlease ensure this column contains the string 'pred'.\n"))
  }
    
  # warnings if cannot find confidence score column
  conf_col <- colnames(preds)[grepl("conf", colnames(preds), ignore.case = TRUE)][1]
  # send error message if no matches
  if(length(conf_col) == 0){
    stop(paste0("Cannot find column of confidence scores in ", deparse(substitute(preds)), ". 
              \nPlease ensure this column contains the string 'conf' .\n"))
  }
  
  # add warnings related to the sequences input
  
  # -- Join
  
  # if sequences is a separate df
  if(is.data.frame(sequences)){
    
    # identify column with filename/filepath
    file_col <- colnames(sequences)[grepl("File", colnames(sequences), ignore.case = TRUE)][1]
    # send error message if no matches
    if(length(file_col) == 0){
      stop(paste0("Cannot find column of file paths in ", deparse(substitute(sequences)), ". 
              \nPlease ensure this column contains the string 'file' .\n"))
    }
    
    # identify column of sequence ids
    seq_col <- colnames(sequences)[grepl("seq", colnames(sequences), ignore.case = TRUE)][1]
    # send error message if no matches
    if(length(seq_col) == 0){
      stop(paste0("Cannot find column of sequences in ", deparse(substitute(sequences)), ". 
              \nPlease ensure this column contains the string 'seq' .\n"))
    }
    
    seq_df <- sequences[, c(file_col, seq_col)]
    
    # merge dataframes
    preds <- merge(preds, seq_df, by.x = file_path, by.y = file_col, all.x = TRUE, all.y = FALSE)

  }
  
  # -- Aggregate

  # group df by sequence id and predicted class
  pred_agg <- dplyr::group_by(preds, sequences, prediction)
  
  # Keep highest confidence class-wise predictions for each sequence
  filtrd_preds <- dplyr::filter(pred_agg, confidence_in_pred == max(confidence_in_pred))
  
  # create df with sequences that have only empty predictions; keep just first prediction of each sequence
  empt_preds <- dplyr::filter(filtrd_preds, all(prediction == "Empty"))
  empt_seqs <- dplyr::slice(empt_preds, 1)
  
  # remove all empty predictions non-empty sequences
  nonempt_seqs <- dplyr::filter(filtrd_preds, prediction != "Empty")
  
  # merge the two dfs
  seq_df <- rbind(empt_seqs, nonempt_seqs)
  
  # ungroup the df and sort by seq_id
  seq_df <- dplyr::ungroup(seq_df)
  seq_preds <- dplyr::arrange(seq_df, seq_id)
  
  return(seq_preds)
}