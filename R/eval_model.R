#' Evaluate model results
#' 
#' @description Join model_predictions output to a user-generated df of ground 
#' truth identifications and assess overall and/or class-wise performance
#' 
#' @details This function takes your output results from deploy_model and joins them
#' to a data frame with ground truth identifications. The final object returned is a list containing 
#' overall and class-wise evaluation metrics for class identification at the image and/or
#' sequence level, and optionally for counts at the image and/or sequence level.
#' 
#' 
#' @param preds df of output from deploy_model. Currently only supports 'long' prediction format
#' @param data ground truth results. Must be an R data frame and contain the following columns:
#' @param filepath name of the column in `data` that contains absolute paths to the the images being assessed
#' @param true_class name of the column in `data` that contains ground truth classifications. 
#' Must match the class level of `preds` i.e. if `preds` contained predictions for the species model, this
#' column must contain ground truth species. 
#' @param assess_counts boolean. Evaluate how well the model predicts counts in addition to classes. Default = FALSE.
#' @param true_count name of the column in `data` that contains ground truth counts for the class listed in 
#' `true_class` for a given row.  
#' @param event_level evaluate results at the individual level or at the sequence level? 
#' Accepts values `c("image", "sequence")`. If including evaluation by sequence, `seq_id` must be specified
#' @param seq_id name of the column in `data` that contains sequence ids for each image. If you need to generate this
#' information, see the `generate_sequences` function.
#' 
#' @import dplyr
#' 
#' @export
eval_model <- function(preds = NULL, data = NULL,
                       filepath = NULL, true_class = NULL, 
                       assess_counts = FALSE, true_count = NULL,
                       event_level = "image", seq_id = NULL){
  
  # -- Warnings
  
  # warnings if receiving the wrong type of input:
  if(!is.data.frame(preds)){
    stop(paste0(preds, " must be in data.frame format. 
                \nUse as.data.frame(", deparse(substitute(preds)), ") to format your predictions.\n"))
  }
  
  # warnings if cannot find filename column
  if(!("filename" %in% colnames(preds))){
    stop(paste0("Cannot find column of absolute paths to image files in ", deparse(substitute(preds)), ". 
                \nPlease ensure this column is named 'filename' .\n"))
  }
  
  # warnings if cannot find predictions column
  if(!("prediction" %in% colnames(preds))){
    stop(paste0("Cannot find column of predictions in ", deparse(substitute(preds)), ". 
                \nPlease ensure this column is named 'prediction' .\n"))
  }
  
  if(!is.data.frame(data)){
    stop(paste0(data, " must be in data.frame format. 
                \nUse as.data.frame(", deparse(substitute(data)), ") to format your annotations.\n"))
  }
  
  # warning if incorrect format for counts 
  if(assess_counts & is.null(true_count)){
    stop(paste0("Please provide the column name for animal counts in ", deparse(substitute(data)), 
                " in the argument `true_count`\n"))
  }
  
  # warning if incorrect format for counts 
  if(("sequence" %in% event_level) & is.null(seq_id)){
    stop(paste0("Please provide the column name for sequence id in ", deparse(substitute(data)), 
                " in the argument `seq_id`\nTo define sequence info for your dataset,
                see the function *generate_sequence_data*\n"))
  }
  
  # -- Join
  
  # join results to ground truths 
  results <- merge(preds, data, by.x = "filename", by.y = filepath, all.x = TRUE, all.y = FALSE)
  
  # flag any predictions that do not have associated annotations
  results[, true_class] <- ifelse(is.na(results[, true_class]), 
                                  "Annotation_not_found",
                                  results[, true_class])
  
  # print("Removing predictions without associated annotations from evaluation calculations.\n")
  
  # filter out predictions without labels
  evals <- results[results[,true_class] != "Annotation_not_found",]
  
  # update name for ground truth
  evals$true_class <- evals[, true_class]
  
  # calculate overall eval metrics
  evals$TP <- ifelse(evals$prediction == evals$true_class, 1, 0)
  evals$FP <- ifelse((evals$prediction != "Empty" & evals$true_class == "Empty"), 1, 0)
  evals$FP <- ifelse((evals$prediction != evals$true_class & evals$prediction != "Empty"), 1, evals$FP)
  evals$FN <- ifelse((evals$prediction == "Empty" & evals$true_class != "Empty"), 1, 0)
  
  mAP <- sum(evals$TP) / sum(sum(evals$TP) + sum(evals$FP))
  mAR <- sum(evals$TP) / sum(sum(evals$TP) + sum(evals$FN))
  F1 <- 2 * ((mAP * mAR) / (mAP + mAR))
  
  overall_metrics <- data.frame("mAP" = mAP, "mAR" = mAR, "F1_score" = F1)
  
  # -- Calculate image-level id eval metrics  
  
  if("image" %in% event_level) {
    
    # evaluate model at the image level
    image_results <- classwise_metrics(evals)
    
  }
  
  # -- Calculate image-level id+count eval metrics
  
  if("image" %in% event_level & assess_counts) {
    
    # function to perform evals on counts
    
    
    
  }
  
  # -- Calculate sequence-level id eval metrics
  
  if("sequence" %in% event_level) {
    # make sure confidence score is included
    if(!("confidence_in_pred" %in% colnames(evals))){
      stop(paste0("Cannot find column with confidence scores in ", deparse(substitute(preds)), ". 
                \nPlease ensure this column is named 'confidence_in_pred' .\n"))
    }
    
    # rename seq_id variable for ease of use
    evals$seq_id <- evals[, seq_id]
    
    # group df by sequence id and predicted class
    seq_evals <- dplyr::group_by(evals, seq_id, prediction)
    
    # Keep highest confidence class-wise predictions for each sequence
    filtrd_evals <- dplyr::filter(seq_evals, confidence_in_pred == max(confidence_in_pred))
    
    # create df with sequences that have only Empty predictions; keep just first prediction of each sequence
    empt_evals <- dplyr::filter(filtrd_evals, all(prediction == "Empty"))
    empt_seqs <- dplyr::slice(empt_evals, 1)
    
    # remove all empty predictions non-empty sequences
    nonempt_seqs <- dplyr::filter(filtrd_evals, prediction != "Empty")
    
    # merge the two dfs
    seq_df <- rbind(empt_seqs, nonempt_seqs)
    
    # ungroup the df and sort by seq_id
    seq_df <- dplyr::ungroup(seq_df)
    seq_df <- dplyr::arrange(seq_df, seq_id)
    
    # evaluate model at the sequence level
    seq_results <- classwise_metrics(seq_df)
    
  }
  
  # -- Calculate sequence-level id+count eval metrics
  

  
  
  # -- Wrap all outputs in a list
  
  eval_obj <- list("overall_metrics" = overall_metrics)
  
  if("image" %in% event_level){
    eval_obj <- append(eval_obj, list("image_metrics" = image_results))
  }
  if("sequence" %in% event_level){
    eval_obj <- append(eval_obj, list("sequence_metrics" = seq_results))
  }
  
  # -- Return final object
  
  return(eval_obj)
}
  