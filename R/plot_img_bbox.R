#' Plot image with bounding box predictions
#' 
#' @description Create a copy of the original image with predicted bounding box 
#' and, optionally, the predicted category plotted on the image copy. Helper
#' function for `deploy_model`
#' 
#' @param filename The file containing the image
#' @param plot_df Prediction dataframe that is output from deployment
#' @param arg_list argument list to pull all the other arguments
#' 
#' @returns png image file in output_dir with bboxes and labels plotted
#' 
#' @import magick
#' 
#' @export
#' 
plot_img_bbox<- function(filename, plot_df, arg_list){
  
  # read in image and resize to user specs
  filename_full <- dplyr::if_else(fs::file_exists(filename), filename, 
                                  fs::path(arg_list$data_dir, filename))
  img <- magick::image_read(filename_full)
  img <- magick::image_scale(img, paste0(arg_list$w, 'x', arg_list$h, '!'))
  
  # strip filename and replace recursive dir structure with underscores
  file_strip <- stringr::str_remove(filename_full, arg_list$data_dir)
  file_strip <- sub("^[^[:alnum:]]+",'', stringr::str_replace(file_strip, "/", "_"))
  
  # substitue output dir for data dir in new filename
  output_nm <- fs::path(paste(arg_list$output_dir, file_strip, sep = "/"))
  
  # replace file extension with png 
  output_nm <- stringr::str_replace(output_nm, stringr::str_split_i(output_nm, "\\.", -1), "png")
  
  # inverse y-values of bboxes for R plotting
  plot_df <- dplyr::mutate(plot_df, dplyr::across(c(YMin, YMax), ~ 1 - .))

  # rescale bounding box
  plot_df <- dplyr::mutate(plot_df, dplyr::across(c(XMin, XMax), ~.*arg_list$w))
  plot_df <- dplyr::mutate(plot_df, dplyr::across(c(YMin, YMax), ~.*arg_list$h))
  
  # make plot
  grDevices::png(output_nm, width=arg_list$w, height=arg_list$h)
  plot(img)
  if (nrow(plot_df) > 0){ # Only plot boxes if there are predictions
    for(i in 1:nrow(plot_df)){
      graphics::segments(x0=plot_df$XMin[i], y0=plot_df$YMin[i],
                         x1=plot_df$XMin[i], y1=plot_df$YMax[i], 
                         col=arg_list$col, lty=arg_list$lty, lwd=arg_list$lwd)
      graphics::segments(x0=plot_df$XMin[i], y0=plot_df$YMin[i],
                         x1=plot_df$XMax[i], y1=plot_df$YMin[i], 
                         col=arg_list$col, lty=arg_list$lty, lwd=arg_list$lwd)
      graphics::segments(x0=plot_df$XMin[i], y0=plot_df$YMax[i],
                         x1=plot_df$XMax[i], y1=plot_df$YMax[i], 
                         col=arg_list$col, lty=arg_list$lty, lwd=arg_list$lwd)
      graphics::segments(x0=plot_df$XMax[i], y0=plot_df$YMax[i],
                         x1=plot_df$XMax[i], y1=plot_df$YMin[i], 
                         col=arg_list$col, lty=arg_list$lty, lwd=arg_list$lwd)
      graphics::text(x= plot_df$XMin[i]+6, y=plot_df$YMax[i]-10, 
                     paste0(plot_df$prediction[i], " = ", round(plot_df$confidence_score[i], 2)),
                     col=arg_list$col, adj=0, cex = 1.6)
    }
  }
  
  grDevices::dev.off()
  
}
