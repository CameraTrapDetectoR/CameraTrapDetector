#' Get List of Possible Species
#' 
#' @description Evaluate user-provided latitude / longitude values with range
#' extent data for species included in the trained models. Helper function for
#' `deploy_model`
#' 
#' @param location dataframe containing a single longitude and latitude value
#' @param extent.data auxiliary location database
#' @param model_type model type or version to pull class dict labels
#' 
#' @returns possible class label list
#' 
#' @import dplyr
#' @export


get_possible_species <- function(location, extent.data, model_type){
  
  #--Test which species to consider
  location.test <- vector()
  
  for(i in 1:nrow(extent.data)){
    location.test[i]<-location_contained_in_extent(location, extent.data[i,])
  }#END Loop
  
  possible.labels <- extent.data[location.test==TRUE,]
  
  if(grepl("general", model_type, fixed=TRUE)){
    possible.labels <- possible.labels[possible.labels$model_type == "general",]
  }
  if(grepl("family", model_type, fixed=TRUE)){
    possible.labels <- possible.labels[possible.labels$model_type == "family",]
  }
  if(grepl("species", model_type, fixed=TRUE)){
    possible.labels <- possible.labels[possible.labels$model_type == "species",]
  }
  if(grepl("pig", model_type, fixed=TRUE)){
    possible.labels <- possible.labels[possible.labels$label == "Wild_Pig",]
    possible.labels$model_type <- "pig_only"
    # create a generic 'not_pig' row
    possible.labels[2, ] <- data.frame(taxa = 'species',
                                       label = 'Not_Pig',
                                       model_type = 'pig_only',
                                       xmin = -180, ymin = -90,
                                       xmax = 180, ymax = 90)
  }
  
  #--Generate unique set of species 
  possible.labels <- unique(possible.labels[,c("taxa","label","model_type")])
  
  possible.labels <- possible.labels[order(possible.labels$taxa, possible.labels$label),]
  
  # standardize label format
  possible.labels <- dplyr::mutate(possible.labels,
                                 label = gsub("'", "", label),
                                 label = gsub(" ", "_", label),
                                 label = gsub("-", "_", label),
                                 label = tools::toTitleCase(label))
  
  return(possible.labels)
}#END


#' Determine if location falls within a species range
#' 
#' @description Evaluate if a location (latitude / longitude) is within a geographic 
#' extent. Helper function for `get_possible_species`
#' 
#' @param location dataframe containing a single longitude and latitude value
#' @param extent.values dataframe containing xmin, xmax, ymin, ymax representing an extent (bounding box)
#'
#' @returns location extent helper function
#' 
#' @export
#' 
location_contained_in_extent<-function(location, extent.values){
  
  return((location$longitude > extent.values$xmin &
            location$longitude < extent.values$xmax &
            location$latitude > extent.values$ymin &
            location$latitude < extent.values$ymax))
}
