#' CameraTrapDetectoR  data
#' 
#' species.extent.data
#' 
#' Location polygons detailing range data for each species across taxonomic level in the models
#' 
#' @format ##`species.extent.data`
#' a data frame with 120397 rows and 7 columns
#' \describe{
#'  \item{taxa}{taxonomic level}
#'  \item{label}{label name}
#'  \item{model_type}{model type}
#'  \item{xmin}{xmin of polygon}
#'  \item{ymin}{ymin of polygon}
#'  \item{xmax}{xmax of polygon}
#'  \item{ymax}{ymax of polygon}
#'}
"species.extent.data"
#' 
#' preds
#' 
#' example_set image predictions from the "species_v2" model
#' 
#' @format ## `preds`
#' A data frame with 33 rows and 17 columns:
#' \describe{
#' \item{\code{filename}}{path to image file}
#' \item{\code{prediction}}{model class prediction, given in species common name}
#' \item{\code{confidence_in_pred}}{model confidence score in the prediction}
#' \item{\code{count}}{individual prediction count per image}
#' \item{\code{certainty}}{categorical measure of certainty in prediction}
#' }
"preds"
#'
#'
#' labels
#'
#' example_set ground truth classification by species
#'
#' @format ## `labels`
#' A data frame with 31 rows and 3 columns
#' \describe{
#' \item{\code{filename}}{path to image file}
#' \item{\code{true_class}}{ground truth in taxonomic class common name}
#' \item{\code{count}}{number of animals per \code{true_class} per image}
#' }
"labels"
#'
#'
#' models
#'
#' list of all models available in the package
#'
#' @format ## `models`
#' A data frame with 5 rows and 5 columns
#' \describe{
#' \item{\code{model_name}}{model and version name}
#' \item{\code{model_type}}{model group: general, family, species, or pig_only}
#' \item{\code{num_classes}}{number of class labels in that model (including background)}
#' \item{\code{description}}{string description of model (under development)}
#' \item{\code{URL}}{download link for model files}
#' }
"models"
