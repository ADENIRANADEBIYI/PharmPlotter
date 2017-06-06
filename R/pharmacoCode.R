# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

#' Function to plot the AUC consistency between the different projects
#'
#' \code{plotAUCConsistency} uses ggplot2 functions to plot a scatterplot of
#' AUC measurements in the CCLE project as a function of the AUC measurements of
#' the GDSC project. Based on this scatterplot, one can evaluate the consistency of
#' these measurements. If the data points are close to the f(x) = x line, it
#' would mean that the measurements are consistent between the different projects.
#' @param data A data frame of summarized pharmacogenomic data with column names
#' "cellLine", "drug", "ic50_CCLE", "auc_CCLE","ic50_GDSC", "auc_GDSC".
#' @param drugName A character vector containing the name of the drug to evaluate.
#' This value has to be contain in the "drug" column from the "data" data frame.
#' @return A ggplot2 object
#' @import ggplot2
#' @examples
#' library(PharmPlotter)
#' data("summarizedData", package="PharmPlotter")
#' pl <- plotAUCConsistency( summarizedData, "AZD0530" )
#' @export
#'
plotAUCConsistency <- function( data, drugName ){
  colsNeeded <- c( "cellLine", "drug", "ic50_CCLE", "auc_CCLE","ic50_GDSC", "auc_GDSC" )
  stopifnot( all( colsNeeded %in% colnames( data ) ) )
  stopifnot( !drugName %in% data$drugName )
  pl <- ggplot( aes(x=auc_GDSC, y=auc_CCLE), data=subset(data, drug == drugName )) +
    geom_point() +
    xlab("GDSC AUC") +
    ylab("CCLE AUC")
  pl
}
