% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/box_iou.R
\name{box_iou}
\alias{box_iou}
\title{Box IoU}
\usage{
box_iou(a, b)
}
\arguments{
\item{a}{Bounding box that will be compared with b}

\item{b}{Second bounding box to compare with a}
}
\value{
overlap area of `a` and `b`
}
\description{
Calculates the intersection over union (IoU) two bounding boxes. 
Run when the user specifies an overlap correction; helper function for `find_unique_sets`
}
