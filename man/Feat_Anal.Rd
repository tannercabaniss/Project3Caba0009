% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Feat_Anal.R
\name{Feat_Anal}
\alias{Feat_Anal}
\title{Feature Analysis}
\usage{
Feat_Anal(features, scaling)
}
\arguments{
\item{features}{The features to include in the analysis.}

\item{scaling}{The scaling scheme to be applied to the data.}
}
\value{
a named list containing correlation, covariance matrices alongside other informative plots.
}
\description{
This function performs helpful feature analysis to aid in feature engineering for the RF model.
}
\examples{
Feat_Anal(features=c(TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,
TRUE,TRUE,TRUE,TRUE,TRUE,TRUE), scaling="None")
}
