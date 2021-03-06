% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Taxonomic_utilities.R
\name{Commontaxer}
\alias{Commontaxer}
\title{Detect common taxonomic names across all source datasets}
\usage{
Commontaxer(Source_taxa_key, Taxa_level, Size_class)
}
\arguments{
\item{Source_taxa_key}{A dataframe with columns named Source, Lifestage, SizeClass, and the value provided to the parameter \code{Taxa_level}. This dataframe should list all \code{Taxa_level} by \code{Lifestage} combinations present for each source dataset. You can provide it with the output of \code{\link{SourceTaxaKeyer}}.}

\item{Taxa_level}{Taxonomic level you would like to perform this calculation for. E.g., if you wish to determine all Genus x lifestage combinations present in all datasets, provide \code{Taxa_level = "Genus"}. The value provided here must be the name of a column in the dataset provided to \code{Source_taxa_key}.}

\item{Size_class}{The size class(es) you would like this function to consider. You should generally only supply 1 size class.}
}
\value{
A tibble with a column for \code{Taxa_level} and another for \code{Lifestage} representing all combinations of these values present in all source datasets.
}
\description{
Calculates taxa by life stage combos present in all source datasets
}
\details{
This function is designed to work on just one size class. To apply to multiple size classes, use \link[purrr]{map} or \link[base]{apply} functions to apply across size classes.
}
\examples{
\dontrun{
library(rlang)
library(purrr)
SourceTaxaKey <- SourceTaxaKeyer(zoopComb, crosswalk)
Size_classes <- set_names(c("Micro", "Meso", "Macro"))
Commontax <- map(Size_classes, ~ Commontaxer(SourceTaxaKey, "Taxname", .))
}
}
\seealso{
\code{\link{Zoopsynther}}, \code{\link{crosswalk}}, \code{\link{SourceTaxaKeyer}}
}
\author{
Sam Bashevkin
}
\keyword{Taxonomy}
\keyword{zooplankton}
