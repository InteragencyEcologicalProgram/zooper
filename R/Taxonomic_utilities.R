#' Finds all of the lowest-level (i.e. counted) taxonomic names within a vector of taxa
#'
#' Helps filter the zooplankton dataset by returning a set of lowest-level taxa (i.e. the level taxa were recorded at when counted in plankton samples) within a vector of taxa (wich can include taxa from any taxonomic level).
#'
#' @param Crosswalk Crosswalk table (such as \code{\link{crosswalk}}) with columns named "Phylum", "Class", "Order", "Family", "Genus", "Species", and "Taxname." "Taxname" corresponds to the full scientific name of the taxonomic level assigned to the plankter when recorded in the dataset.
#' @param Taxa A character vector of taxa you wish to select. These taxa can be from any taxonomic level present in the list above. If using the built-in data and crosswalk, they should be present in the \code{\link{completeTaxaList}}.
#' @keywords Taxonomy, zooplankton.
#' @importFrom magrittr %>%
#' @return A character vector of scientific names contained within the vector of \code{Taxa} provided.
#' @author Sam Bashevkin
#' @examples
#' Taxnames <- Taxnamefinder(crosswalk, c("Calanoida", "Cyclopoida"))
#' @seealso \code{\link{completeTaxaList}}, \code{\link{Zoopsynther}}\code{\link{completeTaxaList}}
#' @export
#'

Taxnamefinder <- function(Crosswalk, Taxa){
  Taxnames<-crosswalk%>%
    dplyr::filter_at(dplyr::vars(Phylum, Class, Order, Family, Genus, Species, Taxname), dplyr::any_vars(.%in%Taxa))%>%
    dplyr::select(Taxname)%>%
    dplyr::distinct()%>%
    dplyr::pull()

  return(Taxnames)
}
