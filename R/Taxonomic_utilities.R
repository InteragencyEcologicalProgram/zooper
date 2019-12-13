#' Finds all of the lowest-level (i.e. counted) taxonomic names within a vector of taxa
#'
#' Helps filter the zooplankton dataset by returning a set of lowest-level taxa (i.e. the level taxa were recorded at when counted in plankton samples) within a vector of taxa (which can include taxa from any taxonomic level).
#'
#' @param Crosswalk Crosswalk table (such as \code{\link{crosswalk}}) with columns named "Phylum", "Class", "Order", "Family", "Genus", "Species", and "Taxname." "Taxname" corresponds to the full scientific name of the taxonomic level assigned to the plankter when recorded in the dataset.
#' @param Taxa A character vector of taxa you wish to select. These taxa can be from any taxonomic level present in the list above. If using the built-in data and crosswalk, they should be present in the \code{\link{completeTaxaList}}.
#' @keywords Taxonomy zooplankton.
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @return A character vector of scientific names contained within the vector of \code{Taxa} provided.
#' @author Sam Bashevkin
#' @examples
#' Taxnames <- Taxnamefinder(crosswalk, c("Calanoida", "Cyclopoida"))
#' @seealso \code{\link{completeTaxaList}}, \code{\link{Zoopsynther}}
#' @export
#'

Taxnamefinder <- function(Crosswalk, Taxa){
  Taxnames<-Crosswalk%>%
    dplyr::filter_at(dplyr::vars(.data$Phylum, .data$Class, .data$Order, .data$Family, .data$Genus, .data$Species, .data$Taxname), dplyr::any_vars(.%in%Taxa))%>%
    dplyr::select(.data$Taxname)%>%
    dplyr::distinct()%>%
    dplyr::pull()

  return(Taxnames)
}

#' Unique taxa by lifestage combinations present in each source and size class
#'
#' Computes a dataframe with all unique taxa by lifestage combinations present in each source and size class
#'
#' @param Data Zooplankton dataset. Must have a column named \code{Source} with the names of the source datasets and a column named \code{SizeClass} with the names of the zooplankton size classes.
#' @param Crosswalk Crosswalk table (e.g., \code{\link{crosswalk}}) with columns named "Phylum", "Class", "Order", "Family", "Genus", "Taxname", "Lifestage", and column names corresponding to each unique value of \code{paste(data$Source, data$SizeClass, sep="_")}.
#' @keywords Taxonomy zooplankton
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @return a tibble with the complete taxonomic information for each combination of source and size class.
#' @author Sam Bashevkin
#' @examples
#' SourceTaxaKey <- SourceTaxaKeyer(Data = zoopComb, Crosswalk = crosswalk)
#' @seealso \code{\link{Zoopsynther}}, \code{\link{crosswalk}}, \code{\link{zoopComb}}
#' @export


SourceTaxaKeyer<-function(Data, Crosswalk){

  #Function that lists all unique taxa x life stage combinations for a given source
  SourceTaxaLister<-function(Source, Crosswalk){
    Source2<-rlang::sym(Source) #unquote input
    Source2<-rlang::enquo(Source2) #capture expression to pass on to functions below
    Crosswalk%>%
      dplyr::filter(!is.na(!!Source2))%>%
      dplyr::select(.data$Phylum, .data$Class, .data$Order, .data$Family, .data$Genus, .data$Taxname, .data$Lifestage)%>%
      dplyr::distinct()%>%
      dplyr::mutate(Source=Source)
  }

  #Find all combinations of source and sizeclass present in the zooplankton dataset
  Sources<-unique(paste(Data$Source, Data$SizeClass, sep="_"))

  #apply above function across all unique combos of source and size class
  SourceTaxaKey<-purrr::map_dfr(Sources, SourceTaxaLister, Crosswalk)%>%
    tidyr::separate(.data$Source, into=c("Source", "SizeClass"), sep="_")

  return(SourceTaxaKey)

}


#' Detect common taxonomic names across all source datasets
#'
#' Calculates taxa by life stage combos present in all source datasets
#'
#' @param Source_taxa_key A dataframe with columns named Source, Lifestage, SizeClass, and the value provided to the parameter \code{Taxa_level}. This dataframe should list all \code{Taxa_level} by \code{Lifestage} combinations present for each source dataset. You can provide it with the output of \code{\link{SourceTaxaKeyer}}.
#' @param Taxa_level Taxonomic level you would like to perform this calculation for. E.g., if you wish to determine all Genus x lifestage combinations present in all datasets, provide \code{Taxa_level = "Genus"}. The value provided here must be the name of a column in the dataset provided to \code{Source_taxa_key}.
#' @param Size_class The size class(es) you would like this function to consider. You should generally only supply 1 size class.
#' @keywords Taxonomy zooplankton
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @details This function is designed to work on just one size class. To apply to multiple size classes, use \link[purrr]{map} or \link[base]{apply} functions to apply across size classes.
#' @return A tibble with a column for \code{Taxa_level} and another for \code{Lifestage} representing all combinations of these values present in all source datasets.
#' @author Sam Bashevkin
#' @examples
#' SourceTaxaKey <- SourceTaxaKeyer(zoopComb, crosswalk)
#' Size_classes <- rlang::set_names(c("Micro", "Meso", "Macro"))
#' Commontax <- purrr::map(Size_classes, ~ Commontaxer(SourceTaxaKey, "Taxname", .))
#' @seealso \code{\link{Zoopsynther}}, \code{\link{crosswalk}}, \code{\link{SourceTaxaKeyer}}
#' @export
#'

Commontaxer<-function(Source_taxa_key, Taxa_level, Size_class){
  Source_taxa_key<-Source_taxa_key%>%
    dplyr::filter(.data$SizeClass%in%Size_class)
  Taxa_level<-rlang::sym(Taxa_level) #unquote input
  Taxa_level<-rlang::enquo(Taxa_level) #capture expression to pass on to functions below
  N<-Source_taxa_key%>%
    dplyr::pull(.data$Source)%>%
    unique()%>%
    length()
  Source_taxa_key%>%
    dplyr::filter(!is.na(!!Taxa_level))%>%
    dplyr::select(!!Taxa_level, .data$Lifestage, .data$Source)%>%
    dplyr::distinct()%>%
    dplyr::group_by(!!Taxa_level, .data$Lifestage)%>%
    dplyr::summarise(n=dplyr::n())%>% #Create index of number of data sources in which each Taxa_level x lifestage combo appears
    dplyr::ungroup()%>%
    dplyr::filter(.data$n==N)%>% #only retain Taxa_level x lifestage combos that appear in all datasets
    dplyr::select(!!Taxa_level, .data$Lifestage)
}
