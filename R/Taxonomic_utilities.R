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
    dplyr::filter(dplyr::if_any(c("Phylum", "Class", "Order", "Family", "Genus", "Species", "Taxname"), ~.x%in%Taxa))%>%
    dplyr::select("Taxname")%>%
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
#' SourceTaxaKey <- SourceTaxaKeyer(Data = dplyr::filter(zoopComb, Source!="YBFMP"),
#' Crosswalk = crosswalk)
#' @seealso \code{\link{Zoopsynther}}, \code{\link{crosswalk}}, \code{\link{zoopComb}}
#' @export


SourceTaxaKeyer<-function(Data, Crosswalk){

  #Function that lists all unique taxa x life stage combinations for a given source
  SourceTaxaLister<-function(Source, Crosswalk){
    Source2<-rlang::sym(Source) #unquote input
    Source2<-rlang::enquo(Source2) #capture expression to pass on to functions below
    Crosswalk%>%
      dplyr::filter(!is.na(!!Source2))%>%
      dplyr::select("Phylum", "Class", "Order", "Family", "Genus", "Taxname", "Lifestage")%>%
      dplyr::distinct()%>%
      dplyr::mutate(Source=Source)
  }

  #Find all combinations of source and sizeclass present in the zooplankton dataset
  Sources<-unique(paste(dplyr::recode(Data$Source, "20mm"= "twentymm"), Data$SizeClass, sep="_"))

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
#' \dontrun{
#' library(rlang)
#' library(purrr)
#' SourceTaxaKey <- SourceTaxaKeyer(zoopComb, crosswalk)
#' Size_classes <- set_names(c("Micro", "Meso", "Macro"))
#' Commontax <- map(Size_classes, ~ Commontaxer(SourceTaxaKey, "Taxname", .))
#' }
#' @seealso \code{\link{Zoopsynther}}, \code{\link{crosswalk}}, \code{\link{SourceTaxaKeyer}}
#' @export
#'

Commontaxer<-function(Source_taxa_key, Taxa_level, Size_class){
  Source_taxa_key<-Source_taxa_key%>%
    dplyr::filter(.data$SizeClass%in%Size_class)
  Taxa_level<-rlang::sym(Taxa_level) #unquote input
  Taxa_level<-rlang::enquo(Taxa_level) #capture expression to pass on to functions below
  N<-Source_taxa_key%>%
    dplyr::pull("Source")%>%
    unique()%>%
    length()
  Source_taxa_key%>%
    dplyr::filter(!is.na(!!Taxa_level))%>%
    dplyr::select(!!Taxa_level, "Lifestage", "Source")%>%
    dplyr::distinct()%>%
    dplyr::group_by(!!Taxa_level, .data$Lifestage)%>%
    dplyr::summarise(n=dplyr::n(), .groups="drop")%>% #Create index of number of data sources in which each Taxa_level x lifestage combo appears
    dplyr::filter(.data$n==N)%>% #only retain Taxa_level x lifestage combos that appear in all datasets
    dplyr::select(!!Taxa_level, "Lifestage")
}


#' Apply LCD approach for "Taxa" option
#'
#' Sums to least common denominator taxa, one taxonomic level at a time
#'
#' @param Data Zooplankton dataset including columns named the same as the \code{Groupers}, a \code{Taxname} column, "CPUE", and no other taxonomic identifying columns.
#' @param Taxalevel The value of Groupers on which to apply this function.
#' @param Groupers A character vector of names of additional taxonomic levels to be removed in this step. This vector can include \code{Taxalevel} and, if so, it will be removed from the vector within the function so \code{Taxalevel} is preserved.
#' @keywords Taxonomy zooplankton
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @details This function is designed to work on just one Taxalevel at a time. To apply to multiple Taxalevels, use \link[purrr]{map} or \link[base]{apply} functions to apply across taxonomic levels.
#' @return A tibble with sums calculated for each unique value in \code{Data$Taxalevel}. Sums will be excluded for grouping taxa that only contain 1 unique Taxname.
#' @author Sam Bashevkin
#' @examples
#' \dontrun{
#' library(dplyr)
#' df <- zoopComb%>%
#'   mutate(dplyr::across(tidyselect::all_of(c("Genus", "Family", "Order", "Class", "Phylum")),
#'                    list(g=~if_else(.%in%completeTaxaList, ., NA_character_))))%>%
#'   select(-Phylum, -Class, -Order, -Family, -Genus, -Species, -Taxlifestage)
#' family_sums <- LCD_Taxa(df, "Family_g")
#' }
#'
#' @seealso \code{\link{Zoopsynther}}, \code{\link{crosswalk}}, \code{\link{zoopComb}}
#' @export

LCD_Taxa<-function(Data, Taxalevel, Groupers = c("Genus_g", "Family_g", "Order_g", "Class_g", "Phylum_g")){
  Taxalevel2<-rlang::sym(Taxalevel) #unquote input
  Taxalevel2<-rlang::enquo(Taxalevel2) #capture expression to pass on to functions below
  Groupers <- Groupers[Groupers!=Taxalevel]
  out<-Data%>%
    dplyr::filter(!is.na(!!Taxalevel2))%>% #filter to include only data belonging to the taxonomic grouping
    dplyr::group_by(!!Taxalevel2)%>%
    dplyr::mutate(N=length(unique(.data$Taxname)))%>%
    dplyr::filter(.data$N>1)%>% # No need to sum up categories if there is only 1 taxa in the category
    dplyr::ungroup()%>%
    dplyr::select(-tidyselect::all_of(c("N", "Taxname", Groupers)))%>%
    dtplyr::lazy_dt()%>%
    dplyr::group_by(dplyr::across(-"CPUE"))%>% #Group data by relavent grouping variables (including taxonomic group) for later data summation
    dplyr::summarise(CPUE=sum(.data$CPUE, na.rm=TRUE))%>% #Add up all members of each grouping taxon
    dplyr::ungroup()%>%
    tibble::as_tibble()%>%
    dplyr::mutate(Taxname=!!Taxalevel2) #Add summarized group names to Taxname
  return(out)
}


#' Find all unique values within a tibble
#'
#' Outputs a vector with all unique non-NA values in a subset of selected columns from a tibble
#'
#' @param Data Data frame containing at least the columns specified in \code{Reduced_vars}.
#' @param Reduced_vars Columns from which you would like to select all unique values. Must be quoted names of columns present in \code{Data}.
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @return A vector of unique non-NA values from the \code{Reduced_vars} columns of \code{Data}.
#' @author Sam Bashevkin
#' @keywords internal
#' @examples
#' #Find all unique taxonomic names in the crosswalk table.
#' \dontrun{
#' All_taxa <- Datareducer(crosswalk, c("Phylum", "Class", "Order", "Family", "Genus", "Species"))
#'}
#' @seealso \code{\link{Zoopsynther}}, \code{\link{crosswalk}}, \code{\link{zoopComb}}

Datareducer<-function(Data, Reduced_vars){
  out<-Data%>%
    dplyr::select(tidyselect::all_of(Reduced_vars))%>%
    dplyr::distinct()%>%
    tidyr::pivot_longer(cols=tidyselect::all_of(Reduced_vars), names_to = "Level", values_to = "Taxa")%>%
    tidyr::drop_na()%>%
    dplyr::pull("Taxa")%>%
    unique()
  return(out)
}

#' Remove taxa from a list of taxlifestages
#'
#' Given 2 lists of character vectors and an ID corresponding to the name of an element in each list, this function will, for the named element, remove values in one character vector that correspond to values in the other and output a concatenated string of all remaining values.
#'
#' @param ID The quoted name of an element in both \code{Taxlifestage_list} and \code{Remove_taxa}
#' @param Taxlifestage_list The list of character vectors of Taxlifestages from which elements will be removed.
#' @param Remove_taxa The list of character vectors of Taxa names containing values to be removed from \code{Taxlifestage_list}.
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @details This function is designed to work on one ID at a time. To apply across multiple IDs, use the \link[purrr]{map} or \link[base]{apply} functions.
#' @return A comma-separated string of remaining Taxlifestages from \code{Taxlifestage_list[[ID]]}
#' @author Sam Bashevkin
#' @keywords internal
#' @examples
#' \dontrun{
#' Taxlifestage_list <- list(EMP = paste(crosswalk$EMP_Meso[!is.na(crosswalk$EMP_Meso)],
#'                           crosswalk$Lifestage[!is.na(crosswalk$EMP_Meso)]))
#' Remove_taxa <- list(EMP = crosswalk$EMP_Micro[!is.na(crosswalk$EMP_Micro)])
#' Meso_not_Micro <- Taxaremover("EMP", Taxlifestage_list, Remove_taxa)
#' }
#' @seealso \code{\link{Zoopsynther}}, \code{\link{crosswalk}}

Taxaremover<-function(ID, Taxlifestage_list, Remove_taxa){
  Taxlifestage_list<-Taxlifestage_list[[ID]]
  Remove_taxa<-Remove_taxa[[ID]]
  Remove<-stringr::str_which(paste0("[", paste(Remove_taxa, collapse="|"), "]"), stringr::word(Taxlifestage_list, 1, -2))
  if(length(Remove)>0) {Taxlifestage_list<-Taxlifestage_list[-Remove]}
  out<-paste(sort(Taxlifestage_list), collapse=", ")
  return(out)
}
