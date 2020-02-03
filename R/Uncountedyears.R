#' Detect dates when a species was not counted
#'
#' Detects years when a species was present in the system (i.e., post-invasion for invasive species) but not counted in each zooplankton survey
#'
#' @param Source String with the name of the source dataset (e.g., \code{Source="EMP"}).
#' @param Size_class String with the name of the desired zooplankton size class (e.g., \code{Source="Meso"}).
#' @param Crosswalk Crosswalk table like \code{\link{crosswalk}} or another table in the same format.
#' @param Start_year First year the \code{Source} survey started sampling zooplankton.
#' @param Intro_lag Number of years buffer after a species is introduced when we expect surveys to start recording them. Effectively adds \code{Intro_lag} years to the introduction year of each species.
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @details This function is designed to work on one source and size class at a time. To apply across multiple IDs, use the \link[purrr]{map} or \link[base]{apply} functions.
#' @return A tibble with columns for the Taxlifestage, Taxname, Lifestage, Source, Sizeclass, and then a list-column of years in which that particular taxon was not counted in the specified study and size class. Taxa that were counted in all applicable years are not included in the tibble.
#' @author Sam Bashevkin
#' @examples
#' require(purrr)
#' require(dplyr)
#' require(lubridate)
#'
#' datasets<-zooper::zoopComb%>%
#'  mutate(names=paste(Source, SizeClass, sep="_"))%>%
#'  select(names, Source, SizeClass)%>%
#'  filter(Source%in%c("EMP", "FMWT", "twentymm"))%>%
#'  distinct()
#'
#' BadYears<-map2_dfr(datasets$Source, datasets$SizeClass, ~ Uncountedyears(Source = .x,
#' Size_class = .y,
#' Crosswalk = zooper::crosswalk,
#' Start_year = zooper::startDates%>%
#'  filter(Source==.x & SizeClass==.y)%>%
#'  pull(Startdate)%>%
#'  year(),
#'  Intro_lag=2))
#' @seealso \code{\link{Zoopsynther}}, \code{\link{crosswalk}}, \code{\link{startDates}}
#' @export

Uncountedyears<- function(Source, Size_class, Crosswalk, Start_year, Intro_lag){

  # Convert inputs into rlang symbols

  start<-rlang::sym(paste0(Source, "start"))
  start<-rlang::enquo(start)
  end<-rlang::sym(paste0(Source, "end"))
  end<-rlang::enquo(end)
  dataset<-rlang::sym(paste(Source, Size_class, sep="_"))
  dataset<-rlang::enquo(dataset)

  start2<-rlang::sym(paste0(Source, "start2"))
  start2<-rlang::enquo(start2)



  out<-Crosswalk%>%
    dplyr::filter(!is.na(!!dataset))%>% #Filter to source and size class of choice
    dplyr::mutate(!!start := dplyr::if_else(!is.finite(!!start), Sys.Date(), !!start))%>% #Change infinite start dates to current year
    dplyr::mutate(Intro = .data$Intro + lubridate::years(Intro_lag))%>% # Add Intro_lag to species introduction year
    dplyr::mutate(Intro = dplyr::if_else(!is.finite(.data$Intro) | .data$Intro > !!start, !!start, .data$Intro), # If introduction year is infinite (native species) or greater than the start date for that taxa, set it equal to the start date
                  !!end := dplyr::if_else(!is.finite(!!end), Sys.Date(), !!end))%>% # If end date is infinite, set it to current year
    dplyr::mutate(Taxlifestage=paste(.data$Taxname, .data$Lifestage))%>%
    {if(rlang::quo_name(start2)%in%names(Crosswalk)){ # If source has a set of second start dates (currently just 20mm)
      dplyr::select(., .data$Taxname, .data$Lifestage, .data$Taxlifestage, !!start, !!start2, !!end, .data$Intro, !!dataset)%>% # select desired columns
        dplyr::group_by(.data$Taxname, .data$Lifestage, !!dataset, .data$Taxlifestage, !!start, !!start2, !!end, .data$Intro, n = dplyr::row_number())%>% #group data by each row to apply operation to each row, and by all columns we want to retain
        dplyr::do(tibble::tibble(Years = list(c(
          as.integer(seq(lubridate::year(.$Intro), lubridate::year(.[[rlang::quo_name(start)]]), by = 1)), # Sequence of years from when taxon was introduced to when it was first counted in this particular source and size class
          as.integer(seq(lubridate::year(.[[rlang::quo_name(end)]]),
                         dplyr::if_else(is.finite(.[[rlang::quo_name(start2)]]), lubridate::year(.[[rlang::quo_name(start2)]])-1, lubridate::year(Sys.Date())), by=1)))))) # Sequence of years from when taxon was last counted to the current year or when counting was restarted
    } else{
      dplyr::select(., .data$Taxname, .data$Lifestage, .data$Taxlifestage, !!start, !!end, .data$Intro, !!dataset)%>% # Same as above but for normal sources that never restarted counting
        dplyr::group_by(.data$Taxname, .data$Lifestage, !!dataset, .data$Taxlifestage, !!start, !!end, .data$Intro, n = dplyr::row_number())%>%
        dplyr::do(tibble::tibble(Years = list(c(
          as.integer(seq(lubridate::year(.$Intro), lubridate::year(.[[rlang::quo_name(start)]]), by = 1)),
          as.integer(seq(lubridate::year(.[[rlang::quo_name(end)]]), lubridate::year(Sys.Date()), by=1))))))
    }}%>%
    dplyr::ungroup()%>%
    dplyr::select(-.data$n)%>% # Remove row number identifier
    tidyr::unnest(cols=c(.data$Years))%>% #unnest list-column to expand dataframe and make it easier to manipulate the years
    dplyr::mutate(Years=dplyr::if_else(.data$Years<Start_year | .data$Years==lubridate::year(Sys.Date()) | .data$Years==lubridate::year(!!start), NA_integer_, .data$Years))%>% #Remove years before survey was started and years that correspond to the current year and the first year a taxa was counted (fixing problem with sequence creation). Turning these into NAs is needed to preserve entries that would have been deleted so they can be compared with other entries that are equivalent taxa
    dplyr::group_by(.data$Taxlifestage, .data$Taxname, .data$Lifestage, !!dataset)%>%
    dplyr::summarise(Years=list(unique(.data$Years)))%>% #Convert back to list-columns of years for each taxlifestage
    dplyr::group_by(.data$Taxlifestage, .data$Taxname, .data$Lifestage)%>%
    dplyr::summarise(Years=list(Reduce(intersect, .data$Years)))%>% #In some cases, a survey stopped counting a taxa but an equivalent taxname was still counted. This finds all years when none of those equivalents were counted
    dplyr::ungroup()%>%
    tidyr::unnest(cols=c(.data$Years))%>% #Unnest again for manipulating years
    dplyr::filter(!is.na(.data$Years))%>% #Remove any NA years
    dplyr::mutate(Source=Source,
                  SizeClass=Size_class)%>% # Add identifiers for survey and size class
    dplyr::group_by(.data$Taxlifestage, .data$Taxname, .data$Lifestage, .data$Source, .data$SizeClass)%>%
    dplyr::summarise(Years=list(unique(.data$Years))) #Convert back to list
  return(out)
}
