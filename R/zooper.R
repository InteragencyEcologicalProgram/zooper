#' zooper: A package for integrating zooplankton datasets from the Sacramento San Joaquin Delta
#'
#' This package contains functions, lookup tables, and 2 built-in pre-combined datasets (one with the zooplankton data and another with the environmental data).
#'
#' @section zooper functions:
#' \itemize{
#'   \item \code{\link{Zoopsynther}}
#'   \item \code{\link{Zoopdownloader}}
#'   \item \code{\link{Taxnamefinder}}
#'   \item \code{\link{SourceTaxaKeyer}}
#'   \item \code{\link{LCD_Taxa}}
#'   \item \code{\link{Uncountedyears}}
#' }
#'
#' @section zooper lookup tables:
#' \itemize{
#'   \item \code{\link{crosswalk}}
#'   \item \code{\link{undersampled}}
#'   \item \code{\link{stations}}
#'   \item \code{\link{completeTaxaList}}
#'   \item \code{\link{startDates}}
#'   }
#'
#' @section zooper pre-combined (with the \code{\link{Zoopdownloader}} function) zooplankton datasets. These may be out of date:
#' \itemize{
#'   \item \code{\link{zoopComb}}
#'   \item \code{\link{zoopEnvComb}}
#' }
#'
#' @section Source datasets:
#' \describe{
#'   \item{Environmental Monitoring Program (EMP)}{The EMP zooplankton survey is run by the California Department
#'        of Fish and Wildlife. Zooplankton were first collected in 1972. It samples monthly at 17 fixed stations,
#'        2 floating entrapment zone stations, and 3 stations in Carquinez Strait and San Pablo Bay that are only
#'        sampled during high outflow and low salinity conditions. EMP samples using micro (43 \eqn{\mu}m),
#'        meso (160 \eqn{\mu}m), and macro (505 \eqn{\mu}m) zooplankton nets. Note that additional Amphipod data with
#'        quality issues (e.g., vegetation in net) are available in the EMP data publication.
#'        \href{https://portal.edirepository.org/nis/mapbrowse?scope=edi&identifier=522}{Data are available here.}}
#'   \item{20-mm Survey}{The 20-mm survey is run by the California Department of Fish and Wildlife.
#'        Zooplankton were first collected in 1995. Zooplankton are collected concurrently with fish
#'        samples at 41-55 fixed open-channel stations per year. Samples are collected twice per month
#'        between March and July. Only Mesozooplankton are collected with a 160 \eqn{\mu}m mesh net.
#'        \href{ftp://ftp.dfg.ca.gov/Delta\%20Smelt}{Data are available here.}}
#'   \item{Fall Midwater Trawl (FMWT) and Summer Townet Survey (STN)}{The FMWT and STN are run by the
#'        California Department of Fish and Wildlife. FMWT samples are collected monthly between September
#'        and December from a subset of the 122 fixed open-channel stations. STN samples are collected
#'        monthly between June and August from 40 fixed open-channel stations. Macrozooplankton have been
#'        collected since 2007 with a 505 \eqn{\mu}m mesh net while mesozooplankton have been collected
#'        since 2005 with a 160 \eqn{\mu}m mesh net.
#'        \href{ftp://ftp.dfg.ca.gov/TownetFallMidwaterTrawl/Zoopl_TownetFMWT}{Data are available here.}.
#'        Supplemental sampling from the Suisun Marsh Salinity Control Gate study data are also included
#'        and \href{ftp://ftp.dfg.ca.gov/TownetFallMidwaterTrawl/Zooplankton_SMSCG}{those data can be found here.}}
#'   \item{Fish Restoration Program (FRP)}{FRP is run by the California Department of Fish and Wildlife.
#'        Zooplankton were first collected in 2015. Samples are collected monthly between March and December
#'        in shallow-water habitats near marshes. FRP samples with meso (150 \eqn{\mu}m) and macro (500 \eqn{\mu}m)
#'        zooplankton nets. \href{doi.org/10.6073/pasta/86810e72766ad19fccb1b9dd3955bdf8}{Data are available here.}}
#'   \item{Directed Outflow Project Lower Trophic Study (DOP)}{The Directed Outflow Project Lower Trophic Study
#'        is run by ICF for the United States Bureau of Reclamation. Zooplankton were first collected in fall 2017.
#'        Samples were collected once every two weeks in 2017 and weekly thereafter. Sampling is conducted in the fall
#'        and, starting in 2019, spring and summer seasons have also been sampled. Three sampling stations per region
#'        are randomly selected for 5 regions (Suisun Bay, Suisun Marsh, Lower Sac. River, Cache Slough, Sac Ship Channel).
#'        In 2017, stations were sampled in 3 additional regions: West of the Benicia Bridge, Lower San Joaquin, and
#'        Upper Sac River. At each station, sample collection is attempted at both shoal (<=10 feet) and channel (>10 feet) habitat.
#'        Channels are sampled at the surface and, if deeper than 20 feet, also at the bottom 1/2 to 1/3 of the water column.
#'        DOP samples with meso (150 \eqn{\mu}m) and macro (500 \eqn{\mu}m) zooplankton nets.
#'        \href{https://doi.org/10.6073/pasta/73b4acc0e1a72cb31d95ba96a4071fbc}{Data are available here.}}
#'   \item{Yolo Bypass Fish Monitoring Program (YBFMP)}{YBFMP is run by the California Department of Water Resources.
#'        Zooplankton were first collected in 1999. Samples are from 2 sites, one in the Yolo Bypass and one in the Sacramento River.
#'        In 1999 and 2000, samples were collected for a couple of months each year, and then it increased to roughly winter/spring from 2001-2010.
#'        Since 2011, samples are collected twice monthly during most of the year, and weekly when the bypass is inundated.
#'        YBFMP samples with micro (50 \eqn{\mu}m) and meso (150 \eqn{\mu}m) zooplankton nets.
#'        \href{https://portal.edirepository.org/nis/mapbrowse?scope=edi&identifier=494}{Data are available here.}}
#'   }
#'
#' @name zooper
"_PACKAGE"

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))
