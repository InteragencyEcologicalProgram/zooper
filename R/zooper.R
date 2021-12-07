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
#'   \item{Environmental Monitoring Program (EMP)}{The EMP zooplankton survey is run by the California Department of Fish and Wildlife. Zooplankton were first collected in 1972. It samples monthly at 17 fixed stations, 2 floating entrapment zone stations, and 3 stations in Carquinez Strait and San Pablo Bay that are only sampled during high outflow and low salinity conditions. EMP samples using micro (43 \eqn{\mu}m), meso (160 \eqn{\mu}m), and macro (505 \eqn{\mu}m) zooplankton nets. Note that additional Amphipod data with quality issues (e.g., vegetation in net) are available in the EMP data publication. \href{https://portal.edirepository.org/nis/mapbrowse?scope=edi&identifier=522}{Data are available here.}}
#'   \item{20-mm Survey}{The 20-mm survey is run by the California Department of Fish and Wildlife. Zooplankton were first collected in 1995. Zooplankton are collected concurrently with fish samples at 41-55 fixed open-channel stations per year. Samples are collected twice per month between March and July. Only Mesozooplankton are collected with a 160 \eqn{\mu}m mesh net. \href{ftp://ftp.dfg.ca.gov/Delta\%20Smelt}{Data are available here.}}
#'   \item{Fall Midwater Trawl (FMWT) and Summer Townet Survey (STN)}{The FMWT and STN are run by the California Department of Fish and Wildlife. FMWT samples are collected monthly between September and December from a subset of the 122 fixed open-channel stations. STN samples are collected monthly between June and August from 40 fixed open-channel stations. Macrozooplankton have been collected since 2007 with a 505 \eqn{\mu}m mesh net while mesozooplankton have been collected since 2005 with a 160 \eqn{\mu}m mesh net. \href{ftp://ftp.dfg.ca.gov/TownetFallMidwaterTrawl/Zoopl_TownetFMWT}{Data are available here.}. Supplemental sampling from the Suisun Marsh Salinity Control Gate study data are also included and \href{ftp://ftp.dfg.ca.gov/TownetFallMidwaterTrawl/Zooplankton_SMSCG}{those data can be found here.}}
#'   \item{Fish Restoration Program (FRP)}{FRP is run by the California Department of Fish and Wildlife. Zooplankton were first collected in 2015. Samples are collected monthly between March and December in shallow-water habitats near marshes. FRP samples with meso (150 \eqn{\mu}m) and macro (500 \eqn{\mu}m) zooplankton nets. \href{doi.org/10.6073/pasta/86810e72766ad19fccb1b9dd3955bdf8}{Data are available here.}}
#'   }
#'
#' @docType package
#' @name zooper
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))
