#' Taxonomic crosswalk among datasets
#'
#' A crosswalk table relating the taxonomic code used by each dataset to current scientific names, life stages, and taxonomic hierarchies.
#'
#' @format a tibble with 318 rows and 28 variables
#' \describe{
#'   \item{EMP_Micro}{Taxonomic codes used in the Environmental Monitoring Program microzooplankton (43 \eqn{\mu}m) mesh dataset}
#'   \item{EMP_Meso}{Taxonomic codes used in the Environmental Monitoring Program mesozooplankton (160 \eqn{\mu}m) mesh dataset}
#'   \item{EMP_Macro}{Taxonomic codes used in the Environmental Monitoring Program macrozooplankton (505 \eqn{\mu}m mesh) dataset}
#'   \item{STN_Meso}{Taxonomic codes used in the Townet Survey mesozooplankton (160 \eqn{\mu}m mesh) dataset}
#'   \item{STN_Macro}{Taxonomic codes used in the Townet Survey macrozooplankton (505 \eqn{\mu}m mesh) dataset}
#'   \item{FMWT_Meso}{Taxonomic codes used in the Fall Midwater Trawl mesozooplankton (160 \eqn{\mu}m mesh) dataset}
#'   \item{FMWT_Macro}{Taxonomic codes used in the Fall Midwater Trawl macrozooplankton (505 \eqn{\mu}m mesh) dataset}
#'   \item{twentymm_Meso}{Taxonomic codes used in the 20mm Survey mesozooplankton (160 \eqn{\mu}m mesh) dataset}
#'   \item{FRP_Meso}{Taxonomic codes used in the Fish Restoration Program mesozooplankton (150 \eqn{\mu}m mesh) dataset}
#'   \item{FRP_Macro}{Taxonomic codes used in the Fish Restoration Program macrozooplankton (500 \eqn{\mu}m mesh) dataset}
#'   \item{YBFMP}{Taxonomic codes used in the Yolo Bypass Fish Monitoring Program zooplankton dataset)}
#'   \item{Lifestage}{Plankton lifestage}
#'   \item{Taxname}{Current scientific name}
#'   \item{Level}{Taxonomic level of the taxa}
#'   \item{Phylum}{Phylum}
#'   \item{Class}{Class}
#'   \item{Order}{Order}
#'   \item{Family}{Family}
#'   \item{Genus}{Genus}
#'   \item{Species}{Species}
#'   \item{Intro}{Introduction year for non-native species}
#'   \item{EMPstart}{First year the Environmental Monitoring Program starting counting this taxa}
#'   \item{EMPend}{Last year the Environmental Monitoring Program counted this taxa}
#'   \item{FMWTstart}{First year the Fall Midwater Trawl starting counting this taxa}
#'   \item{FMWTend}{Last year the Fall Midwater Trawl counted this taxa}
#'   \item{twentymmstart}{First year the 20mm Survey starting counting this taxa}
#'   \item{twentymmend}{Last year the 20mm Survey counted this taxa}
#'   \item{twentymmstart2}{First year the 20mm Survey restarted counting this taxa}
#'   }
#' @seealso \code{\link{Zoopdownloader}}, \code{\link{Zoopsynther}}, \code{\link{zooper}}
"crosswalk"

#' Taxa undersampled in each size class
#'
#' A table listing the taxonomic names and life stages of plankton undersampled by each net mesh size (i.e. size class)
#'
#' @format a tibble with 22 rows and 3 columns
#' \describe{
#'   \item{SizeClass}{The size class of zooplankton intended to be capture be each net mesh size. Micro corresponds to 43 \eqn{\mu}m mesh and Meso corresponds to 150-160 \eqn{\mu}m mesh.}
#'   \item{Taxname}{The scientific name of taxa undersampled by the corresponding mesh size class}
#'   \item{Lifestage}{The lifestage of each taxa undersampled by the corresponding mesh size class}
#' }
#' @seealso \code{\link{Zoopsynther}}, \code{\link{zooper}}
"undersampled"

#' Station locations
#'
#' Latitudes and longitudes for each zooplankton station.
#'
#' @format a tibble with 384 rows and 4 columns
#' \describe{
#'   \item{Source}{Abbreviated name of the source dataset}
#'   \item{Station}{Sampling station name}
#'   \item{Latitude}{Latitude in decimal degrees}
#'   \item{Longitude}{Longitude in decimal degrees}
#' }
#' @seealso \code{\link{Zoopdownloader}}, \code{\link{zooper}}
"stations"

#' EMP EZ Station locations
#'
#' Latitudes and longitudes for EMP EZ stations on each sampling date from 2004 to present.
#'
#' @format a tibble with 412 rows and 4 columns
#' \describe{
#'   \item{Date}{Date sample was collected}
#'   \item{Station}{Sampling station name}
#'   \item{Latitude}{Latitude in decimal degrees}
#'   \item{Longitude}{Longitude in decimal degrees}
#' }
#' @seealso \code{\link{Zoopdownloader}}, \code{\link{zooper}}
"stationsEMPEZ"

#' Combined zooplankton dataset
#'
#' All source zooplankton datasets combined into one tibble.
#'
#' @format a tibble with 2,420,299 rows and 14 columns.
#' \describe{
#'   \item{Source}{Abbreviated name of the source dataset. "EMP"=Environmental Monitoring Program, "FRP"=Fish Restoration Program, "FMWT"= Fall Midwater Trawl), "STN"= Townet Survey, and "20mm" =20mm survey.}
#'   \item{SizeClass}{Net size class. Micro corresponds to 43 \eqn{\mu}m mesh, Meso corresponds to 150-160 \eqn{\mu}m mesh, and Macro corresponds to 500-505 \eqn{\mu}m mesh. However, prior to 1974 EMP macrozooplankton were sampled with a 930 \eqn{\mu}m mesh net.}
#'   \item{Volume}{Volume (\ifelse{html}{\out{m<sup>3</sup>}}{\eqn{m^{3}}}) of water sampled}
#'   \item{Lifestage}{Zooplankton life stage}
#'   \item{Taxname}{Scientific name}
#'   \item{Phylum}{Phylum}
#'   \item{Class}{Class}
#'   \item{Order}{Order}
#'   \item{Family}{Family}
#'   \item{Genus}{Genus}
#'   \item{Species}{Species}
#'   \item{Taxlifestage}{Combined Taxname and Lifestage}
#'   \item{SampleID}{Unique ID of the zooplankton sample. This key and \code{SizeClass} link to the \code{zoopEnvComb} dataset}
#'   \item{CPUE}{Catch per unit effort (number \ifelse{html}{\out{m<sup>-3</sup>}}{\eqn{m^{-3}}})}
#' }
#' @seealso \code{\link{Zoopdownloader}}, \code{\link{Zoopsynther}}, \code{\link{zooper}}
"zoopComb"

#' Environmental data
#'
#' Accessory environmental data from the combined zooplankton dataset. Not all datasets report all environmental parameters.
#'
#' @encoding UTF-8
#' @format a tibble with 37,418 rows and 19 columns
#' \describe{
#'   \item{Source}{Abbreviated name of the source dataset.  "EMP"=Environmental Monitoring Program, "FRP"=Fish Restoration Program, "FMWT"= Fall Midwater Trawl), "STN"= Townet Survey, and "20mm" =20mm survey.}
#'   \item{Year}{Year sample was collected}
#'   \item{Date}{Date sample was collected}
#'   \item{Datetime}{Date and time sample was collected, if time was provided}
#'   \item{Tide}{Tidal stage}
#'   \item{Station}{Station where sample was collected. This is the key that links to the  \code{stations} dataset}
#'   \item{Chl}{Chlorophyll concentration in \eqn{\mu}g/L}
#'   \item{Secchi}{Secchi depth in cm}
#'   \item{Temperature}{Temperature in °C.}
#'   \item{BottomDepth}{Total depth of the water column in m}
#'   \item{Turbidity}{Water turbidity in NTU}
#'   \item{Microcystis}{Intensity of Microcystis bloom coded qualitatively from 1-5 where 1 = absent, 2 = low, 3 = medium, 4 = high, 5 = very high}
#'   \item{pH}{Water pH}
#'   \item{DO}{Dissolved oxygen in mg/L}
#'   \item{SalSurf}{Surface salinity in PPT}
#'   \item{SalBott}{Bottom salinity in PPT}
#'   \item{Latitude}{Latitude in decimal degrees}
#'   \item{Longitude}{Longitude in decimal degrees}
#'   \item{SampleID}{Unique ID of the zooplankton sample. This is the key that links to the \code{zoopComb} dataset}
#' }
#' @seealso \code{\link{Zoopdownloader}}, \code{\link{Zoopsynther}}, \code{\link{zooper}}
"zoopEnvComb"


#' All taxonomic names
#'
#' A complete list of all valid taxonomic names included in the full dataset. Used to limit choices for filtering by taxa.
#'
#' @format a character vector of length 391.
#' @seealso \code{\link{Taxnamefinder}}, \code{\link{Zoopsynther}}, \code{\link{zooper}}
#'
"completeTaxaList"


#' Start dates
#'
#' First dates sampled by each survey and size class
#'
#' @format a tibble with 3 columns and 10 rows
#' \describe{
#'   \item{Source}{Abbreviated name of the source dataset.  "EMP"=Environmental Monitoring Program, "FRP"=Fish Restoration Program, "FMWT"= Fall Midwater Trawl), "STN"= Townet Survey, and "20mm" =20mm survey.}
#'   \item{SizeClass}{Net size class. Micro corresponds to 43 \eqn{\mu}m mesh, Meso corresponds to 150-160 \eqn{\mu}m mesh, and Macro corresponds to 500-505 \eqn{\mu}m mesh. However, prior to 1974 EMP macrozooplankton were sampled with a 930 \eqn{\mu}m mesh net.}
#'   \item{Startdate}{Date first sample was collected.}
#' }
#' @seealso \code{\link{Uncountedyears}}, \code{\link{Zoopsynther}}
#'
"startDates"
