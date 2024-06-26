#' Taxonomic crosswalk among datasets
#'
#' A crosswalk table relating the taxonomic code used by each dataset to current scientific names, life stages, and taxonomic hierarchies.
#'
#' @format a tibble with 404 rows and 34 variables
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
#'   \item{DOP_Meso}{Taxonomic codes used in the Directed Outflow Project mesozooplankton (150 \eqn{\mu}m mesh) dataset}
#'   \item{DOP_Macro}{Taxonomic codes used in the Directed Outflow Project macrozooplankton (500 \eqn{\mu}m mesh) dataset}
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
#'   \item{DOPstart}{First year DOP starting counting this taxa}
#'   \item{DOPend}{Last year DOP counted this taxa}
#'   }
#' @seealso \code{\link{Zoopdownloader}}, \code{\link{Zoopsynther}}, \code{\link{zooper}}
"crosswalk"

#' Taxa undersampled in each size class
#'
#' A table listing the taxonomic names and life stages of plankton undersampled by each net mesh size (i.e. size class)
#'
#' @format a tibble with 27 rows and 3 columns
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
#' @format a tibble with 387 rows and 4 columns
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
#' @format a tibble with 491 rows and 4 columns
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
#' @format a tibble with 3,615,105 rows and 14 columns.
#' \describe{
#'   \item{Source}{Abbreviated name of the source dataset. "EMP"=Environmental Monitoring Program, "FRP"=Fish Restoration Program, "FMWT"= Fall Midwater Trawl, "STN"= Townet Survey, "20mm" =20mm survey, "DOP" = Directed Outflow Project Lower Trophic Study, and "YBFMP"= Yolo Bypass Fish Monitoring Program.}
#'   \item{SizeClass}{Net size class. Micro corresponds to 43-50 \eqn{\mu}m mesh, Meso corresponds to 150-160 \eqn{\mu}m mesh, and Macro corresponds to 500-505 \eqn{\mu}m mesh. However, prior to 1974 EMP macrozooplankton were sampled with a 930 \eqn{\mu}m mesh net.}
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
#'   \item{BPUE}{Biomass per unit effort (\eqn{\mu}g/ \ifelse{html}{\out{m<sup>3</sup>}}{\eqn{m^{3}}})}
#' }
#' @details Note that EMP Macro samples with QAQC flags (any value of AmphipodCode other than "A") have had their Amphipod CPUE set to NA in this integrated dataset. For more information on the source datasets see \code{\link{zooper}}.
#' @seealso \code{\link{Zoopdownloader}}, \code{\link{Zoopsynther}}, \code{\link{zooper}}
"zoopComb"

#' Environmental data
#'
#' Accessory environmental data from the combined zooplankton dataset. Not all datasets report all environmental parameters.
#'
#' @encoding UTF-8
#' @format a tibble with 44,690 rows and 20 columns
#' \describe{
#'   \item{Source}{Abbreviated name of the source dataset.  "EMP"=Environmental Monitoring Program, "FRP"=Fish Restoration Program, "FMWT"= Fall Midwater Trawl, "STN"= Townet Survey, "20mm" =20mm survey, "DOP" = Directed Outflow Project Lower Trophic Study, and "YBFMP"= Yolo Bypass Fish Monitoring Program.}
#'   \item{Year}{Year sample was collected}
#'   \item{Date}{Date sample was collected}
#'   \item{Datetime}{Date and time sample was collected, if time was provided}
#'   \item{TowType}{Sample collection method identifying whether each tow was a surface, bottom, oblique, or vertical pump sample}
#'   \item{Tide}{Tidal stage}
#'   \item{Station}{Station where sample was collected. This is the key that links to the  \code{stations} dataset}
#'   \item{Chl}{Chlorophyll concentration in \eqn{\mu}g/L}
#'   \item{Secchi}{Secchi depth in cm}
#'   \item{Temperature}{Temperature in °C.}
#'   \item{BottomDepth}{Total depth of the water column in m}
#'   \item{TurbidityNTU}{Water turbidity in Nephrelometric Turbidity Units}
#'   \item{TurbidityFNU}{Water turbidity in Formazin Nephelometric Units}
#'   \item{Microcystis}{Intensity of Microcystis bloom coded qualitatively from 1-5 where 1 = absent, 2 = low, 3 = medium, 4 = high, 5 = very high}
#'   \item{pH}{Water pH}
#'   \item{DO}{Dissolved oxygen in mg/L}
#'   \item{SalSurf}{Surface salinity in PPT}
#'   \item{SalBott}{Bottom salinity in PPT}
#'   \item{Latitude}{Latitude in decimal degrees}
#'   \item{Longitude}{Longitude in decimal degrees}
#'   \item{AmphipodCode}{Code indicating sample quality for EMP macro amphipod samples (A=valid, B=questionable [veg/algal bloom in net], C=not valid [error in lab], D=suspect [possible missing data])}
#'   \item{SampleID}{Unique ID of the zooplankton sample. This is the key that links to the \code{zoopComb} dataset}
#' }
#' @seealso \code{\link{Zoopdownloader}}, \code{\link{Zoopsynther}}, \code{\link{zooper}}
"zoopEnvComb"

#' All taxonomic names
#'
#' A complete list of all valid taxonomic names included in the full dataset. Used to limit choices for filtering by taxa.
#'
#' @format a character vector of length 454.
#' @seealso \code{\link{Taxnamefinder}}, \code{\link{Zoopsynther}}, \code{\link{zooper}}
#'
"completeTaxaList"


#' Start dates
#'
#' First dates sampled by each survey and size class
#'
#' @format a tibble with 14 rows and 3 columns.
#' \describe{
#'   \item{Source}{Abbreviated name of the source dataset.  "EMP"=Environmental Monitoring Program, "FRP"=Fish Restoration Program, "FMWT"= Fall Midwater Trawl, "STN"= Townet Survey, "20mm" =20mm survey, "DOP" = Directed Outflow Project Lower Trophic Study, and "YBFMP"= Yolo Bypass Fish Monitoring Program.}
#'   \item{SizeClass}{Net size class. Micro corresponds to 43 (EMP) or 50 (YBFMP) \eqn{\mu}m mesh, Meso corresponds to 150 (FRP and DOP) or 160 (EMP, FMWT, STN, 20mm, YBFMP) \eqn{\mu}m mesh, and Macro corresponds to 500 (FRP and DOP) - 505 (EMP, FMWT, STN) \eqn{\mu}m mesh. However, prior to 1974 EMP macrozooplankton were sampled with a 930 \eqn{\mu}m mesh net.}
#'   \item{Startdate}{Date first sample was collected.}
#' }
#' @seealso \code{\link{Uncountedyears}}, \code{\link{Zoopsynther}}
#'
"startDates"

#' Meso and Micro zooplankton average biomass values
#'
#' Average biomass values for meso and micro zooplankton to be used for biomass conversions
#'
#' @format a tibble with 44 rows and 4 columns.
#' \describe{
#'   \item{Taxname}{Current scientific name.}
#'   \item{Level}{Taxonomic level of the taxa.}
#'   \item{Lifestage}{Plankton lifestage.}
#'   \item{Carbon_mass_micrograms}{Average carbon mass of an individual in micrograms.}
#' }
#' @seealso \code{\link{biomass_macro}}
#'
"biomass_mesomicro"

#' Macro zooplankton length-weight equations
#'
#' Length-weight equations for macro zooplankton to be used for biomass conversions.
#' The equations relate length in mm to dry mass in milligrams. Dry mass can be converted
#' to carbon mass by assuming 40% of dry mass is carbon mass, after Uye 1982.
#' Uye, S. 1982. Length-weight relationships of important zooplankton from the Inland Sea of Japan. Journal of the Oceanographical Society of Japan 38:149–158.
#'
#' @format a tibble with 23 rows and 9 columns.
#' \describe{
#'   \item{Taxname}{Current scientific name.}
#'   \item{Level}{Taxonomic level of the taxa.}
#'   \item{Preservative}{Preservative used to store sample before individuals were measured to develop the equations.}
#'   \item{Weight_type}{The type of weight measurement.}
#'   \item{N}{The number of individuals used in developing the equation.}
#'   \item{Min_length}{Minimum length (mm) of individuals used in developing the equation.}
#'   \item{Max_length}{Maximum length (mm) of individuals used in developing the equation.}
#'   \item{a}{Coefficient a in the equation Weight (mg) = a * Length (mm) ^ b.}
#'   \item{b}{Coefficient b in the equation Weight (mg) = a * Length (mm) ^ b.}
#' }
#' @seealso \code{\link{biomass_mesomicro}}
#'
"biomass_macro"
