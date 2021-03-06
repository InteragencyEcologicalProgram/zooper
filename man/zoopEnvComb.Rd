% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\encoding{UTF-8}
\name{zoopEnvComb}
\alias{zoopEnvComb}
\title{Environmental data}
\format{
a tibble with 37,418 rows and 19 columns
\describe{
  \item{Source}{Abbreviated name of the source dataset.  "EMP"=Environmental Monitoring Program, "FRP"=Fish Restoration Program, "FMWT"= Fall Midwater Trawl), "STN"= Townet Survey, and "20mm" =20mm survey.}
  \item{Year}{Year sample was collected}
  \item{Date}{Date sample was collected}
  \item{Datetime}{Date and time sample was collected, if time was provided}
  \item{Tide}{Tidal stage}
  \item{Station}{Station where sample was collected. This is the key that links to the  \code{stations} dataset}
  \item{Chl}{Chlorophyll concentration in \eqn{\mu}g/L}
  \item{Secchi}{Secchi depth in cm}
  \item{Temperature}{Temperature in °C.}
  \item{BottomDepth}{Total depth of the water column in m}
  \item{Turbidity}{Water turbidity in NTU}
  \item{Microcystis}{Intensity of Microcystis bloom coded qualitatively from 1-5 where 1 = absent, 2 = low, 3 = medium, 4 = high, 5 = very high}
  \item{pH}{Water pH}
  \item{DO}{Dissolved oxygen in mg/L}
  \item{SalSurf}{Surface salinity in PPT}
  \item{SalBott}{Bottom salinity in PPT}
  \item{Latitude}{Latitude in decimal degrees}
  \item{Longitude}{Longitude in decimal degrees}
  \item{SampleID}{Unique ID of the zooplankton sample. This is the key that links to the \code{zoopComb} dataset}
}
}
\usage{
zoopEnvComb
}
\description{
Accessory environmental data from the combined zooplankton dataset. Not all datasets report all environmental parameters.
}
\seealso{
\code{\link{Zoopdownloader}}, \code{\link{Zoopsynther}}, \code{\link{zooper}}
}
\keyword{datasets}
