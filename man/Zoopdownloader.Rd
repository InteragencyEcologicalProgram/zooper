% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zoop_downloader.R
\name{Zoopdownloader}
\alias{Zoopdownloader}
\title{Downloads and combines zooplankton datasets collected by the Interagency Ecological Program from the Sacramento-San Joaquin Delta}
\usage{
Zoopdownloader(
  Data_sets = c("EMP_Meso", "FMWT_Meso", "STN_Meso", "20mm_Meso", "FRP_Meso",
    "EMP_Micro", "FRP_Macro", "EMP_Macro", "FMWT_Macro", "STN_Macro", "DOP_Meso",
    "DOP_Macro"),
  Biomass = TRUE,
  Data_folder = tempdir(),
  Save_object = TRUE,
  Return_object = FALSE,
  Return_object_type = "List",
  Redownload_data = FALSE,
  Download_method = "auto",
  Zoop_path = file.path(Data_folder, "zoopforzooper"),
  Env_path = file.path(Data_folder, "zoopenvforzooper"),
  Crosswalk = zooper::crosswalk,
  Stations = zooper::stations
)
}
\arguments{
\item{Data_sets}{Datasets to include in combined data. Choices include "EMP_Meso", "FMWT_Meso", "STN_Meso", "20mm_Meso", "FRP_Meso", "YBFMP_Meso", "EMP_Micro", "YBFMP_Micro", "FRP_Macro", "EMP_Macro", "FMWT_Macro", "STN_Macro", "DOP_Macro", and "DOP_Meso". Defaults to including all datasets except the two YBFMP datasets.}

\item{Biomass}{Whether to add carbon biomass (carbon biomass per unit effort (\eqn{\mu}g/ \ifelse{html}{\out{m<sup>3</sup>}}{\eqn{m^{3}}})) to the dataset (where conversion equations and required data are available). Defaults to \code{Biomass = TRUE}}

\item{Data_folder}{Path to folder in which source datasets are stored, and to which you would like datasets to be downloaded if you set \code{Redownload_data = TRUE}. If you do not want to store every source dataset, you can leave this at the default \code{tempdir()}. If you do not wish to redownload these datasets every time you run the function, you can set this to a directory on your computer and run the function in the future with \code{Redownload_data = FALSE}, which will load the source datasets from \code{Data_folder} instead of downloading them again.}

\item{Save_object}{Should the combined data be saved to disk? Defaults to \code{Save_object = TRUE}.}

\item{Return_object}{Should data be returned as an R object? If \code{TRUE}, the function will return the full combined dataset. Defaults to `Return_object = FALSE`.}

\item{Return_object_type}{If \code{Return_object = TRUE}, should data be returned as a combined dataframe (\code{Return_object_type = "Combined"}) or a list with component "Zooplankton" containing the zooplankton data and component "Environment" containing the environmental data (\code{Return_object_type = "List"}, the default). A list is required to feed data into the \code{Zoopsynther} function without saving the combined dataset to disk.}

\item{Redownload_data}{Should source datasets be redownloaded from the internet? Defaults to \code{Redownload_data = FALSE}.}

\item{Download_method}{Method used to download files. See argument \code{method} options in \code{\link[utils]{download.file}}. Defaults to "curl".}

\item{Zoop_path}{File path specifying the folder and filename of the zooplankton dataset. Defaults to \code{Zoop_path = file.path(Data_folder, "zoopforzooper")}.}

\item{Env_path}{File path specifying the folder and filename of the dataset with accessory environmental parameters. Defaults to \code{Env_path = file.path(Data_folder, "zoopenvforzooper")}.}

\item{Crosswalk}{Crosswalk table to be used for conversions. Must have columns named for each unique combination of source and size class with an underscore separator, as well as all taxonomic levels Phylum through Species, Taxname (full scientific name) and Lifestage. See \code{\link{crosswalk}} (the default) for an example.}

\item{Stations}{Latitudes and longitudes for each unique station. See \code{\link{stations}} (the default) for an example.}
}
\value{
If \code{Return_object = TRUE}, returns the combined dataset as a list or tibble, depending on whether \code{Return_object_type} is set to \code{"List"} or \code{"Combined"}. If \code{Save_object = TRUE}, writes 2 .Rds files to disk: one with the zooplankton catch data and another with accessory environmental parameters.
}
\description{
This function downloads all IEP zooplankton datasets from the internet,
converts them to a consistent format, binds them together, and exports
the combined dataset as .Rds R data files and/or an R object.
Datasets currently include "EMP" (Environmental Monitoring Program),
"FRP" (Fish Restoration Program), "FMWT" (Fall Midwater Trawl), "STN" (Townet Survey), "20mm" (20mm survey),
"DOP" (Directed Outflow Project Lower Trophic Study), and "YBFMP" (Yolo Bypass Fish Monitoring Program).
}
\details{
Note that EMP Macro samples with QAQC flags (any value of AmphipodCode other than "A") have had their Amphipod CPUE set to NA in this function. For more information on the source datasets see \code{\link{zooper}}.
}
\examples{
\dontrun{
Data <- Zoopdownloader(Data_folder = tempdir(), Return_object = TRUE,
Save_object = FALSE, Redownload_data = TRUE)
}
}
\seealso{
\code{\link{Zoopsynther}}, \code{\link{crosswalk}}, \code{\link{stations}}, \code{\link{zooper}}
}
\author{
Sam Bashevkin
}
\keyword{download}
\keyword{integration}
\keyword{synthesis}
\keyword{zooplankton}
