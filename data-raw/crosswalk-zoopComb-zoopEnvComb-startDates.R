## code to prepare `crosswalk, zoopComb, zoopEnvComb, and startDates` datasets goes here
require(magrittr)

crosswalk<-readxl::read_excel(file.path("data-raw", "crosswalk.xlsx"), sheet = "Hierarchy2")%>%
  dplyr::mutate_at(dplyr::vars(c("EMPstart", "EMPend", "Intro", "FMWTstart", "FMWTend", "twentymmstart", "twentymmend", "twentymmstart2")), ~readr::parse_date(as.character(.), format="%Y"))%>%
  dplyr::mutate_at(dplyr::vars(c("EMPstart", "FMWTstart", "twentymmstart", "twentymmstart2", "EMPend", "FMWTend", "twentymmend")), ~tidyr::replace_na(., lubridate::as_date(Inf)))%>% #Change any NAs for starts or ends to Infinity (i.e. never started or ended)
  dplyr::mutate(EMPend = dplyr::if_else(is.finite(.data$EMPend), .data$EMPend+lubridate::years(1), .data$EMPend))%>% #Change end dates to beginning of next year (first day it was not counted)
  dplyr::mutate(FMWTend = dplyr::if_else(is.finite(.data$FMWTend), .data$FMWTend+lubridate::years(1), .data$FMWTend))%>% #Change end dates to beginning of next year (first day it was not counted)
  dplyr::mutate(twentymmend = dplyr::if_else(is.finite(.data$twentymmend), .data$twentymmend+lubridate::years(1), .data$twentymmend))%>% #Change end dates to beginning of next year (first day it was not counted)
  dplyr::mutate(Intro=tidyr::replace_na(.data$Intro, lubridate::as_date(-Inf))) #Change any NAs in Intro date to -Inf (i.e., always been around)

zoop<-Zoopdownloader(Data_folder=tempdir(), Save_object=FALSE, Return_object=TRUE, Redownload_data=TRUE, Crosswalk=crosswalk)

zoopComb <- zoop$Zooplankton
zoopEnvComb <- zoop$Environment

startDates<-zoopComb%>%
  dplyr::left_join(zoopEnvComb%>%
                     dplyr::select(Date, SampleID),
                   by="SampleID")%>%
  dplyr::select(Source, SizeClass, Date)%>%
  dplyr::distinct()%>%
  dplyr::group_by(Source, SizeClass)%>%
  dplyr::summarise(Startdate = min(Date))

usethis::use_data(zoopComb, zoopEnvComb, startDates, crosswalk, overwrite = TRUE)
