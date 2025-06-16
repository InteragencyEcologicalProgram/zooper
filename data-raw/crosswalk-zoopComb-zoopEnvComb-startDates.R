## code to prepare `crosswalk, zoopComb, zoopEnvComb, and startDates` datasets goes here
require(magrittr)
require(zooper)

crosswalk<-readr::read_csv(file.path("data-raw", "crosswalk.csv"),
                           col_types=readr::cols_only(EMP_Micro="c", EMP_Meso="c", EMP_Macro="c", EMP_Lengths="c",
                                                      STN_Meso="c", STN_Macro="c", FMWT_Meso="c",
                                                      FMWT_Macro="c", twentymm_Meso="c", FRP_Meso="c",
                                                      FRP_Macro="c", YBFMP="c", LI_Meso="c",
                                                      LI_Micro="c", DOP_Meso="c", DOP_Macro="c",
                                                      Lifestage="c", Taxname="c", Level="c",
                                                      Phylum="c", Class="c", Order="c",
                                                      Family="c", Genus="c", Species="c",
                                                      Intro="d", EMPstart="d", EMPend="d",
                                                      FMWTstart="d", FMWTend="d", twentymmstart="d",
                                                      twentymmend="d", twentymmstart2="d", DOPstart="d",
                                                      DOPend="d", FRPstart = "d", FRPend = "d"))%>%
  dplyr::mutate_at(dplyr::vars(c("EMPstart", "EMPend", "Intro", "FMWTstart", "FMWTend", "twentymmstart", "twentymmend", "twentymmstart2", "DOPstart", "DOPend", "FRPstart", "FRPend")), ~readr::parse_date(as.character(.), format="%Y"))%>%
  dplyr::mutate_at(dplyr::vars(c("EMPstart", "FMWTstart", "twentymmstart", "twentymmstart2", "EMPend", "FMWTend", "twentymmend", "DOPstart", "DOPend", "FRPstart", "FRPend")), ~tidyr::replace_na(., lubridate::ymd("2500-01-01")))%>% #Change any NAs for starts or ends to 2500 (i.e. never started or ended, super far in the future)
  dplyr::mutate(EMPend = dplyr::if_else(is.finite(.data$EMPend), .data$EMPend+lubridate::years(1), .data$EMPend))%>% #Change end dates to beginning of next year (first day it was not counted)
  dplyr::mutate(FMWTend = dplyr::if_else(is.finite(.data$FMWTend), .data$FMWTend+lubridate::years(1), .data$FMWTend))%>% #Change end dates to beginning of next year (first day it was not counted)
  dplyr::mutate(twentymmend = dplyr::if_else(is.finite(.data$twentymmend), .data$twentymmend+lubridate::years(1), .data$twentymmend))%>% #Change end dates to beginning of next year (first day it was not counted)
  dplyr::mutate(DOPend = dplyr::if_else(is.finite(.data$DOPend), .data$DOPend+lubridate::years(1), .data$DOPend))%>% #Change end dates to beginning of next year (first day it was not counted)
  dplyr::mutate(FRPend = dplyr::if_else(is.finite(.data$FRPend), .data$FRPend+lubridate::years(1), .data$FRPend))%>% #Change end dates to beginning of next year (first day it was not counted)
  dplyr::mutate(Intro=tidyr::replace_na(.data$Intro, lubridate::ymd("1800-01-01"))) #Change any NAs in Intro date to 1800 (i.e., always been around)

zoop<-Zoopdownloader(Data_sets=c("EMP_Meso", "FMWT_Meso", "STN_Meso",
                                 "20mm_Meso", "FRP_Meso","EMP_Micro",
                                 "FRP_Macro", "EMP_Macro", "FMWT_Macro",
                                 "STN_Macro", "YBFMP_Meso", "YBFMP_Micro", "DOP_Macro", "DOP_Meso"),
                     Data_folder=tempdir(), Save_object=FALSE, Return_object=TRUE,
                     Redownload_data=TRUE, Crosswalk=crosswalk, Biomass=TRUE)

zoopComb <- zoop$Zooplankton

zoopEnvComb <- zoop$Environment

startDates<-zoopComb%>%
  dplyr::left_join(zoopEnvComb%>%
                     dplyr::select(Date, SampleID),
                   by="SampleID")%>%
  dplyr::select(Source, SizeClass, Date)%>%
  dplyr::distinct()%>%
  dplyr::group_by(Source, SizeClass)%>%
  dplyr::summarise(Startdate = min(Date), .groups="drop")

usethis::use_data(zoopComb, zoopEnvComb, startDates, crosswalk, overwrite = TRUE)

