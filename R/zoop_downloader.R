#' Downloads and combines zooplankton datasets collected by the Interagency Ecological Program from the Sacramento-San Joaquin Delta
#'
#' This function downloads all IEP zooplankton datasets from the internet, converts them to a consistent format, binds them together, and exports the combined dataset as .Rds R data files and/or an R object. Datasets currently include "EMP" (Environmental Monitoring Program), "FRP" (Fish Restoration Program), "FMWT" (Fall Midwater Trawl), "TNS" (Townet Survey), and "20mm" (20mm survey).
#' @param Data_sets Datasets to include in combined data. Choices include "EMP_Meso", "FMWT_Meso", "TNS_Meso", "20mm_Meso", "FRP_Meso","EMP_Micro", "FRP_Macro", "EMP_Macro", "FMWT_Macro", "TNS_Macro". Defaults to including all datasets.
#' @param Data_folder Path to folder in which source datasets are stored, and to which you would like datasets to be downloaded if you set \code{Redownload_data = TRUE}. If you do not want to store every source dataset, you can leave this at the default \code{tempdir()}. If you do not wish to redownload these datasets every time you run the function, you can set this to a directory on your computer and run the function in the future with \code{Redownload_data = FALSE}, which will load the source datasets from \code{Data_folder} instead of downloading them again.
#' @param Save_object Should the combined data be saved to disk? Defaults to \code{Save_object = TRUE}.
#' @param Return_object Should data be returned as an R object? If \code{TRUE}, the function will return the full combined dataset. Defaults to `Return_object = FALSE`.
#' @param Return_object_type If \code{Return_object = TRUE}, should data be returned as a combined dataframe (\code{Return_object_type = "Combined"}) or a list with component "Zooplankton" containing the zooplankton data and component "Environment" containing the environmental data (\code{Return_object_type = "List"}, the default). A list is required to feed data into the \code{Zoopsynther} function without saving the combined dataset to disk.
#' @param Redownload_data Should source datasets be redownloaded from the internet? Defaults to \code{Redownload_data = FALSE}.
#' @param Zoop_path File path specifying the folder and filename of the zooplankton dataset. Defaults to \code{Zoop_path = file.path(Data_folder, "zoopforzooper")}.
#' @param Env_path File path specifying the folder and filename of the dataset with accessory environmental parameters. Defaults to \code{Env_path = file.path(Data_folder, "zoopenvforzooper")}.
#' @param Crosswalk Crosswalk table to be used for conversions. Must have columns named for each unique combination of source and size class with an underscore separator, as well as all taxonomic levels Phylum through Species, Taxname (full scientific name) and Lifestage. See \code{\link{crosswalk}} (the default) for an example.
#' @param Stations Latitudes and longitudes for each unique station. See \code{\link{stations}} (the default) for an example.
#' @keywords download integration synthesis zooplankton
#' @import data.table
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @return If \code{Return_object = TRUE}, returns the combined dataset as a list or tibble, depending on whether \code{Return_object_type} is set to \code{"List"} or \code{"Combined"}. If \code{Save_object = TRUE}, writes 2 .Rds files to disk: one with the zooplankton catch data and another with accessory environmental parameters.
#' @author Sam Bashevkin
#' @examples
#' Data <- Zoopdownloader(Data_folder = tempdir(), Return_object = TRUE,
#' Save_object = FALSE, Redownload_data = TRUE)
#' @seealso \code{\link{Zoopsynther}}, \code{\link{crosswalk}}, \code{\link{stations}}, \code{\link{zooper}}
#' @export

Zoopdownloader <- function(
  Data_sets = c("EMP_Meso", "FMWT_Meso", "TNS_Meso",
                "20mm_Meso", "FRP_Meso","EMP_Micro",
                "FRP_Macro", "EMP_Macro", "FMWT_Macro", "TNS_Macro"),
  Data_folder = tempdir(),
  Save_object = TRUE,
  Return_object = FALSE,
  Return_object_type = "List",
  Redownload_data = FALSE,
  Zoop_path = file.path(Data_folder, "zoopforzooper"),
  Env_path = file.path(Data_folder, "zoopenvforzooper"),
  Crosswalk = zooper::crosswalk,
  Stations = zooper::stations){

  # Setup -------------------------------------------------------------------

  # Check arguments

  if (!purrr::every(Data_sets, ~.%in%c("EMP_Meso", "FMWT_Meso", "TNS_Meso",
                                       "20mm_Meso", "FRP_Meso","EMP_Micro",
                                       "FRP_Macro", "EMP_Macro", "FMWT_Macro", "TNS_Macro"))){
    stop("Data_sets must contain one or more of the following options: 'EMP_Meso', 'FMWT_Meso', 'TNS_Meso', '20mm_Meso', 'FRP_Meso', 'EMP_Micro', 'FRP_Macro', 'EMP_Macro', 'FMWT_Macro', 'TNS_Macro'.")
  }

  if (!Return_object_type%in%c("List", "Combined")){
    stop("Return_object_type must be either 'List' or 'Combined'.")
  }

  if(!purrr::every(list(Save_object, Return_object, Redownload_data), is.logical)){
    stop("Save_object, Return_object, and Redownload_data must all have logical arguments.")
  }

  # Load station key to later incorporate latitudes and longitudes

  stations <- Stations

  # Initialize list of dataframes

  data.list<-list()



  # EMP Meso ---------------------------------------------------------------------
  if("EMP_Meso"%in%Data_sets) {

    #download the file
    if (!file.exists(file.path(Data_folder, "1972-2018CBMatrix.xlsx")) | Redownload_data) {
      Downloader("ftp://ftp.wildlife.ca.gov/IEP_Zooplankton/1972-2018CBMatrix.xlsx",
                           file.path(Data_folder, "1972-2018CBMatrix.xlsx"), mode="wb", method="libcurl")
    }


    # Import the EMP data

    zoo_EMP_Meso <- readxl::read_excel(file.path(Data_folder, "1972-2018CBMatrix.xlsx"),
                                       sheet = "CB CPUE Matrix 1972-2018",
                                       col_types = c("numeric","numeric", "numeric", "numeric", "date",
                                                     "text", "text", "text", "numeric", "text", "text",
                                                     "text", rep("numeric", 62)))

    # Tranform from "wide" to "long" format, add some variables,
    # alter data to match other datasets

    data.list[["EMP_Meso"]] <- zoo_EMP_Meso%>%
      dplyr::mutate(Datetime=suppressWarnings(lubridate::parse_date_time(paste(.data$Date, .data$Time), "%Y-%m-%d %H:%M", tz="America/Los_Angeles")))%>% #create a variable for datetime
      tidyr::pivot_longer(cols=c(-.data$SurveyCode, -.data$Year, -.data$Survey, -.data$SurveyRep,
                                 -.data$Date, -.data$Station, -.data$EZStation, -.data$DWRStation,
                                 -.data$Core, -.data$Region, -.data$Secchi, -.data$`Chl-a`, -.data$Temperature,
                                 -.data$ECSurfacePreTow, -.data$ECBottomPreTow, -.data$CBVolume, -.data$Datetime, -.data$Time, -.data$TideCode),
                          names_to="EMP_Meso", values_to="CPUE")%>% #transform from wide to long
      dplyr::mutate(Source="EMP",
                    SizeClass="Meso")%>% #add variable for data source
      dplyr::select(.data$Source, .data$Year, .data$Date, .data$Datetime, Tide=.data$TideCode,
                    .data$Station, .data$Region, Chl = .data$`Chl-a`, CondBott = .data$ECBottomPreTow, CondSurf = .data$ECSurfacePreTow, .data$Secchi, .data$SizeClass,
                    .data$Temperature, Volume = .data$CBVolume, .data$EMP_Meso, .data$CPUE)%>% #Select for columns in common and rename columns to match
      dplyr::left_join(Crosswalk%>% #Add in Taxnames, Lifestage, and taxonomic info
                         dplyr::select(.data$EMP_Meso, .data$Lifestage, .data$Taxname, .data$Phylum, .data$Class, .data$Order, .data$Family, .data$Genus, .data$Species, .data$Intro, .data$EMPstart, .data$EMPend)%>% #only retain EMP codes
                         dplyr::filter(!is.na(.data$EMP_Meso))%>% #Only retain Taxnames corresponding to EMP codes
                         dplyr::distinct(),
                       by="EMP_Meso")%>%
      dplyr::filter(!is.na(.data$Taxname))%>% #Should remove all the summed categories in original dataset
      dplyr::mutate(Taxlifestage=paste(.data$Taxname, .data$Lifestage), #create variable for combo taxonomy x life stage
                    SampleID=paste(.data$Source, .data$Station, .data$Date))%>% #Create identifier for each sample
      dplyr::mutate(CPUE=dplyr::case_when(
        .data$CPUE!=0 ~ .data$CPUE,
        .data$CPUE==0 & .data$Date < .data$Intro ~ 0,
        .data$CPUE==0 & .data$Date >= .data$Intro & .data$Date < .data$EMPstart ~ NA_real_,
        .data$CPUE==0 & .data$Date >= .data$EMPstart & .data$Date < .data$EMPend ~ 0,
        .data$CPUE==0 & .data$Date >= .data$EMPend ~ NA_real_
      ))%>%
      dplyr::select(-.data$EMP_Meso, -.data$EMPstart, -.data$EMPend, -.data$Intro)%>% #Remove EMP taxa codes
      dplyr::select(-.data$Datetime)%>% #Add this back in when other EMP data have time
      dtplyr::lazy_dt()%>% #Speed up code using dtplyr package that takes advantage of data.table speed
      dplyr::group_by_at(dplyr::vars(-.data$CPUE))%>%
      dplyr::summarise(CPUE=sum(.data$CPUE, na.rm=TRUE))%>% #Some taxa now have the same names (e.g., CYCJUV and OTHCYCJUV)
      #so we now add those categories together.
      dplyr::ungroup()%>%
      tibble::as_tibble() #required to finish operation after lazy_dt()

  }
  # FMWT Meso --------------------------------------------------------------------
  if("FMWT_Meso"%in%Data_sets | "TNS_Meso"%in%Data_sets) {

    #download the file
    if (!file.exists(file.path(Data_folder, "FMWT_TNSZooplanktonDataCPUEOct2017.xls")) | Redownload_data) {
      Downloader("ftp://ftp.wildlife.ca.gov/TownetFallMidwaterTrawl/Zoopl_TownetFMWT/FMWT%20TNS%20ZooplanktonDataCPUE01Apr2020.xls",
                           file.path(Data_folder, "FMWT_TNSZooplanktonDataCPUEOct2017.xls"), mode="wb", method="libcurl")
    }

    # Import the FMWT data

    suppressWarnings(zoo_FMWT_Meso <- readxl::read_excel(file.path(Data_folder, "FMWT_TNSZooplanktonDataCPUEOct2017.xls"),
                                                         sheet = "FMWT&TNS ZP CPUE",
                                                         col_types=c("text", rep("numeric", 3), "date", "text", "text",
                                                                     "text", "numeric", rep("text", 3), rep("numeric", 3),
                                                                     "text", rep("numeric", 5), "text", rep("numeric", 55))))

    # Tranform from "wide" to "long" format, add some variables,
    # alter data to match other datasets

    data.list[["FMWT_Meso"]] <- zoo_FMWT_Meso%>%
      dplyr::mutate(Datetime=suppressWarnings(lubridate::parse_date_time(paste(.data$Date, .data$Time), "%Y-%m-%d %H:%M", tz="America/Los_Angeles")))%>% #create a variable for datetime
      tidyr::pivot_longer(cols=c(-.data$Project, -.data$Year, -.data$Survey, -.data$Month, -.data$Date, -.data$Datetime,
                                 -.data$Station, -.data$Index, -.data$Time, -.data$TowDuration,
                                 -.data$Region, -.data$FLaSHRegionGroup, -.data$TideCode,
                                 -.data$DepthBottom, -.data$CondSurf, -.data$PPTSurf,
                                 -.data$SurfSalinityGroup, -.data$CondBott, -.data$PPTBott,
                                 -.data$TempSurf, -.data$Secchi, -.data$Turbidity, -.data$Microcystis,
                                 -.data$TotalMeter, -.data$Volume),
                          names_to="FMWT_Meso", values_to="CPUE")%>% #transform from wide to long
      dplyr::select(Source = .data$Project, .data$Year, .data$Date, .data$Datetime, .data$Station, .data$Region, Tide = .data$TideCode, BottomDepth = .data$DepthBottom, .data$CondSurf, .data$CondBott, Temperature = .data$TempSurf, .data$Secchi, .data$Turbidity, .data$Microcystis, .data$Volume, .data$FMWT_Meso, .data$CPUE)%>% #Select for columns in common and rename columns to match
      dplyr::left_join(Crosswalk%>% #Add in Taxnames, Lifestage, and taxonomic info
                         dplyr::select(.data$FMWT_Meso, .data$Lifestage, .data$Taxname, .data$Phylum, .data$Class, .data$Order, .data$Family, .data$Genus, .data$Species, .data$Intro, .data$FMWTstart, .data$FMWTend)%>% #only retain FMWT codes
                         dplyr::filter(!is.na(.data$FMWT_Meso))%>% #Only retain Taxnames corresponding to FMWT codes
                         dplyr::distinct(),
                       by = "FMWT_Meso")%>%
      dplyr::filter(!is.na(.data$Taxname))%>%
      dplyr::mutate(Taxlifestage=paste(.data$Taxname, .data$Lifestage), #create variable for combo taxonomy x life stage
                    Microcystis=dplyr::if_else(.data$Microcystis=="6", "2", .data$Microcystis), #Microsystis value of 6 only used from 2012-2015 and is equivalent to a 2 in other years, so just converting all 6s to 2s.
                    SampleID=paste(.data$Source, .data$Station, .data$Datetime),
                    SizeClass="Meso")%>% #Create identifier for each sample
      dplyr::ungroup()%>%
      dplyr::mutate(CPUE=dplyr::case_when(
        .data$CPUE!=0 ~ CPUE,
        .data$CPUE==0 & .data$Date < .data$Intro ~ 0,
        .data$CPUE==0 & .data$Date >= .data$Intro & .data$Date < .data$FMWTstart ~ NA_real_,
        .data$CPUE==0 & .data$Date >= .data$FMWTstart & .data$Date < .data$FMWTend ~ 0,
        .data$CPUE==0 & .data$Date >= .data$FMWTend ~ NA_real_
      ))%>%
      dplyr::filter(!is.na(.data$CPUE))%>%
      dplyr::select(-.data$FMWT_Meso, -.data$FMWTstart, -.data$FMWTend, -.data$Intro)%>% #Remove FMWT taxa codes
      {if(!("FMWT_Meso"%in%Data_sets)){
  dplyr::filter(., .data$Source != "FMWT")
      } else{
  .
}}%>%
      {if(!("TNS_Meso"%in%Data_sets)){
        dplyr::filter(., .data$Source != "TNS")
      } else{
        .
      }}



  }
  # twentymm Meso ----------------------------------------------------------------

  if("20mm_Meso"%in%Data_sets) {

    #download the file
    if (!file.exists(file.path(Data_folder, "CDFW 20-mm Zooplankton Catch Matrix.xlsx")) | Redownload_data) {
      Downloader("ftp://ftp.dfg.ca.gov/Delta%20Smelt/20mm%20Zooplankton%20Catch%20Matrix_1995-2017.xlsx",
                           file.path(Data_folder, "CDFW 20-mm Zooplankton Catch Matrix.xlsx"), mode="wb", method="libcurl")
    }

    # Import and modify 20mm data

    zoo_20mm_Meso<-readxl::read_excel(file.path(Data_folder, "CDFW 20-mm Zooplankton Catch Matrix.xlsx"),
                                      sheet="20-mm CB CPUE Data",
                                      col_types = c("date", rep("numeric", 3), "date", rep("numeric", 80)))

    data.list[["twentymm_Meso"]]<-zoo_20mm_Meso%>%
      dplyr::mutate(SampleID = paste(.data$Station, .data$SampleDate, .data$TowNum),
                    Datetime=lubridate::parse_date_time(paste0(.data$SampleDate, " ", lubridate::hour(.data$TowTime), ":", lubridate::minute(.data$TowTime)), "%Y-%m-%d %%H:%M", tz="America/Los_Angeles"))%>%
      tidyr::pivot_longer(cols=c(-.data$SampleDate, -.data$Survey, -.data$Station, -.data$TowTime, -.data$Temp, -.data$TopEC,
                                 -.data$BottomEC, -.data$Secchi, -.data$Turbidity, -.data$Tide, -.data$BottomDepth, -.data$Duration, -.data$MeterCheck, -.data$Volume,
                                 -.data$Dilution, -.data$SampleID, -.data$Datetime),
                          names_to="twentymm_Meso", values_to="CPUE")%>% #transform from wide to long
      dplyr::select(Date=.data$SampleDate, .data$Station, Temperature = .data$Temp, CondSurf = .data$TopEC, CondBott = .data$BottomEC, .data$Secchi,
                    .data$Turbidity, .data$Tide, .data$BottomDepth, .data$Volume, .data$SampleID, .data$Datetime, .data$twentymm_Meso, .data$CPUE)%>% #Select for columns in common and rename columns to match
      dplyr::left_join(Crosswalk%>% #Add in Taxnames, Lifestage, and taxonomic info
                         dplyr::select(.data$twentymm_Meso, .data$Lifestage, .data$Taxname, .data$Phylum, .data$Class, .data$Order, .data$Family, .data$Genus, .data$Species, .data$Intro, .data$twentymmstart, .data$twentymmend, .data$twentymmstart2)%>% #only retain FMWT codes
                         dplyr::filter(!is.na(.data$twentymm_Meso))%>% #Only retain Taxnames corresponding to FMWT codes
                         dplyr::distinct(),
                       by = "twentymm_Meso")%>%
      dplyr::filter(!is.na(.data$Taxname))%>%
      dplyr::mutate(Source="twentymm",
                    SizeClass="Meso",
                    Station=as.character(.data$Station),
                    Tide=as.character(.data$Tide),
                    Taxlifestage=paste(.data$Taxname, .data$Lifestage))%>% #add variable for data source, create variable for combo taxonomy x life stage
      dplyr::mutate(CPUE=dplyr::case_when(
        .data$CPUE!=0 ~ .data$CPUE,
        .data$CPUE==0 & .data$Date < .data$Intro ~ 0,
        .data$CPUE==0 & .data$Date >= .data$Intro & .data$Date < .data$twentymmstart ~ NA_real_,
        .data$CPUE==0 & .data$Date >= .data$twentymmstart & .data$Date < .data$twentymmend ~ 0,
        .data$CPUE==0 & .data$Date >= .data$twentymmend & .data$Date < .data$twentymmstart2 ~ NA_real_,
        .data$CPUE==0 & .data$Date >= .data$twentymmstart2 ~ 0 #20mm dataset had one case of a taxa starting, ending, and starting again
      ))%>%
      dplyr::select(-.data$twentymmend, -.data$twentymmstart, -.data$twentymmstart2, -.data$Intro, -.data$twentymm_Meso)%>%
      dtplyr::lazy_dt()%>% #Speed up
      dplyr::group_by_at(dplyr::vars(-.data$CPUE))%>% #Some taxa names are repeated as in EMP so
      dplyr::summarise(CPUE=sum(.data$CPUE, na.rm=TRUE))%>% #this just adds up those duplications
      dplyr::ungroup()%>%
      tibble::as_tibble()%>%
      dplyr::mutate(SampleID=paste(.data$Source, .data$SampleID)) #Create identifier for each sample
  }

  # FRP Meso ---------------------------------------------------------------------

  if("FRP_Meso"%in%Data_sets) {

    # Import the FRP data

    #download the file
    if (!file.exists(file.path(Data_folder, "zoopsFRP2018.csv")) | Redownload_data) {
      Downloader("https://portal.edirepository.org/nis/dataviewer?packageid=edi.269.2&entityid=d4c76f209a0653aa86bab1ff93ab9853",
                           file.path(Data_folder, "zoopsFRP2018.csv"), mode="wb", method="curl")
    }

    zoo_FRP_Meso <- readr::read_csv(file.path(Data_folder, "zoopsFRP2018.csv"),
                                    col_types = "cctddddddddcccdddddc", na=c("", "NA"))

    #Already in long format
    data.list[["FRP_Meso"]] <- zoo_FRP_Meso%>%
      dplyr::mutate(Date=lubridate::parse_date_time(.data$Date, "%m/%d/%Y", tz="America/Los_Angeles"))%>%
      dplyr::mutate(Station=dplyr::recode(.data$Station, `Lindsey Tules`="Lindsey tules", LinBR="LinBr"))%>% #Rename inconsistent station names to match
      dplyr::mutate(Datetime=lubridate::parse_date_time(paste0(.data$Date, " ", lubridate::hour(.data$time), ":", lubridate::minute(.data$time)), "%Y-%m-%d %%H:%M", tz="America/Los_Angeles"))%>% #Create a variable for datetime
      dplyr::mutate(Source="FRP", #add variable for data source
                    SizeClass="Meso",
                    Microcystis = dplyr::recode(.data$Microcystis, `1=absent`="1", `2=low`="2"))%>%
      dplyr::select(.data$Source, .data$Date, .data$Datetime,
                    .data$Station, CondSurf = .data$SC, .data$Secchi, .data$pH, .data$DO, .data$Turbidity, .data$Tide, .data$Microcystis, .data$SizeClass,
                    Temperature = .data$Temp, Volume = .data$volume, FRP_Meso = .data$CommonName, .data$CPUE, .data$SampleID)%>% #Select for columns in common and rename columns to match
      dplyr::group_by_at(dplyr::vars(-.data$CPUE))%>% #Some taxa names are repeated as in EMP so
      dplyr::summarise(CPUE=sum(.data$CPUE, na.rm=T))%>% #this just adds up those duplications
      dplyr::ungroup()%>%
      tidyr::pivot_wider(names_from=.data$FRP_Meso, values_from=.data$CPUE, values_fill=list(CPUE=0))%>%
      tidyr::pivot_longer(cols=c(-.data$Source, -.data$Date, -.data$Datetime,
                                 -.data$Station, -.data$CondSurf, -.data$Secchi, -.data$pH, -.data$DO, -.data$Turbidity, -.data$Tide, -.data$Microcystis, -.data$SizeClass,
                                 -.data$Temperature, -.data$Volume, -.data$SampleID),
                          names_to="FRP_Meso", values_to="CPUE")%>%
      dplyr::left_join(Crosswalk%>% #Add in Taxnames, Lifestage, and taxonomic info
                         dplyr::select(.data$FRP_Meso, .data$Lifestage, .data$Taxname, .data$Phylum, .data$Class, .data$Order, .data$Family, .data$Genus, .data$Species)%>% #only retain FRP codes
                         dplyr::filter(!is.na(.data$FRP_Meso))%>% #Only retain Taxnames corresponding to FRP codes
                         dplyr::distinct(),
                       by = "FRP_Meso")%>%
      dplyr::mutate(Taxlifestage=paste(.data$Taxname, .data$Lifestage))%>% #create variable for combo taxonomy x life stage
      dplyr::select(-.data$FRP_Meso)%>% #Remove FRP taxa codes
      dtplyr::lazy_dt()%>% #Speed up code
      dplyr::group_by_at(dplyr::vars(-.data$CPUE))%>% #Some taxa names are repeated as in EMP so
      dplyr::summarise(CPUE=sum(.data$CPUE, na.rm=TRUE))%>% #this just adds up those duplications
      tibble::as_tibble()%>%
      dplyr::ungroup()%>%
      dplyr::mutate(SampleID=paste(.data$Source, .data$SampleID)) #Create identifier for each sample

  }

  # YBFMP Meso/Micro -------------------------------------------------------------

  #if("YBFMP"%in%Data_sets) {

  #NO IDEA WHAT TO DO ABOUT INCONSISTENT TAXONOMIC RESOLUTION WITH NO DOCUMENTATION AND LACK OF LIFE STAGE INFORMATION

  #   zoo_YBFMP<-readr::read_csv(file.path(Data_folder, "yolo_zoop_public.csv"), col_types = "ctddcccdddddddccccccccccccccccccdd")

  #  data.list[["YBFMP"]]<-zoo_YBFMP%>%
  #    dplyr::mutate(SampleDate=lubridate::parse_date_time(.data$SampleDate, "%m/%d/%Y", tz="America/Los_Angeles"))%>%
  #    dplyr::mutate(Datetime=lubridate::parse_date_time(paste(.data$SampleDate, .data$SampleTime), "%Y-%m-%d %H:%M:%S", tz="America/Los_Angeles"),
  #           Source="YBFMP",
  #           YBFMP=paste(.data$TaxonName, .data$LifeStage),
  #           SampleID=paste(.data$SampleDate, .data$StationCode))%>%
  #    select(.data$SampleID, Date = .data$SampleDate, Station = .data$StationCode, Temperature = .data$WaterTemperature, .data$Secchi, .data$Turbidity, CondSurf = .data$Conductivity, .data$SpCnd, .data$pH, .data$DO, .data$YBFMP, .data$Source, .data$Datetime, .data$NetSize, .data$CPUE)%>%
  #    dplyr::left_join(Crosswalk%>% #Add in Taxnames, Lifestage, and taxonomic info
  #                select(.data$YBFMP, .data$Lifestage, .data$Taxname, .data$Phylum, .data$Class, .data$Order, .data$Family, .data$Genus, .data$Species)%>% #only retain YBFMP codes
  #                dplyr::filter(!is.na(.data$YBFMP))%>% #Only retain Taxnames corresponding to YBFMP codes
  #                dplyr::distinct(),
  #              by = "YBFMP")%>%
  #    dplyr::mutate(Taxlifestage=paste(.data$Taxname, .data$Lifestage))%>% #create variable for combo taxonomy x life stage
  #    select(-.data$YBFMP)%>% #Remove YBFMP taxa codes
  #    dtplyr::lazy_dt()%>% #Speed up code
  #    dplyr::group_by_at(dplyr::vars(-.data$CPUE))%>% #In case some taxa names are repeated as in EMP so
  #    dplyr::summarise(CPUE=sum(.data$CPUE, na.rm=TRUE))%>% #this just adds up those duplications
  #    dplyr::ungroup()%>%
  #    tibble::as_tibble()%>%
  #    dplyr::mutate(SampleID=paste(.data$Source, .data$SampleID)) #Create identifier for each sample
#}

# EMP Micro ---------------------------------------------------------------

if("EMP_Micro"%in%Data_sets) {

  #download the file
  if (!file.exists(file.path(Data_folder, "1972-2018PumpMatrix.xlsx")) | Redownload_data) {
    Downloader("ftp://ftp.dfg.ca.gov/IEP_Zooplankton/1972-2018Pump%20Matrix.xlsx",
                         file.path(Data_folder, "1972-2018PumpMatrix.xlsx"), mode="wb", method="libcurl")
  }


  # Import the EMP data

  zoo_EMP_Micro <- readxl::read_excel(file.path(Data_folder, "1972-2018PumpMatrix.xlsx"),
                                      sheet = " Pump CPUE Matrix 1972-2018",
                                      col_types = c(rep("numeric", 4), "date", rep("text", 3), "numeric", "text", rep("numeric", 36)))

  # Tranform from "wide" to "long" format, add some variables,
  # alter data to match other datasets

  data.list[["EMP_Micro"]] <- zoo_EMP_Micro%>%
    dplyr::select(-.data$Year, -.data$SurveyCode, -.data$Survey, -.data$SurveyRep, -.data$EZStation, -.data$DWRStationNo, -.data$Core, -.data$Region)%>%
    dplyr::rename(OTHCYCADPUMP = .data$OTHCYCAD)%>%
    tidyr::pivot_longer(cols=c(-.data$SampleDate, -.data$Station, -.data$Secchi, -.data$`Chl-a`, -.data$Temperature,
                               -.data$ECSurfacePreTow, -.data$ECBottomPreTow, -.data$PumpVolume),
                        names_to="EMP_Micro", values_to="CPUE")%>% #transform from wide to long
    dplyr::mutate(Source="EMP",
                  SizeClass="Micro")%>% #add variable for data source
    dplyr::select(.data$Source, Date = .data$SampleDate, .data$Station, Chl = .data$`Chl-a`, CondBott = .data$ECBottomPreTow, CondSurf = .data$ECSurfacePreTow, .data$Secchi,
                  .data$Temperature, .data$SizeClass, Volume = .data$PumpVolume, .data$EMP_Micro, .data$CPUE)%>% #Select for columns in common and rename columns to match
    dplyr::left_join(Crosswalk%>% #Add in Taxnames, Lifestage, and taxonomic info
                       dplyr::select(.data$EMP_Micro, .data$Lifestage, .data$Taxname, .data$Phylum, .data$Class, .data$Order, .data$Family, .data$Genus, .data$Species, .data$Intro, .data$EMPstart, .data$EMPend)%>% #only retain EMP codes
                       dplyr::filter(!is.na(.data$EMP_Micro))%>% #Only retain Taxnames corresponding to EMP codes
                       dplyr::distinct(),
                     by="EMP_Micro")%>%
    dplyr::filter(!is.na(.data$Taxname))%>% #Should remove all the summed categories in original dataset
    dplyr::mutate(Taxlifestage=paste(.data$Taxname, .data$Lifestage), #create variable for combo taxonomy x life stage
                  SampleID=paste(.data$Source, .data$Station, .data$Date),
                  Tide="1")%>% #Create identifier for each sample
    dplyr::mutate(CPUE=dplyr::case_when(
      .data$CPUE!=0 ~ .data$CPUE,
      .data$CPUE==0 & .data$Date < .data$Intro ~ 0,
      .data$CPUE==0 & .data$Date >= .data$Intro & .data$Date < .data$EMPstart ~ NA_real_,
      .data$CPUE==0 & .data$Date >= .data$EMPstart & .data$Date < .data$EMPend ~ 0,
      .data$CPUE==0 & .data$Date >= .data$EMPend ~ NA_real_
    ))%>%
    dplyr::select(-.data$EMP_Micro, -.data$EMPstart, -.data$EMPend, -.data$Intro)%>% #Remove EMP taxa codes
    dtplyr::lazy_dt()%>% #Speed up code using dtplyr package that takes advantage of data.table speed
    dplyr::group_by_at(dplyr::vars(-.data$CPUE))%>%
    dplyr::summarise(CPUE=sum(.data$CPUE, na.rm=TRUE))%>% #Some taxa now have the same names (e.g., CYCJUV and OTHCYCJUV)
    #so we now add those categories together.
    dplyr::ungroup()%>%
    tibble::as_tibble() #required to finish operation after lazy_dt()

}
# FRP Macro ---------------------------------------------------------------

if("FRP_Macro"%in%Data_sets) {

  # Import the FRP data

  #download the file
  if (!file.exists(file.path(Data_folder, "bugsFRP2018.csv")) | Redownload_data) {
    Downloader("https://portal.edirepository.org/nis/dataviewer?packageid=edi.269.2&entityid=630f16b33a9cbf75f1989fc18690a6b3",
                         file.path(Data_folder, "bugsFRP2018.csv"), mode="wb", method="curl")
  }

  zoo_FRP_Macro <- readr::read_csv(file.path(Data_folder, "bugsFRP2018.csv"),
                                   col_types = "cctcddddddddccdddcddc", na=c("", "NA"))

  #Already in long format
  data.list[["FRP_Macro"]] <- zoo_FRP_Macro%>%
    dplyr::filter(.data$Sampletype=="trawl")%>%
    dplyr::mutate(Date=lubridate::parse_date_time(.data$Date, "%m/%d/%Y", tz="America/Los_Angeles"))%>%
    dplyr::mutate(Station=dplyr::recode(.data$Station, `Lindsey Tules`="Lindsey tules", LinBR="LinBr", MINSLO1="MinSlo1", ProBR="ProBr", WinBR="WinBr"))%>% #Rename inconsistent station names to match
    dplyr::mutate(Datetime=lubridate::parse_date_time(paste0(.data$Date, " ", lubridate::hour(.data$time), ":", lubridate::minute(.data$time)), "%Y-%m-%d %%H:%M", tz="America/Los_Angeles"))%>% #Create a variable for datetime
    dplyr::mutate(Source = "FRP",
                  SizeClass = "Macro",
                  CPUE = .data$AdjCount/.data$volume, #add variable for data source and calculate CPUE
                  Microcystis = dplyr::recode(.data$Microcystis, `1=absent`="1", `2=low`="2"))%>%
    dplyr::select(.data$Source, .data$Date, .data$Datetime,
                  .data$Station, CondSurf = .data$SC, .data$Secchi, .data$pH, .data$DO, .data$Turbidity, .data$Tide, .data$Microcystis, .data$SizeClass,
                  Temperature = .data$Temp, Volume = .data$volume, FRP_Macro = .data$CommonName, .data$CPUE, .data$SampleID)%>% #Select for columns in common and rename columns to match
    dplyr::group_by_at(dplyr::vars(-.data$CPUE))%>% #Some taxa names are repeated as in EMP so
    dplyr::summarise(CPUE=sum(.data$CPUE, na.rm=T))%>% #this just adds up those duplications
    dplyr::ungroup()%>%
    tidyr::pivot_wider(names_from=.data$FRP_Macro, values_from=.data$CPUE, values_fill=list(CPUE=0))%>%
    tidyr::pivot_longer(cols=c(-.data$Source, -.data$Date, -.data$Datetime,
                               -.data$Station, -.data$CondSurf, -.data$Secchi, -.data$pH, -.data$DO, -.data$Turbidity, -.data$Tide, -.data$Microcystis, -.data$SizeClass,
                               -.data$Temperature, -.data$Volume, -.data$SampleID),
                        names_to="FRP_Macro", values_to="CPUE")%>%
    dplyr::left_join(Crosswalk%>% #Add in Taxnames, Lifestage, and taxonomic info
                       dplyr::select(.data$FRP_Macro, .data$Lifestage, .data$Taxname, .data$Phylum, .data$Class, .data$Order, .data$Family, .data$Genus, .data$Species)%>% #only retain FRP codes
                       dplyr::filter(!is.na(.data$FRP_Macro))%>% #Only retain Taxnames corresponding to FRP codes
                       dplyr::distinct(),
                     by = "FRP_Macro")%>%
    dplyr::mutate(Taxlifestage=paste(.data$Taxname, .data$Lifestage))%>% #create variable for combo taxonomy x life stage
    dplyr::select(-.data$FRP_Macro)%>% #Remove FRP taxa codes
    dtplyr::lazy_dt()%>% #Speed up code
    dplyr::group_by_at(dplyr::vars(-.data$CPUE))%>% #Some taxa names are repeated as in EMP so
    dplyr::summarise(CPUE=sum(.data$CPUE, na.rm=T))%>% #this just adds up those duplications
    tibble::as_tibble()%>%
    dplyr::ungroup()%>%
    dplyr::mutate(SampleID=paste(.data$Source, .data$SampleID)) #Create identifier for each sample
}

# EMP Macro ---------------------------------------------------------------

if("EMP_Macro"%in%Data_sets) {

  #download the file
  if (!file.exists(file.path(Data_folder, "1972-2018MysidMatrix.xlsx")) | Redownload_data) {
    Downloader("ftp://ftp.dfg.ca.gov/IEP_Zooplankton/1972-2018MysidMatrix.xlsx",
                         file.path(Data_folder, "1972-2018MysidMatrix.xlsx"), mode="wb", method="libcurl")
  }


  # Import the EMP data

  zoo_EMP_Macro <- readxl::read_excel(file.path(Data_folder, "1972-2018MysidMatrix.xlsx"),
                                      sheet = "Mysid CPUE Matrix 1972-2018 ",
                                      col_types = c(rep("numeric", 4), "date", rep("text", 3), "numeric", "text", rep("numeric", 14)))

  # Tranform from "wide" to "long" format, add some variables,
  # alter data to match other datasets

  data.list[["EMP_Macro"]] <- zoo_EMP_Macro%>%
    dplyr::select(-.data$Year, -.data$SurveyCode, -.data$Survey, -.data$SurveyRep, -.data$EZStation, -.data$DWRStation, -.data$Core, -.data$Region)%>%
    tidyr::pivot_longer(cols=c(-.data$SampleDate, -.data$Station, -.data$Secchi, -.data$`Chl-a`, -.data$Temperature,
                               -.data$ECSurfacePreTow, -.data$ECBottomPreTow, -.data$MysidVolume),
                        names_to="EMP_Macro", values_to="CPUE")%>% #transform from wide to long
    dplyr::mutate(Source="EMP",
                  SizeClass="Macro")%>% #add variable for data source
    dplyr::select(.data$Source, Date = .data$SampleDate, .data$Station, Chl = .data$`Chl-a`, CondBott = .data$ECBottomPreTow, CondSurf = .data$ECSurfacePreTow, .data$Secchi, .data$SizeClass,
                  .data$Temperature, Volume = .data$MysidVolume, .data$EMP_Macro, .data$CPUE)%>% #Select for columns in common and rename columns to match
    dplyr::left_join(Crosswalk%>% #Add in Taxnames, Lifestage, and taxonomic info
                       dplyr::select(.data$EMP_Macro, .data$Lifestage, .data$Taxname, .data$Phylum, .data$Class, .data$Order, .data$Family, .data$Genus, .data$Species, .data$Intro, .data$EMPstart, .data$EMPend)%>% #only retain EMP codes
                       dplyr::filter(!is.na(.data$EMP_Macro))%>% #Only retain Taxnames corresponding to EMP codes
                       dplyr::distinct(),
                     by="EMP_Macro")%>%
    dplyr::filter(!is.na(.data$Taxname))%>% #Should remove all the summed categories in original dataset
    dplyr::mutate(Taxlifestage=paste(.data$Taxname, .data$Lifestage), #create variable for combo taxonomy x life stage
                  SampleID=paste(.data$Source, .data$Station, .data$Date), #Create identifier for each sample
                  Tide="1")%>%
    dplyr::mutate(CPUE=dplyr::case_when(
      .data$CPUE!=0 ~ .data$CPUE,
      .data$CPUE==0 & .data$Date < .data$Intro ~ 0,
      .data$CPUE==0 & .data$Date >= .data$Intro & .data$Date < .data$EMPstart ~ NA_real_,
      .data$CPUE==0 & .data$Date >= .data$EMPstart & .data$Date < .data$EMPend ~ 0,
      .data$CPUE==0 & .data$Date >= .data$EMPend ~ NA_real_
    ))%>%
    dplyr::select(-.data$EMP_Macro, -.data$EMPstart, -.data$EMPend, -.data$Intro)%>% #Remove EMP taxa codes
    dtplyr::lazy_dt()%>% #Speed up code using dtplyr package that takes advantage of data.table speed
    dplyr::group_by_at(dplyr::vars(-.data$CPUE))%>%
    dplyr::summarise(CPUE=sum(.data$CPUE, na.rm=TRUE))%>% #Some taxa now have the same names (e.g., CYCJUV and OTHCYCJUV)
    #so we now add those categories together.
    dplyr::ungroup()%>%
    tibble::as_tibble() #required to finish operation after lazy_dt()

}
# FMWT Macro --------------------------------------------------------------

if("FMWT_Macro"%in%Data_sets | "TNS_Macro"%in%Data_sets) {

  #download the file
  if (!file.exists(file.path(Data_folder, "FMWT_TNSMysidCPUEJuly2019.xlsx")) | Redownload_data) {
    Downloader("ftp://ftp.dfg.ca.gov/TownetFallMidwaterTrawl/Zoopl_TownetFMWT/FMWT%20TNSMysidCPUEJuly2019.xlsx",
                         file.path(Data_folder, "FMWT_TNSMysidCPUEJuly2019.xlsx"), mode="wb", method="libcurl")
  }

  #download the file
  if (!file.exists(file.path(Data_folder, "FMWT_TNSAmphipodCPUEJuly2019.xls")) | Redownload_data) {
    Downloader("ftp://ftp.dfg.ca.gov/TownetFallMidwaterTrawl/Zoopl_TownetFMWT/FMWT%20TNSAmphipodCPUEJuly2019.xls",
                         file.path(Data_folder, "FMWT_TNSAmphipodCPUEJuly2019.xls"), mode="wb", method="libcurl")
  }

  zoo_FMWT_Macro_Mysid <- readxl::read_excel(file.path(Data_folder, "FMWT_TNSMysidCPUEJuly2019.xlsx"),
                                             sheet = "FMWT STN Mysid CPUE Matrix")

  zoo_FMWT_Macro_Amph <- readxl::read_excel(file.path(Data_folder, "FMWT_TNSAmphipodCPUEJuly2019.xls"),
                                            sheet = "FMWT STN amphipod CPUE")

  data.list[["FMWT_Macro"]] <- zoo_FMWT_Macro_Mysid%>%
    dplyr::rename(Date = .data$SampleDate, `PPT Surface` = .data$PPTSurf)%>%
    dplyr::mutate(Tax="Mysid")%>%
    dplyr::bind_rows(zoo_FMWT_Macro_Amph%>%
                       dplyr::mutate(Tax="Amph"))%>%
    dplyr::mutate(Datetime = lubridate::parse_date_time(paste0(.data$Date, " ", lubridate::hour(.data$Time), ":", lubridate::minute(.data$Time)), "%Y-%m-%d %%H:%M", tz="America/Los_Angeles"),
                  Microcystis = as.character(.data$Microcystis))%>% #create a variable for datetime
    tidyr::pivot_longer(cols=c(-.data$Project, -.data$Year, -.data$Survey, -.data$Date, -.data$Datetime,
                               -.data$Station, -.data$Index, -.data$SMSCG, -.data$Time, -.data$TowDuration,
                               -.data$Region, -.data$FLaSHRegionGroup, -.data$TideCode,
                               -.data$DepthBottom, -.data$ConductivityTop, -.data$`PPT Surface`,
                               -.data$ConductivityBottom, -.data$`PPT Bottom`,
                               -.data$WaterTemperature, -.data$Secchi, -.data$Turbidity, -.data$Microcystis,
                               -.data$TotalMeter, -.data$Volume, -.data$Tax),
                        names_to="FMWT_Macro", values_to="CPUE")%>% #transform from wide to long
    dplyr::select(Source = .data$Project, .data$Date, .data$Datetime, .data$Station, .data$SMSCG, Tide = .data$TideCode, BottomDepth = .data$DepthBottom, CondSurf = .data$ConductivityTop, CondBott = .data$ConductivityBottom, Temperature = .data$WaterTemperature, .data$Secchi, .data$Turbidity, .data$Microcystis, .data$Volume, .data$FMWT_Macro, .data$CPUE)%>% #Select for columns in common and rename columns to match
    dplyr::left_join(Crosswalk%>% #Add in Taxnames, Lifestage, and taxonomic info
                       dplyr::select(.data$FMWT_Macro, .data$Lifestage, .data$Taxname, .data$Phylum, .data$Class, .data$Order, .data$Family, .data$Genus, .data$Species, .data$Intro, .data$FMWTstart, .data$FMWTend)%>% #only retain FMWT codes
                       dplyr::filter(!is.na(.data$FMWT_Macro))%>% #Only retain Taxnames corresponding to FMWT codes
                       dplyr::distinct(),
                     by = "FMWT_Macro")%>%
    dplyr::filter(!is.na(.data$Taxname))%>%
    dplyr::mutate(Taxlifestage=paste(.data$Taxname, .data$Lifestage), #create variable for combo taxonomy x life stage
                  Microcystis=dplyr::if_else(.data$Microcystis=="6", "2", .data$Microcystis), #Microsystis value of 6 only used from 2012-2015 and is equivalent to a 2 in other years, so just converting all 6s to 2s.
                  SampleID=paste(.data$Source, .data$Station, .data$SMSCG, .data$Datetime), #Create identifier for each sample
                  SizeClass="Macro",
                  Tide=as.character(.data$Tide))%>%
    dplyr::mutate(CPUE=dplyr::case_when(
      .data$CPUE!=0 ~ .data$CPUE,
      .data$CPUE==0 & .data$Date < .data$Intro ~ 0,
      .data$CPUE==0 & .data$Date >= .data$Intro & .data$Date < .data$FMWTstart ~ NA_real_,
      .data$CPUE==0 & .data$Date >= .data$FMWTstart & .data$Date < .data$FMWTend ~ 0,
      .data$CPUE==0 & .data$Date >= .data$FMWTend ~ NA_real_
    ))%>%
    dplyr::filter(!is.na(.data$CPUE))%>%
    dplyr::select(-.data$FMWT_Macro, -.data$FMWTstart, -.data$FMWTend, -.data$Intro, -.data$SMSCG)%>% #Remove FMWT taxa codes
    dplyr::mutate(Source=dplyr::recode(.data$Source, STN="TNS"))%>%
    {if(!("FMWT_Macro"%in%Data_sets)){
      dplyr::filter(., .data$Source != "FMWT")
    } else{
      .
    }}%>%
    {if(!("TNS_Macro"%in%Data_sets)){
      dplyr::filter(., .data$Source != "TNS")
    } else{
      .
    }}
}

# Combine data ----------------------------------------

zoop<-dplyr::bind_rows(data.list)%>% # Combine data
  dplyr::filter(!is.na(.data$Taxname))%>% #Remove NA taxnames (should only correspond to previously summed "all" categories from input datasets)
  dplyr::mutate(SalSurf= wql::ec2pss(.data$CondSurf/1000, t=25),
                SalBott=wql::ec2pss(.data$CondBott/1000, t=25),
                Year=lubridate::year(.data$Date))%>%
  dplyr::left_join(stations, by=c("Source", "Station"))%>% #Add lat and long
  dplyr::select(-.data$Region, -.data$CondBott, -.data$CondSurf)%>% #Remove some extraneous variables to save memory
  dplyr::mutate(Tide=dplyr::recode(.data$Tide, "1"="High slack", "2"="Ebb", "3"="Low slack", "4"="Flood", "1=high slack"="High slack", "2=ebb"="Ebb", "3=low slack"="Low slack", "4=flood"="Flood"))#Rename tide codes to be consistent

zoopEnv<-zoop%>%
  dplyr::select(-.data$SizeClass, -.data$Volume, -.data$Lifestage, -.data$Taxname, -.data$Phylum, -.data$Class, -.data$Order, -.data$Family, -.data$Genus, -.data$Species, -.data$Taxlifestage, -.data$CPUE)%>%
  dplyr::distinct()

zoop<-zoop%>%
  dplyr::select(.data$Source, .data$SizeClass, .data$Volume, .data$Lifestage, .data$Taxname, .data$Phylum, .data$Class, .data$Order, .data$Family, .data$Genus, .data$Species, .data$Taxlifestage, .data$SampleID, .data$CPUE)

if(Save_object){
  saveRDS(zoop, file=paste0(Zoop_path, ".Rds"))
  saveRDS(zoopEnv, file=paste0(Env_path, ".Rds"))
}

if(Return_object){
  if(Return_object_type=="Combined"){
    zoop_full <- dplyr::left_join(zoop, dplyr::select(zoopEnv, -.data$Source), by="SampleID")
    return(zoop_full)
  }
  if(Return_object_type=="List"){
    return(list(Zooplankton = zoop, Environment = zoopEnv))
  }
}

}
