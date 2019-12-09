#' Downloads and combines zooplankton datasets from the Sacramento San Joaquin Delta
#'
#' This function downloads datasets from the internet, converts them to a consistent format, binds them together, and exports the combined dataset as .Rds R data files and/or an R object.
#' @param Data_folder Path to folder in which source datasets are stored, and to which you would like datasets to be downloaded if you set `Redownload_data = TRUE`. If you do not want to store every source dataset, you can set this to \code{tempdir()}.
#' @param Save_object Should the combined data be saved to disk? Defaults to \code{Save_object = TRUE}.
#' @param Return_object Should data be returned as an R object? If \code{TRUE}, the function will return the full combined dataset. Defaults to `Return_object = FALSE`.
#' @param Return_object_type If \code{Return_object = TRUE}, should data be returned as a combined dataframe (\code{Return_object_type = "Combined"}) or a list with component "Zooplankton" containing the zooplankton data and component "Environment" containing the environmental data (\code{Return_object_type = "List"}, the default). A list is required to feed data into the \code{Zoopsynther} function without saving the combined dataset to disk.
#' @param Redownload_data Should source datasets be redownloaded from the internet? Defaults to \code{Redownload_data = FALSE}.
#' @param Zoop_path File path specifying the folder and filename of the zooplankton dataset. Defaults to \code{Zoop_path = file.path(Data_folder, "zoopforzooper")}.
#' @param Env_path File path specifying the folder and filename of the dataset with accessory environmental parameters. Defaults to \code{Env_path = file.path(Data_folder, "zoopenvforzooper")}.
#' @keywords download, integration, synthesis, zooplankton
#' @import data.table
#' @importFrom magrittr %>%
#' @return If \code{Return_object = TRUE}, returns the combined dataset as a list or tibble, depending on whether \code{Return_object_type} is set to \code{"List"} or \code{"Combined"}. If \code{Save_object = TRUE}, writes 2 .Rds files to disk: one with the zooplankton catch data and another with accessory environmental parameters.
#' @author Sam Bashevkin
#' @examples
#' Data <- Zoopdownloader(Data_folder = tempdir(), Return_object = TRUE, Save_object = FALSE)
#' @seealso \code{\link{Zoopsynther}}, \code{\link{crosswalk}}, \code{\link{stations}}, \code{\link{zooper}}
#' @export

Zoopdownloader <- function(
  Data_folder = "data",
  Save_object = TRUE,
  Return_object = FALSE,
  Return_object_type="List",
  Redownload_data=FALSE,
  Zoop_path = file.path(Data_folder, "zoopforzooper"),
  Env_path = file.path(Data_folder, "zoopenvforzooper")){

  # Setup -------------------------------------------------------------------

  # Load crosswalk key to convert each dataset's taxonomic codes to a
  # unified set of "Taxname" and "Lifestage" values.

  crosswalk <- crosswalk%>%
    dplyr::mutate_at(dplyr::vars(c("EMPstart", "EMPend", "Intro", "FMWTstart", "FMWTend", "twentymmstart", "twentymmend", "twentymmstart2")), ~readr::parse_date(as.character(.), format="%Y"))%>%
    dplyr::mutate_at(dplyr::vars(c("EMPstart", "FMWTstart", "twentymmstart", "twentymmstart2", "EMPend", "FMWTend", "twentymmend")), ~tidyr::replace_na(., lubridate::as_date(Inf)))%>% #Change any NAs for starts or ends to Infinity (i.e. never started or ended)
    dplyr::mutate(EMPend = dplyr::if_else(is.finite(EMPend), EMPend+lubridate::years(1), EMPend))%>% #Change end dates to beginning of next year (first day it was not counted)
    dplyr::mutate(FMWTend = dplyr::if_else(is.finite(FMWTend), FMWTend+lubridate::years(1), FMWTend))%>% #Change end dates to beginning of next year (first day it was not counted)
    dplyr::mutate(twentymmend = dplyr::if_else(is.finite(twentymmend), twentymmend+lubridate::years(1), twentymmend))%>% #Change end dates to beginning of next year (first day it was not counted)
    dplyr::mutate(Intro=tidyr::replace_na(Intro, lubridate::as_date(-Inf))) #Change any NAs in Intro date to -Inf (i.e., always been around)

  # Load station key to later incorporate latitudes and longitudes

  stations <- stations

  # Initialize list of dataframes

  data.list<-list()



  # EMP Meso ---------------------------------------------------------------------


  #download the file
  if (!file.exists(file.path(Data_folder, "1972-2018CBMatrix.xlsx")) | Redownload_data) {
    download.file("ftp://ftp.wildlife.ca.gov/IEP_Zooplankton/1972-2018CBMatrix.xlsx",
                  file.path(Data_folder, "1972-2018CBMatrix.xlsx"), mode="wb")
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
    dplyr::mutate(Datetime=suppressWarnings(lubridate::parse_date_time(paste(Date, Time), "%Y-%m-%d %H:%M", tz="America/Los_Angeles")))%>% #create a variable for datetime
    tidyr::pivot_longer(cols=c(-SurveyCode, -Year, -Survey, -SurveyRep,
                               -Date, -Station, -EZStation, -DWRStation,
                               -Core, -Region, -Secchi, -`Chl-a`, -Temperature,
                               -ECSurfacePreTow, -ECBottomPreTow, -CBVolume, -Datetime, -Time, -TideCode),
                        names_to="EMP_Meso", values_to="CPUE")%>% #transform from wide to long
    dplyr::mutate(Source="EMP",
                  SizeClass="Meso")%>% #add variable for data source
    dplyr::select(Source, Year, Date, Datetime, Tide=TideCode,
                  Station, Region, Chl=`Chl-a`, CondBott = ECBottomPreTow, CondSurf = ECSurfacePreTow, Secchi, SizeClass,
                  Temperature, Volume = CBVolume, EMP_Meso, CPUE)%>% #Select for columns in common and rename columns to match
    dplyr::left_join(crosswalk%>% #Add in Taxnames, Lifestage, and taxonomic info
                       dplyr::select(EMP_Meso, Lifestage, Taxname, Phylum, Class, Order, Family, Genus, Species, Intro, EMPstart, EMPend)%>% #only retain EMP codes
                       dplyr::filter(!is.na(EMP_Meso))%>% #Only retain Taxnames corresponding to EMP codes
                       dplyr::distinct(),
                     by="EMP_Meso")%>%
    dplyr::filter(!is.na(Taxname))%>% #Should remove all the summed categories in original dataset
    dplyr::mutate(Taxlifestage=paste(Taxname, Lifestage), #create variable for combo taxonomy x life stage
                  SampleID=paste(Source, Station, Date))%>% #Create identifier for each sample
    dplyr::mutate(CPUE=dplyr::case_when(
      CPUE!=0 ~ CPUE,
      CPUE==0 & Date < Intro ~ 0,
      CPUE==0 & Date >= Intro & Date < EMPstart ~ NA_real_,
      CPUE==0 & Date >= EMPstart & Date < EMPend ~ 0,
      CPUE==0 & Date >= EMPend ~ NA_real_
    ))%>%
    dplyr::select(-EMP_Meso, -EMPstart, -EMPend, -Intro)%>% #Remove EMP taxa codes
    dplyr::select(-Datetime)%>% #Add this back in when other EMP data have time
    dtplyr::lazy_dt()%>% #Speed up code using dtplyr package that takes advantage of data.table speed
    dplyr::group_by_at(dplyr::vars(-CPUE))%>%
    dplyr::summarise(CPUE=sum(CPUE, na.rm=T))%>% #Some taxa now have the same names (e.g., CYCJUV and OTHCYCJUV)
    #so we now add those categories together.
    dplyr::ungroup()%>%
    tibble::as_tibble() #required to finish operation after lazy_dt()


  # FMWT Meso --------------------------------------------------------------------

  #download the file
  if (!file.exists(file.path(Data_folder, "FMWT_TNSZooplanktonDataCPUEOct2017.xls")) | Redownload_data) {
    download.file("ftp://ftp.wildlife.ca.gov/TownetFallMidwaterTrawl/Zoopl_TownetFMWT/FMWT%20TNSZooplanktonDataCPUEOct2017.xls",
                  file.path(Data_folder, "FMWT_TNSZooplanktonDataCPUEOct2017.xls"), mode="wb")
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
    dplyr::mutate(Datetime=suppressWarnings(lubridate::parse_date_time(paste(Date, Time), "%Y-%m-%d %H:%M", tz="America/Los_Angeles")))%>% #create a variable for datetime
    tidyr::pivot_longer(cols=c(-Project, -Year, -Survey, -Month, -Date, -Datetime,
                               -Station, -Index, -Time, -TowDuration,
                               -Region, -FLaSHRegionGroup, -TideCode,
                               -DepthBottom, -CondSurf, -PPTSurf,
                               -SurfSalinityGroup, -CondBott, -PPTBott,
                               -TempSurf, -Secchi, -Turbidity, -Microcystis,
                               -TotalMeter, -Volume),
                        names_to="FMWT_Meso", values_to="CPUE")%>% #transform from wide to long
    dplyr::select(Source=Project, Year, Date, Datetime, Station, Region, Tide=TideCode, BottomDepth=DepthBottom, CondSurf, CondBott, Temperature = TempSurf, Secchi, Turbidity, Microcystis, Volume, FMWT_Meso, CPUE)%>% #Select for columns in common and rename columns to match
    dplyr::left_join(crosswalk%>% #Add in Taxnames, Lifestage, and taxonomic info
                       dplyr::select(FMWT_Meso, Lifestage, Taxname, Phylum, Class, Order, Family, Genus, Species, Intro, FMWTstart, FMWTend)%>% #only retain FMWT codes
                       dplyr::filter(!is.na(FMWT_Meso))%>% #Only retain Taxnames corresponding to FMWT codes
                       dplyr::distinct(),
                     by = "FMWT_Meso")%>%
    dplyr::filter(!is.na(Taxname))%>%
    dplyr::mutate(Taxlifestage=paste(Taxname, Lifestage), #create variable for combo taxonomy x life stage
                  Microcystis=dplyr::if_else(Microcystis=="6", "2", Microcystis), #Microsystis value of 6 only used from 2012-2015 and is equivalent to a 2 in other years, so just converting all 6s to 2s.
                  SampleID=paste(Source, Station, Datetime),
                  SizeClass="Meso")%>% #Create identifier for each sample
    dplyr::ungroup()%>%
    dplyr::mutate(CPUE=dplyr::case_when(
      CPUE!=0 ~ CPUE,
      CPUE==0 & Date < Intro ~ 0,
      CPUE==0 & Date >= Intro & Date < FMWTstart ~ NA_real_,
      CPUE==0 & Date >= FMWTstart & Date < FMWTend ~ 0,
      CPUE==0 & Date >= FMWTend ~ NA_real_
    ))%>%
    dplyr::filter(!is.na(CPUE))%>%
    dplyr::select(-FMWT_Meso, -FMWTstart, -FMWTend, -Intro) #Remove FMWT taxa codes


  # twentymm Meso ----------------------------------------------------------------

  #download the file
  if (!file.exists(file.path(Data_folder, "CDFW 20-mm Zooplankton Catch Matrix.xlsx")) | Redownload_data) {
    download.file("ftp://ftp.dfg.ca.gov/Delta%20Smelt/20mm%20Zooplankton%20Catch%20Matrix_1995-2017.xlsx",
                  file.path(Data_folder, "CDFW 20-mm Zooplankton Catch Matrix.xlsx"), mode="wb")
  }

  # Import and modify 20mm data

  zoo_20mm_Meso<-readxl::read_excel(file.path(Data_folder, "CDFW 20-mm Zooplankton Catch Matrix.xlsx"),
                                    sheet="20-mm CB CPUE Data",
                                    col_types = c("date", rep("numeric", 3), "date", rep("numeric", 80)))

  data.list[["twentymm_Meso"]]<-zoo_20mm_Meso%>%
    dplyr::mutate(SampleID = paste(Station, SampleDate, TowNum),
                  Datetime=lubridate::parse_date_time(paste0(SampleDate, " ", lubridate::hour(TowTime), ":", lubridate::minute(TowTime)), "%Y-%m-%d %%H:%M", tz="America/Los_Angeles"))%>%
    tidyr::pivot_longer(cols=c(-SampleDate, -Survey, -Station, -TowTime, -Temp, -TopEC,
                               -BottomEC, -Secchi, -Turbidity, -Tide, -BottomDepth, -Duration, -MeterCheck, -Volume,
                               -Dilution, -SampleID, -Datetime),
                        names_to="twentymm_Meso", values_to="CPUE")%>% #transform from wide to long
    dplyr::select(Date=SampleDate, Station, Temperature=Temp, CondSurf=TopEC, CondBott=BottomEC, Secchi,
                  Turbidity, Tide, BottomDepth, Volume, SampleID, Datetime, twentymm_Meso, CPUE)%>% #Select for columns in common and rename columns to match
    dplyr::left_join(crosswalk%>% #Add in Taxnames, Lifestage, and taxonomic info
                       dplyr::select(twentymm_Meso, Lifestage, Taxname, Phylum, Class, Order, Family, Genus, Species, Intro, twentymmstart, twentymmend, twentymmstart2)%>% #only retain FMWT codes
                       dplyr::filter(!is.na(twentymm_Meso))%>% #Only retain Taxnames corresponding to FMWT codes
                       dplyr::distinct(),
                     by = "twentymm_Meso")%>%
    dplyr::filter(!is.na(Taxname))%>%
    dplyr::mutate(Source="twentymm",
                  SizeClass="Meso",
                  Station=as.character(Station),
                  Tide=as.character(Tide),
                  Taxlifestage=paste(Taxname, Lifestage))%>% #add variable for data source, create variable for combo taxonomy x life stage
    dplyr::mutate(CPUE=dplyr::case_when(
      CPUE!=0 ~ CPUE,
      CPUE==0 & Date < Intro ~ 0,
      CPUE==0 & Date >= Intro & Date < twentymmstart ~ NA_real_,
      CPUE==0 & Date >= twentymmstart & Date < twentymmend ~ 0,
      CPUE==0 & Date >= twentymmend & Date < twentymmstart2 ~ NA_real_,
      CPUE==0 & Date >= twentymmstart2 ~ 0 #20mm dataset had one case of a taxa starting, ending, and starting again
    ))%>%
    dplyr::select(-twentymmend, -twentymmstart, -twentymmstart2, -Intro, -twentymm_Meso)%>%
    dtplyr::lazy_dt()%>% #Speed up
    dplyr::group_by_at(dplyr::vars(-CPUE))%>% #Some taxa names are repeated as in EMP so
    dplyr::summarise(CPUE=sum(CPUE, na.rm=T))%>% #this just adds up those duplications
    dplyr::ungroup()%>%
    dplyr::mutate(SampleID=paste(Source, SampleID))%>% #Create identifier for each sample
    tibble::as_tibble()


  # FRP Meso ---------------------------------------------------------------------

  # Import the FRP data

  #download the file
  if (!file.exists(file.path(Data_folder, "zoopsFRP2018.csv")) | Redownload_data) {
    download.file("https://portal.edirepository.org/nis/dataviewer?packageid=edi.269.2&entityid=d4c76f209a0653aa86bab1ff93ab9853",
                  file.path(Data_folder, "zoopsFRP2018.csv"), mode="wb")
  }

  zoo_FRP_Meso <- readr::read_csv(file.path(Data_folder, "zoopsFRP2018.csv"),
                                  col_types = "cctddddddddcccdddddc", na=c("", "NA"))

  #Already in long format
  data.list[["FRP_Meso"]] <- zoo_FRP_Meso%>%
    dplyr::mutate(Date=lubridate::parse_date_time(Date, "%m/%d/%Y", tz="America/Los_Angeles"))%>%
    dplyr::mutate(Station=dplyr::recode(Station, `Lindsey Tules`="Lindsey tules", LinBR="LinBr"))%>% #Rename inconsistent station names to match
    dplyr::mutate(Datetime=lubridate::parse_date_time(paste0(Date, " ", lubridate::hour(time), ":", lubridate::minute(time)), "%Y-%m-%d %%H:%M", tz="America/Los_Angeles"))%>% #Create a variable for datetime
    dplyr::mutate(Source="FRP",
                  SizeClass="Meso")%>% #add variable for data source
    dplyr::select(Source, Date, Datetime,
                  Station, CondSurf = SC, Secchi, pH, DO, Turbidity, Tide, Microcystis, SizeClass,
                  Temperature = Temp, Volume = volume, FRP_Meso = CommonName, CPUE, SampleID)%>% #Select for columns in common and rename columns to match
    dplyr::group_by_at(dplyr::vars(-CPUE))%>% #Some taxa names are repeated as in EMP so
    dplyr::summarise(CPUE=sum(CPUE, na.rm=T))%>% #this just adds up those duplications
    dplyr::ungroup()%>%
    tidyr::pivot_wider(names_from=FRP_Meso, values_from=CPUE, values_fill=list(CPUE=0))%>%
    tidyr::pivot_longer(cols=c(-Source, -Date, -Datetime,
                               -Station, -CondSurf, -Secchi, -pH, -DO, -Turbidity, -Tide, -Microcystis, -SizeClass,
                               -Temperature, -Volume, -SampleID),
                        names_to="FRP_Meso", values_to="CPUE")%>%
    dplyr::left_join(crosswalk%>% #Add in Taxnames, Lifestage, and taxonomic info
                       dplyr::select(FRP_Meso, Lifestage, Taxname, Phylum, Class, Order, Family, Genus, Species)%>% #only retain FRP codes
                       dplyr::filter(!is.na(FRP_Meso))%>% #Only retain Taxnames corresponding to FRP codes
                       dplyr::distinct(),
                     by = "FRP_Meso")%>%
    dplyr::mutate(Taxlifestage=paste(Taxname, Lifestage))%>% #create variable for combo taxonomy x life stage
    dplyr::select(-FRP_Meso)%>% #Remove FRP taxa codes
    dtplyr::lazy_dt()%>% #Speed up code
    dplyr::group_by_at(dplyr::vars(-CPUE))%>% #Some taxa names are repeated as in EMP so
    dplyr::summarise(CPUE=sum(CPUE, na.rm=T))%>% #this just adds up those duplications
    dplyr::ungroup()%>%
    dplyr::mutate(SampleID=paste(Source, SampleID))%>% #Create identifier for each sample
    tibble::as_tibble()



  # YBFMP Meso/Micro -------------------------------------------------------------


  #NO IDEA WHAT TO DO ABOUT INCONSISTENT TAXONOMIC RESOLUTION WITH NO DOCUMENTATION AND LACK OF LIFE STAGE INFORMATION

  #   zoo_YBFMP<-readr::read_csv(file.path(Data_folder, "yolo_zoop_public.csv"), col_types = "ctddcccdddddddccccccccccccccccccdd")

  #  data.list[["YBFMP"]]<-zoo_YBFMP%>%
  #    dplyr::mutate(SampleDate=lubridate::parse_date_time(SampleDate, "%m/%d/%Y", tz="America/Los_Angeles"))%>%
  #    dplyr::mutate(Datetime=lubridate::parse_date_time(paste(SampleDate, SampleTime), "%Y-%m-%d %H:%M:%S", tz="America/Los_Angeles"),
  #           Source="YBFMP",
  #           YBFMP=paste(TaxonName, LifeStage),
  #           SampleID=paste(SampleDate, StationCode))%>%
  #    select(SampleID, Date=SampleDate, Station=StationCode, Temperature=WaterTemperature, Secchi, Turbidity, CondSurf=Conductivity, SpCnd, pH, DO, YBFMP, Source, Datetime, NetSize, CPUE)%>%
  #    dplyr::left_join(crosswalk%>% #Add in Taxnames, Lifestage, and taxonomic info
  #                select(YBFMP, Lifestage, Taxname, Phylum, Class, Order, Family, Genus, Species)%>% #only retain YBFMP codes
  #                dplyr::filter(!is.na(YBFMP))%>% #Only retain Taxnames corresponding to YBFMP codes
  #                dplyr::distinct(),
  #              by = "YBFMP")%>%
  #    dplyr::mutate(Taxlifestage=paste(Taxname, Lifestage))%>% #create variable for combo taxonomy x life stage
  #    select(-YBFMP)%>% #Remove YBFMP taxa codes
  #    dtplyr::lazy_dt()%>% #Speed up code
  #    dplyr::group_by_at(dplyr::vars(-CPUE))%>% #In case some taxa names are repeated as in EMP so
  #    dplyr::summarise(CPUE=sum(CPUE, na.rm=T))%>% #this just adds up those duplications
  #    dplyr::ungroup()%>%
  #    dplyr::mutate(SampleID=paste(Source, SampleID))%>% #Create identifier for each sample
  #    tibble::as_tibble()


  # EMP Micro ---------------------------------------------------------------

  #download the file
  if (!file.exists(file.path(Data_folder, "1972-2018PumpMatrix.xlsx")) | Redownload_data) {
    download.file("ftp://ftp.dfg.ca.gov/IEP_Zooplankton/1972-2018Pump%20Matrix.xlsx",
                  file.path(Data_folder, "1972-2018PumpMatrix.xlsx"), mode="wb")
  }


  # Import the EMP data

  zoo_EMP_Micro <- readxl::read_excel(file.path(Data_folder, "1972-2018PumpMatrix.xlsx"),
                                      sheet = " Pump CPUE Matrix 1972-2018",
                                      col_types = c(rep("numeric", 4), "date", rep("text", 3), "numeric", "text", rep("numeric", 36)))

  # Tranform from "wide" to "long" format, add some variables,
  # alter data to match other datasets

  data.list[["EMP_Micro"]] <- zoo_EMP_Micro%>%
    dplyr::select(-Year, -SurveyCode, -Survey, -SurveyRep, -EZStation, -DWRStationNo, -Core, -Region)%>%
    dplyr::rename(OTHCYCADPUMP=OTHCYCAD)%>%
    tidyr::pivot_longer(cols=c(-SampleDate, -Station, -Secchi, -`Chl-a`, -Temperature,
                               -ECSurfacePreTow, -ECBottomPreTow, -PumpVolume),
                        names_to="EMP_Micro", values_to="CPUE")%>% #transform from wide to long
    dplyr::mutate(Source="EMP",
                  SizeClass="Micro")%>% #add variable for data source
    dplyr::select(Source, Date=SampleDate, Station, Chl=`Chl-a`, CondBott = ECBottomPreTow, CondSurf = ECSurfacePreTow, Secchi,
                  Temperature, SizeClass, Volume = PumpVolume, EMP_Micro, CPUE)%>% #Select for columns in common and rename columns to match
    dplyr::left_join(crosswalk%>% #Add in Taxnames, Lifestage, and taxonomic info
                       dplyr::select(EMP_Micro, Lifestage, Taxname, Phylum, Class, Order, Family, Genus, Species, Intro, EMPstart, EMPend)%>% #only retain EMP codes
                       dplyr::filter(!is.na(EMP_Micro))%>% #Only retain Taxnames corresponding to EMP codes
                       dplyr::distinct(),
                     by="EMP_Micro")%>%
    dplyr::filter(!is.na(Taxname))%>% #Should remove all the summed categories in original dataset
    dplyr::mutate(Taxlifestage=paste(Taxname, Lifestage), #create variable for combo taxonomy x life stage
                  SampleID=paste(Source, Station, Date),
                  Tide="1")%>% #Create identifier for each sample
    dplyr::mutate(CPUE=dplyr::case_when(
      CPUE!=0 ~ CPUE,
      CPUE==0 & Date < Intro ~ 0,
      CPUE==0 & Date >= Intro & Date < EMPstart ~ NA_real_,
      CPUE==0 & Date >= EMPstart & Date < EMPend ~ 0,
      CPUE==0 & Date >= EMPend ~ NA_real_
    ))%>%
    dplyr::select(-EMP_Micro, -EMPstart, -EMPend, -Intro)%>% #Remove EMP taxa codes
    dtplyr::lazy_dt()%>% #Speed up code using dtplyr package that takes advantage of data.table speed
    dplyr::group_by_at(dplyr::vars(-CPUE))%>%
    dplyr::summarise(CPUE=sum(CPUE, na.rm=T))%>% #Some taxa now have the same names (e.g., CYCJUV and OTHCYCJUV)
    #so we now add those categories together.
    dplyr::ungroup()%>%
    tibble::as_tibble() #required to finish operation after lazy_dt()


  # FRP Macro ---------------------------------------------------------------

  # Import the FRP data

  #download the file
  if (!file.exists(file.path(Data_folder, "bugsFRP2018.csv")) | Redownload_data) {
    download.file("https://portal.edirepository.org/nis/dataviewer?packageid=edi.269.2&entityid=630f16b33a9cbf75f1989fc18690a6b3",
                  file.path(Data_folder, "bugsFRP2018.csv"), mode="wb")
  }

  zoo_FRP_Macro <- readr::read_csv(file.path(Data_folder, "bugsFRP2018.csv"),
                                   col_types = "cctcddddddddccdddcddc", na=c("", "NA"))

  #Already in long format
  data.list[["FRP_Macro"]] <- zoo_FRP_Macro%>%
    dplyr::filter(Sampletype=="trawl")%>%
    dplyr::mutate(Date=lubridate::parse_date_time(Date, "%m/%d/%Y", tz="America/Los_Angeles"))%>%
    dplyr::mutate(Station=dplyr::recode(Station, `Lindsey Tules`="Lindsey tules", LinBR="LinBr", MINSLO1="MinSlo1", ProBR="ProBr", WinBR="WinBr"))%>% #Rename inconsistent station names to match
    dplyr::mutate(Datetime=lubridate::parse_date_time(paste0(Date, " ", lubridate::hour(time), ":", lubridate::minute(time)), "%Y-%m-%d %%H:%M", tz="America/Los_Angeles"))%>% #Create a variable for datetime
    dplyr::mutate(Source="FRP",
                  SizeClass="Macro",
                  CPUE=AdjCount/volume)%>% #add variable for data source and calculate CPUE
    dplyr::select(Source, Date, Datetime,
                  Station, CondSurf = SC, Secchi, pH, DO, Turbidity, Tide, Microcystis, SizeClass,
                  Temperature = Temp, Volume = volume, FRP_Macro = CommonName, CPUE, SampleID)%>% #Select for columns in common and rename columns to match
    dplyr::group_by_at(dplyr::vars(-CPUE))%>% #Some taxa names are repeated as in EMP so
    dplyr::summarise(CPUE=sum(CPUE, na.rm=T))%>% #this just adds up those duplications
    dplyr::ungroup()%>%
    tidyr::pivot_wider(names_from=FRP_Macro, values_from=CPUE, values_fill=list(CPUE=0))%>%
    tidyr::pivot_longer(cols=c(-Source, -Date, -Datetime,
                               -Station, -CondSurf, -Secchi, -pH, -DO, -Turbidity, -Tide, -Microcystis, -SizeClass,
                               -Temperature, -Volume, -SampleID),
                        names_to="FRP_Macro", values_to="CPUE")%>%
    dplyr::left_join(crosswalk%>% #Add in Taxnames, Lifestage, and taxonomic info
                       dplyr::select(FRP_Macro, Lifestage, Taxname, Phylum, Class, Order, Family, Genus, Species)%>% #only retain FRP codes
                       dplyr::filter(!is.na(FRP_Macro))%>% #Only retain Taxnames corresponding to FRP codes
                       dplyr::distinct(),
                     by = "FRP_Macro")%>%
    dplyr::mutate(Taxlifestage=paste(Taxname, Lifestage))%>% #create variable for combo taxonomy x life stage
    dplyr::select(-FRP_Macro)%>% #Remove FRP taxa codes
    dtplyr::lazy_dt()%>% #Speed up code
    dplyr::group_by_at(dplyr::vars(-CPUE))%>% #Some taxa names are repeated as in EMP so
    dplyr::summarise(CPUE=sum(CPUE, na.rm=T))%>% #this just adds up those duplications
    dplyr::ungroup()%>%
    dplyr::mutate(SampleID=paste(Source, SampleID))%>% #Create identifier for each sample
    tibble::as_tibble()


  # EMP Macro ---------------------------------------------------------------

  #download the file
  if (!file.exists(file.path(Data_folder, "1972-2018MysidMatrix.xlsx")) | Redownload_data) {
    download.file("ftp://ftp.dfg.ca.gov/IEP_Zooplankton/1972-2018MysidMatrix.xlsx",
                  file.path(Data_folder, "1972-2018MysidMatrix.xlsx"), mode="wb")
  }


  # Import the EMP data

  zoo_EMP_Macro <- readxl::read_excel(file.path(Data_folder, "1972-2018MysidMatrix.xlsx"),
                                      sheet = "Mysid CPUE Matrix 1972-2018 ",
                                      col_types = c(rep("numeric", 4), "date", rep("text", 3), "numeric", "text", rep("numeric", 14)))

  # Tranform from "wide" to "long" format, add some variables,
  # alter data to match other datasets

  data.list[["EMP_Macro"]] <- zoo_EMP_Macro%>%
    dplyr::select(-Year, -SurveyCode, -Survey, -SurveyRep, -EZStation, -DWRStation, -Core, -Region)%>%
    tidyr::pivot_longer(cols=c(-SampleDate, -Station, -Secchi, -`Chl-a`, -Temperature,
                               -ECSurfacePreTow, -ECBottomPreTow, -MysidVolume),
                        names_to="EMP_Macro", values_to="CPUE")%>% #transform from wide to long
    dplyr::mutate(Source="EMP",
                  SizeClass="Macro")%>% #add variable for data source
    dplyr::select(Source, Date=SampleDate, Station, Chl=`Chl-a`, CondBott = ECBottomPreTow, CondSurf = ECSurfacePreTow, Secchi, SizeClass,
                  Temperature, Volume = MysidVolume, EMP_Macro, CPUE)%>% #Select for columns in common and rename columns to match
    dplyr::left_join(crosswalk%>% #Add in Taxnames, Lifestage, and taxonomic info
                       dplyr::select(EMP_Macro, Lifestage, Taxname, Phylum, Class, Order, Family, Genus, Species, Intro, EMPstart, EMPend)%>% #only retain EMP codes
                       dplyr::filter(!is.na(EMP_Macro))%>% #Only retain Taxnames corresponding to EMP codes
                       dplyr::distinct(),
                     by="EMP_Macro")%>%
    dplyr::filter(!is.na(Taxname))%>% #Should remove all the summed categories in original dataset
    dplyr::mutate(Taxlifestage=paste(Taxname, Lifestage), #create variable for combo taxonomy x life stage
                  SampleID=paste(Source, Station, Date), #Create identifier for each sample
                  Tide="1")%>%
    dplyr::mutate(CPUE=dplyr::case_when(
      CPUE!=0 ~ CPUE,
      CPUE==0 & Date < Intro ~ 0,
      CPUE==0 & Date >= Intro & Date < EMPstart ~ NA_real_,
      CPUE==0 & Date >= EMPstart & Date < EMPend ~ 0,
      CPUE==0 & Date >= EMPend ~ NA_real_
    ))%>%
    dplyr::select(-EMP_Macro, -EMPstart, -EMPend, -Intro)%>% #Remove EMP taxa codes
    dtplyr::lazy_dt()%>% #Speed up code using dtplyr package that takes advantage of data.table speed
    dplyr::group_by_at(dplyr::vars(-CPUE))%>%
    dplyr::summarise(CPUE=sum(CPUE, na.rm=T))%>% #Some taxa now have the same names (e.g., CYCJUV and OTHCYCJUV)
    #so we now add those categories together.
    dplyr::ungroup()%>%
    tibble::as_tibble() #required to finish operation after lazy_dt()


  # FMWT Macro --------------------------------------------------------------

  #download the file
  if (!file.exists(file.path(Data_folder, "FMWT_TNSMysidCPUEJuly2019.xlsx")) | Redownload_data) {
    download.file("ftp://ftp.dfg.ca.gov/TownetFallMidwaterTrawl/Zoopl_TownetFMWT/FMWT%20TNSMysidCPUEJuly2019.xlsx",
                  file.path(Data_folder, "FMWT_TNSMysidCPUEJuly2019.xlsx"), mode="wb")
  }

  #download the file
  if (!file.exists(file.path(Data_folder, "FMWT_TNSAmphipodCPUEJuly2019.xls")) | Redownload_data) {
    download.file("ftp://ftp.dfg.ca.gov/TownetFallMidwaterTrawl/Zoopl_TownetFMWT/FMWT%20TNSAmphipodCPUEJuly2019.xls",
                  file.path(Data_folder, "FMWT_TNSAmphipodCPUEJuly2019.xls"), mode="wb")
  }

  zoo_FMWT_Macro_Mysid <- readxl::read_excel(file.path(Data_folder, "FMWT_TNSMysidCPUEJuly2019.xlsx"),
                                             sheet = "FMWT STN Mysid CPUE Matrix")

  zoo_FMWT_Macro_Amph <- readxl::read_excel(file.path(Data_folder, "FMWT_TNSAmphipodCPUEJuly2019.xls"),
                                            sheet = "FMWT STN amphipod CPUE")

  data.list[["FMWT_Macro"]] <- zoo_FMWT_Macro_Mysid%>%
    dplyr::rename(Date=SampleDate, `PPT Surface`=PPTSurf)%>%
    dplyr::mutate(Tax="Mysid")%>%
    dplyr::bind_rows(zoo_FMWT_Macro_Amph%>%
                       dplyr::mutate(Tax="Amph"))%>%
    dplyr::mutate(Datetime=lubridate::parse_date_time(paste0(Date, " ", lubridate::hour(Time), ":", lubridate::minute(Time)), "%Y-%m-%d %%H:%M", tz="America/Los_Angeles"),
                  Microcystis=as.character(Microcystis))%>% #create a variable for datetime
    tidyr::pivot_longer(cols=c(-Project, -Year, -Survey, -Date, -Datetime,
                               -Station, -Index, -SMSCG, -Time, -TowDuration,
                               -Region, -FLaSHRegionGroup, -TideCode,
                               -DepthBottom, -ConductivityTop, -`PPT Surface`,
                               -ConductivityBottom, -`PPT Bottom`,
                               -WaterTemperature, -Secchi, -Turbidity, -Microcystis,
                               -TotalMeter, -Volume, -Tax),
                        names_to="FMWT_Macro", values_to="CPUE")%>% #transform from wide to long
    dplyr::select(Source=Project, Date, Datetime, Station, SMSCG, Tide=TideCode, BottomDepth=DepthBottom, CondSurf=ConductivityTop, CondBott=ConductivityBottom, Temperature = WaterTemperature, Secchi, Turbidity, Microcystis, Volume, FMWT_Macro, CPUE)%>% #Select for columns in common and rename columns to match
    dplyr::left_join(crosswalk%>% #Add in Taxnames, Lifestage, and taxonomic info
                       dplyr::select(FMWT_Macro, Lifestage, Taxname, Phylum, Class, Order, Family, Genus, Species, Intro, FMWTstart, FMWTend)%>% #only retain FMWT codes
                       dplyr::filter(!is.na(FMWT_Macro))%>% #Only retain Taxnames corresponding to FMWT codes
                       dplyr::distinct(),
                     by = "FMWT_Macro")%>%
    dplyr::filter(!is.na(Taxname))%>%
    dplyr::mutate(Taxlifestage=paste(Taxname, Lifestage), #create variable for combo taxonomy x life stage
                  Microcystis=dplyr::if_else(Microcystis=="6", "2", Microcystis), #Microsystis value of 6 only used from 2012-2015 and is equivalent to a 2 in other years, so just converting all 6s to 2s.
                  SampleID=paste(Source, Station, SMSCG, Datetime), #Create identifier for each sample
                  SizeClass="Macro",
                  Tide=as.character(Tide))%>%
    dplyr::mutate(CPUE=dplyr::case_when(
      CPUE!=0 ~ CPUE,
      CPUE==0 & Date < Intro ~ 0,
      CPUE==0 & Date >= Intro & Date < FMWTstart ~ NA_real_,
      CPUE==0 & Date >= FMWTstart & Date < FMWTend ~ 0,
      CPUE==0 & Date >= FMWTend ~ NA_real_
    ))%>%
    dplyr::filter(!is.na(CPUE))%>%
    dplyr::select(-FMWT_Macro, -FMWTstart, -FMWTend, -Intro, -SMSCG)%>% #Remove FMWT taxa codes
    dplyr::mutate(Source=dplyr::recode(Source, STN="TNS"))

  # Combine data ----------------------------------------

  zoop<-dplyr::bind_rows(data.list)%>% # Combine data
    dplyr::filter(!is.na(Taxname))%>% #Remove NA taxnames (should only correspond to previously summed "all" categories from input datasets)
    dplyr::mutate(SalSurf=((0.36966/(((CondSurf*0.001)^(-1.07))-0.00074))*1.28156),
                  SalBott=((0.36966/(((CondBott*0.001)^(-1.07))-0.00074))*1.28156),#Convert conductivity to salinity using formula in FMWT metadata
                  Year=lubridate::year(Date))%>%
    dplyr::left_join(stations, by=c("Source", "Station"))%>% #Add lat and long
    dplyr::select(-Region, -CondBott, -CondSurf)%>% #Remove some extraneous variables to save memory
    dplyr::mutate(Tide=dplyr::recode(Tide, "1"="High slack", "2"="Ebb", "3"="Low slack", "4"="Flood", "1=high slack"="High slack", "2=ebb"="Ebb", "3=low slack"="Low slack", "4=flood"="Flood"))#Rename tide codes to be consistent

  zoopEnv<-zoop%>%
    dplyr::select(Source, Year, Date, Datetime, Tide, Station, Chl, Secchi, Temperature, BottomDepth, Turbidity, Microcystis, pH, DO, SalSurf, SalBott, Latitude, Longitude, SampleID)%>%
    dplyr::distinct()

  zoop<-zoop%>%
    dplyr::select(-Year, -Date, -Datetime, -Tide, -Station, -Chl, -Secchi, -Temperature, -BottomDepth, -Turbidity, -Microcystis, -pH, -DO, -SalSurf, -SalBott, -Latitude, -Longitude)

  if(Save_object){
    saveRDS(zoop, file=paste0(Zoop_path, ".Rds"))
    saveRDS(zoopEnv, file=paste0(Env_path, ".Rds"))
  }

  if(Return_object){
    if(Return_object_type=="Combined"){
    zoop_full <- dplyr::left_join(zoop, dplyr::select(zoopEnv, -Source), by="SampleID")
    return(zoop_full)
    }
    if(Return_object_type=="List"){
      return(list(Zooplankton=zoop, Environment=zoopEnv))
    }
  }

}
