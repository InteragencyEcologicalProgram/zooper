#' Downloads and combines zooplankton datasets collected by the Interagency Ecological Program from the Sacramento-San Joaquin Delta
#'
#' This function downloads all IEP zooplankton datasets from the internet, converts them to a consistent format, binds them together, and exports the combined dataset as .Rds R data files and/or an R object. Datasets currently include "EMP" (Environmental Monitoring Program), "FRP" (Fish Restoration Program), "FMWT" (Fall Midwater Trawl), "STN" (Townet Survey), and "20mm" (20mm survey).
#' @param Data_sets Datasets to include in combined data. Choices include "EMP_Meso", "FMWT_Meso", "STN_Meso", "20mm_Meso", "FRP_Meso", "YBFMP_Meso", "EMP_Micro", "YBFMP_Micro", "FRP_Macro", "EMP_Macro", "FMWT_Macro", "STN_Macro"., "DOP_Macro", and "DOP_Meso". Defaults to including all datasets except the two YBFMP datasets.
#' @param Data_folder Path to folder in which source datasets are stored, and to which you would like datasets to be downloaded if you set \code{Redownload_data = TRUE}. If you do not want to store every source dataset, you can leave this at the default \code{tempdir()}. If you do not wish to redownload these datasets every time you run the function, you can set this to a directory on your computer and run the function in the future with \code{Redownload_data = FALSE}, which will load the source datasets from \code{Data_folder} instead of downloading them again.
#' @param Save_object Should the combined data be saved to disk? Defaults to \code{Save_object = TRUE}.
#' @param Return_object Should data be returned as an R object? If \code{TRUE}, the function will return the full combined dataset. Defaults to `Return_object = FALSE`.
#' @param Return_object_type If \code{Return_object = TRUE}, should data be returned as a combined dataframe (\code{Return_object_type = "Combined"}) or a list with component "Zooplankton" containing the zooplankton data and component "Environment" containing the environmental data (\code{Return_object_type = "List"}, the default). A list is required to feed data into the \code{Zoopsynther} function without saving the combined dataset to disk.
#' @param Redownload_data Should source datasets be redownloaded from the internet? Defaults to \code{Redownload_data = FALSE}.
#' @param Download_method Method used to download files. See argument \code{method} options in \code{\link[utils]{download.file}}. Defaults to "curl".
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
#' @details Note that EMP Macro samples with QAQC flags (any value of AmphipodCode other than "A") have had their Amphipod CPUE set to NA in this function. For more information on the source datasets see \code{\link{zooper}}.
#' @examples
#' \dontrun{
#' Data <- Zoopdownloader(Data_folder = tempdir(), Return_object = TRUE,
#' Save_object = FALSE, Redownload_data = TRUE)
#' }
#' @seealso \code{\link{Zoopsynther}}, \code{\link{crosswalk}}, \code{\link{stations}}, \code{\link{zooper}}
#' @export

Zoopdownloader <- function(
  Data_sets = c("EMP_Meso", "FMWT_Meso", "STN_Meso",
                "20mm_Meso", "FRP_Meso", "EMP_Micro",
                "FRP_Macro", "EMP_Macro", "FMWT_Macro", "STN_Macro", "DOP_Meso", "DOP_Macro"),
  Data_folder = tempdir(),
  Save_object = TRUE,
  Return_object = FALSE,
  Return_object_type = "List",
  Redownload_data = FALSE,
  Download_method="auto",
  Zoop_path = file.path(Data_folder, "zoopforzooper"),
  Env_path = file.path(Data_folder, "zoopenvforzooper"),
  Crosswalk = zooper::crosswalk,
  Stations = zooper::stations){

  # Setup -------------------------------------------------------------------
  where <- utils::getFromNamespace("where", "tidyselect")

  # Check arguments

  if (!purrr::every(Data_sets, ~.%in%c("EMP_Meso", "FMWT_Meso", "STN_Meso",
                                       "20mm_Meso", "FRP_Meso","EMP_Micro",
                                       "FRP_Macro", "EMP_Macro", "FMWT_Macro",
                                       "STN_Macro", "YBFMP_Meso", "YBFMP_Micro",
                                       "DOP_Meso", "DOP_Macro"))){
    stop("Data_sets must contain one or more of the following options: 'EMP_Meso',
         'FMWT_Meso', 'STN_Meso', '20mm_Meso', 'FRP_Meso', 'EMP_Micro', 'FRP_Macro', 'EMP_Macro',
         'FMWT_Macro', 'STN_Macro', 'YBFMP_Meso', 'YBFMP_Micro', 'DOP_Macro', 'DOP_Meso'")
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


  # Find URLs ---------------------------------------------------------------

  if(any(c("EMP_Meso", "EMP_Macro", "EMP_Micro")%in%Data_sets)){
    EMP_revision_url <- "https://pasta.lternet.edu/package/eml/edi/522"
    EMP_latest_revision <- utils::tail(Tryer(n=3, fun=readLines, con=EMP_revision_url, warn = FALSE), 1)
    EMP_pkg_url <- paste0("https://pasta.lternet.edu/package/data/eml/edi/522/", EMP_latest_revision)
    EMP_entities <- Tryer(n=3, fun=readLines, con=EMP_pkg_url, warn = FALSE)
    EMP_name_urls <- paste("https://pasta.lternet.edu/package/name/eml/edi/522", EMP_latest_revision, EMP_entities, sep="/")
    names(EMP_entities) <- purrr::map_chr(EMP_name_urls, ~Tryer(n=3, fun=readLines, con=.x, warn = FALSE))

  }

  if(any(c("FMWT_Meso", "FMWT_Macro")%in%Data_sets)){
    FMWTSTN_revision_url <- "https://pasta.lternet.edu/package/eml/edi/1103"
    FMWTSTN_latest_revision <- utils::tail(Tryer(n=3, fun=readLines, con=FMWTSTN_revision_url, warn = FALSE), 1)
    FMWTSTN_pkg_url <- paste0("https://pasta.lternet.edu/package/data/eml/edi/1103/", FMWTSTN_latest_revision)
    FMWTSTN_entities <- Tryer(n=3, fun=readLines, con=FMWTSTN_pkg_url, warn = FALSE)
    FMWTSTN_name_urls <- paste("https://pasta.lternet.edu/package/name/eml/edi/1103", FMWTSTN_latest_revision, FMWTSTN_entities, sep="/")
    names(FMWTSTN_entities) <- purrr::map_chr(FMWTSTN_name_urls, ~Tryer(n=3, fun=readLines, con=.x, warn = FALSE))

  }


  if(any(c("STN_Meso", "STN_Macro")%in%Data_sets)){
    SMSCG_URL<-"https://filelib.wildlife.ca.gov/Public/TownetFallMidwaterTrawl/Zooplankton_SMSCG/"
    SMSCG_files<-html_file_list(SMSCG_URL)
  }

  if(any(c("20mm_Meso")%in%Data_sets)){
    twentymm_URL<-"https://filelib.wildlife.ca.gov/Public/Delta%20Smelt/"
    twentymm_files<-html_file_list(twentymm_URL)
  }

  if(any(c("YBFMP_Meso", "YBFMP_Micro")%in%Data_sets)){
    YBFMP_revision_url <- "https://pasta.lternet.edu/package/eml/edi/494"
    YBFMP_latest_revision <- utils::tail(Tryer(n=3, fun=readLines, con=YBFMP_revision_url, warn = FALSE), 1)
    YBFMP_pkg_url <- paste0("https://pasta.lternet.edu/package/data/eml/edi/494/", YBFMP_latest_revision)
    YBFMP_entities <- Tryer(n=3, fun=readLines, con=YBFMP_pkg_url, warn = FALSE)
    YBFMP_name_urls <- paste("https://pasta.lternet.edu/package/name/eml/edi/494", YBFMP_latest_revision, YBFMP_entities, sep="/")
    names(YBFMP_entities) <- purrr::map_chr(YBFMP_name_urls, ~Tryer(n=3, fun=readLines, con=.x, warn = FALSE))

  }

  if(any(c("DOP_Meso", "DOP_Macro")%in%Data_sets)){
    DOP_revision_url <- "https://pasta.lternet.edu/package/eml/edi/1187"
    DOP_latest_revision <- utils::tail(Tryer(n=3, fun=readLines, con=DOP_revision_url, warn = FALSE), 1)
    DOP_pkg_url <- paste0("https://pasta.lternet.edu/package/data/eml/edi/1187/", DOP_latest_revision)
    DOP_entities <- Tryer(n=3, fun=readLines, con=DOP_pkg_url, warn = FALSE)
    DOP_name_urls <- paste("https://pasta.lternet.edu/package/name/eml/edi/1187", DOP_latest_revision, DOP_entities, sep="/")
    names(DOP_entities) <- purrr::map_chr(DOP_name_urls, ~Tryer(n=3, fun=readLines, con=.x, warn = FALSE))

  }

  # EMP Meso ---------------------------------------------------------------------
  if("EMP_Meso"%in%Data_sets) {

    EMP_Meso_file<-"cb_matrix.csv"
    EMP_Meso_URL<-paste0(EMP_pkg_url, "/", EMP_entities[EMP_Meso_file])

    #download the file
    if (!file.exists(file.path(Data_folder, EMP_Meso_file)) | Redownload_data) {
      Tryer(n=3, fun=utils::download.file, url=EMP_Meso_URL,
            destfile=file.path(Data_folder, EMP_Meso_file), mode="wb", method=Download_method)
    }


    # Import the EMP data

    zoo_EMP_Meso<-readr::read_csv(file.path(Data_folder, EMP_Meso_file),
                                  col_types=readr::cols_only(SampleDate="c", Time="c", StationNZ="c",
                                                             Chl_a="d", Secchi="d", Temperature="d",
                                                             ECSurfacePreTow="d", ECBottomPreTow="d",
                                                             Volume="d", Depth="d", ACARTELA="d", ACARTIA="d",
                                                             DIAPTOM="d", EURYTEM="d", OTHCALAD="d",
                                                             PDIAPFOR="d", PDIAPMAR="d", SINOCAL="d",
                                                             TORTANUS="d", ACANTHO="d", LIMNOSPP="d",
                                                             LIMNOSINE="d", LIMNOTET="d", OITHDAV="d",
                                                             OITHSIM="d", OITHSPP="d", OTHCYCAD="d",
                                                             HARPACT="d", CALJUV="d", EURYJUV="d",
                                                             OTHCALJUV="d", PDIAPJUV="d", SINOCALJUV="d",
                                                             ASINEJUV="d", ACARJUV="d", DIAPTJUV="d",
                                                             TORTJUV="d", CYCJUV="d", LIMNOJUV="d",
                                                             OITHJUV="d", OTHCYCJUV="d", COPNAUP="d",
                                                             EURYNAUP="d", OTHCOPNAUP="d", PDIAPNAUP="d",
                                                             SINONAUP="d", BOSMINA="d", DAPHNIA="d",
                                                             DIAPHAN="d",OTHCLADO="d", ASPLANCH="d",
                                                             KERATELA="d",OTHROT="d", POLYARTH="d",
                                                             SYNCH="d",SYNCHBIC="d", TRICHO="d",
                                                             BARNNAUP="d", CRABZOEA="d"))

    # Tranform from "wide" to "long" format, add some variables,
    # alter data to match other datasets

    data.list[["EMP_Meso"]] <- zoo_EMP_Meso%>%
      dplyr::filter(!is.na(.data$SampleDate))%>%
      dplyr::mutate(SampleDate=lubridate::parse_date_time(.data$SampleDate, "%m/%d/%Y", tz="America/Los_Angeles"),
                    Datetime=lubridate::parse_date_time(dplyr::if_else(is.na(.data$Time), NA_character_, paste(.data$SampleDate, .data$Time)),
                                                        c("%Y-%m-%d %H:%M", "%Y-%m-%d %I:%M:%S %p"), tz="Etc/GMT+8"), #create a variable for datetime
                    Datetime=lubridate::with_tz(.data$Datetime, "America/Los_Angeles"))%>% # Ensure everything ends up in local time
      tidyr::pivot_longer(cols=c(-"SampleDate", -"StationNZ", -"Time", -"Secchi", -"Chl_a", -"Temperature",
                                 -"ECSurfacePreTow", -"ECBottomPreTow", -"Volume", -"Datetime", -"Depth"),
                          names_to="EMP_Meso", values_to="CPUE")%>% #transform from wide to long
      dplyr::mutate(Source="EMP",
                    SizeClass="Meso")%>% #add variable for data source
      dplyr::select("Source", Date="SampleDate", "Datetime",
                    Station="StationNZ", Chl = "Chl_a", CondBott = "ECBottomPreTow", CondSurf = "ECSurfacePreTow", "Secchi", "SizeClass",
                    "Temperature", "Volume", BottomDepth="Depth", "EMP_Meso", "CPUE")%>% #Select for columns in common and rename columns to match
      dplyr::left_join(Crosswalk%>% #Add in Taxnames, Lifestage, and taxonomic info
                         dplyr::select("EMP_Meso", "Lifestage", "Taxname", "Phylum", "Class", "Order", "Family", "Genus", "Species", "Intro", "EMPstart", "EMPend")%>% #only retain EMP codes
                         dplyr::filter(!is.na(.data$EMP_Meso))%>% #Only retain Taxnames corresponding to EMP codes
                         dplyr::distinct(),
                       by="EMP_Meso")%>%
      dplyr::filter(!is.na(.data$Taxname))%>% #Should remove all the summed categories in original dataset
      dplyr::mutate(Taxlifestage=paste(.data$Taxname, .data$Lifestage), #create variable for combo taxonomy x life stage
                    SampleID=paste(.data$Source, .data$Station, .data$Date), #Create identifier for each sample
                    Tide="1",# All EMP samples collected at high slack
                    BottomDepth=.data$BottomDepth*0.3048)%>% # Convert feet to meters
      dplyr::mutate(CPUE=dplyr::case_when(
        .data$CPUE!=0 ~ .data$CPUE,
        .data$CPUE==0 & .data$Date < .data$Intro ~ 0,
        .data$CPUE==0 & .data$Date >= .data$Intro & .data$Date < .data$EMPstart ~ NA_real_,
        .data$CPUE==0 & .data$Date >= .data$EMPstart & .data$Date < .data$EMPend ~ 0,
        .data$CPUE==0 & .data$Date >= .data$EMPend ~ NA_real_
      ))%>%
      dplyr::select(-"EMP_Meso", -"EMPstart", -"EMPend", -"Intro")%>% #Remove EMP taxa codes
      dplyr::select(-"Datetime")%>% #Add this back in when other EMP data have time
      dtplyr::lazy_dt()%>% #Speed up code using dtplyr package that takes advantage of data.table speed
      dplyr::group_by(dplyr::across(-"CPUE"))%>%
      dplyr::summarise(CPUE=sum(.data$CPUE, na.rm=TRUE))%>% #Some taxa now have the same names (e.g., CYCJUV and OTHCYCJUV) so we now add those categories together.
      dplyr::ungroup()%>%
      tibble::as_tibble() %>% #required to finish operation after lazy_dt()
      dplyr::left_join(stations, by=c("Source", "Station")) #Add lat and long


    cat("\nEMP_Meso finished!\n\n")
  }


  # DOP Meso ---------------------------------------------------------------------
  if("DOP_Meso"%in%Data_sets) {

    DOP_Meso_file<-"DOP_ICF_Mesozooplankton_Abundance2017-2021"
    DOP_Meso_URL<-paste0(DOP_pkg_url, "/", DOP_entities[DOP_Meso_file])

    DOP_trawls_file<-"DOP_ICF_TowData2017-2021"
    DOP_trawls_URL<-paste0(DOP_pkg_url, "/", DOP_entities[DOP_trawls_file])

    #download the files
    if (!file.exists(file.path(Data_folder, DOP_Meso_file)) | Redownload_data) {
      Tryer(n=3, fun=utils::download.file, url=DOP_Meso_URL,
            destfile=file.path(Data_folder, DOP_Meso_file), mode="wb", method=Download_method)
    }
    if (!file.exists(file.path(Data_folder, DOP_trawls_file)) | Redownload_data) {
      Tryer(n=3, fun=utils::download.file, url=DOP_trawls_URL,
            destfile=file.path(Data_folder, DOP_trawls_file), mode="wb", method=Download_method)
    }


    # Import the DOP data

    zoo_DOP_Meso<-readr::read_csv(file.path(Data_folder, DOP_Meso_file))
    zoo_DOP_trawls<-readr::read_csv(file.path(Data_folder, DOP_trawls_file))

    # Tranform from "wide" to "long" format, add some variables,
    # alter data to match other datasets

    data.list[["DOP_Meso"]] <- zoo_DOP_Meso %>%
      tidyr::pivot_longer(cols = !.data$ICF_ID, names_to = "DOP_Meso", values_to = "CPUE") %>%
      dplyr::left_join(zoo_DOP_trawls) %>%
      dplyr::mutate(Datetime =  lubridate::ymd(as.character(.data$Date), tz = "America/Los_Angeles") + lubridate::hms(as.character(.data$Start_Time)),
              Source = "DOP", #add variable for data source
              SizeClass = "Meso") %>%
      dplyr::filter(!is.na(.data$Mesozooplankton_Volume)) %>% #get rid of environmental variables with no data

      #Select variables we are interested in.
      dplyr::select(.data$Source, .data$Date, .data$Datetime,
                    Station = .data$Station_Code, Chl = .data$Chl_a, CondSurf = .data$Conductivity, .data$Secchi, .data$SizeClass,
                    .data$Temperature, Volume = .data$Mesozooplankton_Volume, BottomDepth = .data$Start_Depth,
                    .data$DOP_Meso, .data$CPUE, .data$Latitude, .data$Longitude, .data$ICF_ID) %>%
      dplyr::left_join(Crosswalk %>% #Add in Taxnames, Lifestage, and taxonomic info
                       dplyr::select(.data$DOP_Meso, .data$Lifestage, .data$Taxname, .data$Phylum,
                                     .data$Class, .data$Order, .data$Family, .data$Genus, .data$Species,
                                     .data$DOPstart, .data$DOPend, .data$Intro)%>% #only retain dop codes
                         dplyr::filter(!is.na(.data$DOP_Meso))%>% #Only retain Taxnames corresponding to EMP codes
                         dplyr::distinct(),
                       by="DOP_Meso")%>%
      dplyr::filter(!is.na(.data$Taxname), !is.na(.data$CPUE)) %>%  #get rid of the lines with "NA" because the critter wasn't counted in this sample.
      dplyr::mutate(Taxlifestage=paste(.data$Taxname, .data$Lifestage), #create variable for combo taxonomy x life stage
                    SampleID=paste(.data$Source, .data$Station, .data$Date, .data$ICF_ID), #Create identifier for each sample
                    BottomDepth=.data$BottomDepth*0.3048)%>% # Convert feet to meters
      dplyr::mutate(CPUE=dplyr::case_when(
        .data$CPUE!=0 ~ .data$CPUE,
        .data$CPUE==0 & .data$Date < .data$Intro ~ 0,
        .data$CPUE==0 & .data$Date >= .data$Intro & .data$Date < .data$DOPstart ~ NA_real_,
        .data$CPUE==0 & .data$Date >= .data$DOPstart & .data$Date < .data$DOPend ~ 0,
        .data$CPUE==0 & .data$Date >= .data$DOPend ~ NA_real_)) %>%
      dplyr::filter(!is.na(CPUE)) %>%
   dplyr::select(-.data$DOP_Meso, -.data$ICF_ID, -.data$DOPstart, -.data$DOPend, -.data$Intro) #Remove DOP code
    cat("\nDOP_Meso finished!\n\n")

  }


# DOP Macro ---------------------------------------------------------------------
  if("DOP_Macro"%in%Data_sets) {

    DOP_Macro_file<-"DOP_ICF_Macrozooplankton_Abundance2017-2021"
    DOP_Macro_URL<-paste0(DOP_pkg_url, "/", DOP_entities[DOP_Macro_file])

    DOP_trawls_file<-"DOP_ICF_TowData2017-2021"
    DOP_trawls_URL<-paste0(DOP_pkg_url, "/", DOP_entities[DOP_trawls_file])

    #download the files
    if (!file.exists(file.path(Data_folder, DOP_Macro_file)) | Redownload_data) {
      Tryer(n=3, fun=utils::download.file, url=DOP_Macro_URL,
            destfile=file.path(Data_folder, DOP_Macro_file), mode="wb", method=Download_method)
    }
    if (!file.exists(file.path(Data_folder, DOP_trawls_file)) | Redownload_data) {
      Tryer(n=3, fun=utils::download.file, url=DOP_trawls_URL,
            destfile=file.path(Data_folder, DOP_trawls_file), mode="wb", method=Download_method)
    }


    # Import the DOP data

    zoo_DOP_Macro<-readr::read_csv(file.path(Data_folder, DOP_Macro_file))
    zoo_DOP_trawls<-readr::read_csv(file.path(Data_folder, DOP_trawls_file))

    # Tranform from "wide" to "long" format, add some variables,
    # alter data to match other datasets

    data.list[["DOP_Macro"]] <- zoo_DOP_Macro %>%
      tidyr::pivot_longer(cols = !.data$ICF_ID, names_to = "DOP_Macro", values_to = "CPUE") %>%
      dplyr::left_join(zoo_DOP_trawls) %>%
      dplyr::filter(!is.na(.data$Macrozooplankton_Volume)) %>%
      dplyr::mutate( Datetime =  lubridate::ymd(as.character(.data$Date), tz = "America/Los_Angeles") + lubridate::hms(as.character(.data$Start_Time)), #create a variable for datetime
                     #Datetime=lubridate::with_tz(.data$Datetime, "America/Los_Angeles"),
                     Source = "DOP", #add variable for data source
                     SizeClass = "Macro") %>%

      #Select variables we are interested in. I need to check on the latitude/longitude issue with Sam.
      dplyr::select(.data$Source, .data$Date, .data$Datetime,
                    Station = .data$Station_Code, Chl = .data$Chl_a, CondSurf = .data$Conductivity, .data$Secchi, .data$SizeClass,
                    .data$Temperature, Volume = .data$Macrozooplankton_Volume, BottomDepth = .data$Start_Depth, .data$ICF_ID,
                    .data$DOP_Macro, .data$CPUE, .data$Latitude, .data$Longitude) %>%
      dplyr::left_join(Crosswalk %>% #Add in Taxnames, Lifestage, and taxonomic info
                         dplyr::select(.data$DOP_Macro, .data$Lifestage, .data$Taxname, .data$Phylum,
                                       .data$Class, .data$Order, .data$Family, .data$Genus, .data$Species,
                                        .data$DOPstart, .data$DOPend, .data$Intro)%>% #only retain dop codes
                         dplyr::filter(!is.na(.data$DOP_Macro))%>% #Only retain Taxnames corresponding to EMP codes
                         dplyr::distinct(),
                       by="DOP_Macro")%>%
      dplyr::filter(!is.na(.data$Taxname), !is.na(.data$CPUE)) %>%

      dplyr::mutate(Taxlifestage=paste(.data$Taxname, .data$Lifestage), #create variable for combo taxonomy x life stage
                    SampleID=paste(.data$Source, .data$Station, .data$Date, .data$ICF_ID), #Create identifier for each sample
                    BottomDepth=.data$BottomDepth*0.3048)%>% # Convert feet to meters
      dplyr::mutate(CPUE=dplyr::case_when(
        .data$CPUE!=0 ~ .data$CPUE,
        .data$CPUE==0 & .data$Date < .data$Intro ~ 0,
        .data$CPUE==0 & .data$Date >= .data$Intro & .data$Date < .data$DOPstart ~ NA_real_,
        .data$CPUE==0 & .data$Date >= .data$DOPstart & .data$Date < .data$DOPend ~ 0,
        .data$CPUE==0 & .data$Date >= .data$DOPend ~ NA_real_)) %>%
      dplyr::filter(!is.na(.data$CPUE)) %>%
      dplyr::select(-.data$DOP_Macro, -.data$ICF_ID, -.data$DOPstart, -.data$DOPend, -.data$Intro) #Remove DOP code
    cat("\nDOP_Macro finished!\n\n")

  }


  # FMWTSTN Meso --------------------------------------------------------------------

  if("FMWT_Meso"%in%Data_sets | "STN_Meso"%in%Data_sets) {

    FMWTSTN_Meso_file <- "FMWT_STN_CBNetCPUE.csv"
    FMWTSTN_Meso_URL<-paste0(FMWTSTN_pkg_url, "/", FMWTSTN_entities[FMWTSTN_Meso_file])
    SMSCG_Meso_file<-SMSCG_files[grep("CBNet", SMSCG_files)]

    #download the file
    if (!file.exists(file.path(Data_folder, FMWTSTN_Meso_file)) | Redownload_data) {
      Tryer(n=3, fun=utils::download.file, url=FMWTSTN_Meso_URL,
            destfile=file.path(Data_folder,FMWTSTN_Meso_file), mode="wb", method=Download_method)
    }

    if (!file.exists(file.path(Data_folder, names(SMSCG_Meso_file))) | Redownload_data) {
      Tryer(n=3, fun=utils::download.file, url=SMSCG_Meso_file,
            destfile=file.path(Data_folder, names(SMSCG_Meso_file)), mode="wb", method=Download_method)
    }


    # Import the FMWT data

    zoo_FMWT_Meso <- readr::read_csv(file.path(Data_folder, FMWTSTN_Meso_file),
                                     col_types=readr::cols_only(Project="c", Year="d", Survey="d",
                                                                Date="c", Station="c", Time="c",
                                                                TideCode="c", DepthBottom="d", CondSurf="d",
                                                                CondBott="d", TempSurf="d", Secchi="d",Turbidity="d",
                                                                Microcystis="c", Volume="d",
                                                                ACARTELA="d", ACARTIA="d", DIAPTOM="d",
                                                                EURYTEM="d", OTHCALAD="d", PDIAPFOR="d",
                                                                PDIAPMAR="d", SINOCAL="d", TORTANUS="d",
                                                                ACANTHO="d", LIMNOSPP="d", LIMNOSINE="d",
                                                                LIMNOTET="d", OITHDAV="d", OITHSIM="d",
                                                                OITHSPP="d", OTHCYCAD="d", HARPACT="d",
                                                                EURYJUV="d", OTHCALJUV="d", PDIAPJUV="d",
                                                                SINOCALJUV="d", ASINEJUV="d", ACARJUV="d",
                                                                DIAPTJUV="d", TORTJUV="d", LIMNOJUV="d",
                                                                OITHJUV="d", OTHCYCJUV="d", EURYNAUP="d",
                                                                OTHCOPNAUP="d", PDIAPNAUP="d", SINONAUP="d",
                                                                BOSMINA="d", DAPHNIA="d", DIAPHAN="d",
                                                                OTHCLADO="d", ASPLANCH="d", KERATELA="d",
                                                                OTHROT="d", POLYARTH="d", SYNCH="d",
                                                                TRICHO="d", BARNNAUP="d", CRABZOEA="d",
                                                                OSTRACOD="d", CUMAC="d"))%>%
      dplyr::mutate(ID=paste(.data$Year, .data$Project, .data$Survey, .data$Station),
                    Date=lubridate::parse_date_time(.data$Date, "%m/%d/%Y", tz="America/Los_Angeles"))

    zoo_SMSCG_Meso<-readr::read_csv(file.path(Data_folder, names(SMSCG_Meso_file)),
                                    col_types=readr::cols_only(Project="c", Year="d", Survey="d",
                                                               Date="c", Station="c", Time="c",
                                                               TideCode="c", DepthBottom="d", CondSurf="d",
                                                               PPTSurf="d", CondBott="d", PPTBott="d",
                                                               TempSurf="d", TempBottom="d", Secchi="d",
                                                               Turbidity="d", Microcystis="c", Volume="d",
                                                               ACARTELA="d", ACARTIA="d", DIAPTOM="d",
                                                               EURYTEM="d", OTHCALAD="d", PDIAPFOR="d",
                                                               PDIAPMAR="d", SINOCAL="d", TORTANUS="d",
                                                               ACANTHO="d", LIMNOSPP="d", LIMNOSINE="d",
                                                               LIMNOTET="d", OITHDAV="d", OITHSIM="d",
                                                               OTHCYCAD="d", HARPACT="d", EURYJUV="d",
                                                               OTHCALJUV="d", PDIAPJUV="d", SINOCALJUV="d",
                                                               ASINEJUV="d", ACARJUV="d", DIAPTJUV="d",
                                                               TORTJUV="d", LIMNOJUV="d", OITHJUV="d",
                                                               OTHCYCJUV="d", EURYNAUP="d", OTHCOPNAUP="d",
                                                               PDIAPNAUP="d", SINONAUP="d", BOSMINA="d",
                                                               DAPHNIA="d", DIAPHAN="d", OTHCLADO="d",
                                                               ASPLANCH="d", KERATELA="d", OTHROT="d",
                                                               POLYARTH="d", SYNCH="d", TRICHO="d",
                                                               BARNNAUP="d", CRABZOEA="d", OSTRACOD="d", CUMAC="d"))%>%
      dplyr::mutate(Project=dplyr::recode(.data$Project, TNS="STN"),
                    ID=paste(.data$Year, .data$Project, .data$Survey, .data$Station),
                    Date=lubridate::parse_date_time(.data$Date, "%m/%d/%Y", tz="America/Los_Angeles"))%>%
      dplyr::filter(!.data$ID%in%unique(zoo_FMWT_Meso$ID) & .data$Project!="EMP")%>%
      dplyr::mutate(Station=dplyr::if_else(.data$Project=="FRP", paste(.data$Project, .data$Station), .data$Station),
                    Project=dplyr::recode(.data$Project, FRP="STN"))

    # Transform from "wide" to "long" format, add some variables,
    # alter data to match other datasets

    data.list[["FMWT_Meso"]] <- zoo_FMWT_Meso%>%
      dplyr::bind_rows(zoo_SMSCG_Meso)%>%
      dplyr::select(-"ID")%>%
      dplyr::mutate(Datetime=lubridate::parse_date_time(dplyr::if_else(is.na(.data$Time) | !stringr::str_detect(.data$Time, stringr::fixed(":")),
                                                                       NA_character_,
                                                                       paste(.data$Date, .data$Time)), "%Y-%m-%d %H:%M", tz="America/Los_Angeles"))%>% #create a variable for datetime
      tidyr::pivot_longer(cols=c(-"Project", -"Year", -"Survey", -"Date", -"Datetime",
                                 -"Station",-"Time", -"TideCode",
                                 -"DepthBottom", -"CondSurf",
                                 -"CondBott",  -"TempSurf", -"Secchi",
                                 -"Turbidity", -"Microcystis",
                                 -"Volume"),
                          names_to="FMWT_Meso", values_to="CPUE")%>% #transform from wide to long
      dplyr::select(Source = "Project", "Year", "Date", "Datetime", "Station", Tide = "TideCode",
                    BottomDepth = "DepthBottom", "CondSurf", "CondBott", Temperature = "TempSurf",
                    "Secchi", "Turbidity", "Microcystis", "Volume", "FMWT_Meso", "CPUE")%>% #Select for columns in common and rename columns to match
      dplyr::left_join(Crosswalk%>% #Add in Taxnames, Lifestage, and taxonomic info
                         dplyr::select("FMWT_Meso", "Lifestage", "Taxname", "Phylum", "Class",
                                       "Order", "Family", "Genus", "Species", "Intro",
                                       "FMWTstart", "FMWTend")%>% #only retain FMWT codes
                         dplyr::filter(!is.na(.data$FMWT_Meso))%>% #Only retain Taxnames corresponding to FMWT codes
                         dplyr::distinct(),
                       by = "FMWT_Meso")%>%
      dplyr::filter(!is.na(.data$Taxname))%>%
      dplyr::mutate(Station=dplyr::recode(.data$Station, MONT="Mont", HONK="Honk"),
                    Taxlifestage=paste(.data$Taxname, .data$Lifestage), #create variable for combo taxonomy x life stage
                    Microcystis=dplyr::if_else(.data$Microcystis=="6", "2", .data$Microcystis), #Microsystis value of 6 only used from 2012-2015 and is equivalent to a 2 in other years, so just converting all 6s to 2s.
                    SampleID=paste(.data$Source, .data$Station, .data$Date),
                    SizeClass="Meso")%>% #Create identifier for each sample
      dplyr::mutate(CPUE=dplyr::case_when(
        .data$CPUE!=0 ~ CPUE,
        .data$CPUE==0 & .data$Date < .data$Intro ~ 0,
        .data$CPUE==0 & .data$Date >= .data$Intro & .data$Date < .data$FMWTstart ~ NA_real_,
        .data$CPUE==0 & .data$Date >= .data$FMWTstart & .data$Date < .data$FMWTend ~ 0,
        .data$CPUE==0 & .data$Date >= .data$FMWTend ~ NA_real_
      ))%>%
      dplyr::filter(!is.na(.data$CPUE))%>%
      dplyr::select(-"FMWT_Meso", -"FMWTstart", -"FMWTend", -"Intro")%>% #Remove FMWT taxa codes
      dplyr::left_join(stations, by=c("Source", "Station"))%>% #Add lat and long
      {if(!("FMWT_Meso"%in%Data_sets)){
        dplyr::filter(., .data$Source != "FMWT")
      } else{
        .
      }}%>%
      {if(!("STN_Meso"%in%Data_sets)){
        dplyr::filter(., .data$Source != "STN")
      } else{
        .
      }}

    cat("\nFMWT_Meso and/or STN_Meso finished!\n\n")

  }
  # twentymm Meso ----------------------------------------------------------------

  if("20mm_Meso"%in%Data_sets) {

    twentymm_Meso_file<-twentymm_files[grep("Zooplankton%20Catch%20Matrix", twentymm_files)]

    #download the file
    if (!file.exists(file.path(Data_folder, names(twentymm_Meso_file))) | Redownload_data) {
      Tryer(n=3, fun=utils::download.file, url=twentymm_Meso_file,
            destfile=file.path(Data_folder, names(twentymm_Meso_file)), mode="wb", method=Download_method)
    }



    # Import and modify 20mm data

    zoo_20mm_Meso<-readxl::read_excel(file.path(Data_folder, names(twentymm_Meso_file)),
                                      sheet="20-mm CB CPUE Data",
                                      col_types = c("date", rep("numeric", 3), "date", rep("numeric", 5), "text", rep("numeric", 74)))

    data.list[["twentymm_Meso"]]<-zoo_20mm_Meso%>%
      dplyr::mutate(SampleID = paste(.data$Station, .data$SampleDate, .data$TowNum),
                    SampleDate=lubridate::force_tz(.data$SampleDate, "America/Los_Angeles"),
                    Datetime=lubridate::parse_date_time(dplyr::if_else(is.na(.data$TowTime),
                                                                       NA_character_,
                                                                       paste0(.data$SampleDate, " ", lubridate::hour(.data$TowTime), ":", lubridate::minute(.data$TowTime))),
                                                        "%Y-%m-%d %H:%M", tz="America/Los_Angeles"))%>%
      tidyr::pivot_longer(cols=c(-"SampleDate", -"Survey", -"Station", -"TowTime", -"Temp", -"TopEC",
                                 -"BottomEC", -"Secchi", -"Turbidity", -"Tide", -"BottomDepth", -"Duration", -"MeterCheck", -"Volume",
                                 -"Dilution", -"SampleID", -"Datetime"),
                          names_to="twentymm_Meso", values_to="CPUE")%>% #transform from wide to long
      dplyr::select(Date="SampleDate", "Station", Temperature = "Temp", CondSurf = "TopEC", CondBott = "BottomEC", "Secchi",
                    "Turbidity", "Tide", "BottomDepth", "Volume", "SampleID", "Datetime", "twentymm_Meso", "CPUE")%>% #Select for columns in common and rename columns to match
      dplyr::left_join(Crosswalk%>% #Add in Taxnames, Lifestage, and taxonomic info
                         dplyr::select("twentymm_Meso", "Lifestage", "Taxname", "Phylum", "Class",
                                       "Order", "Family", "Genus", "Species", "Intro", "twentymmstart", "twentymmend", "twentymmstart2")%>% #only retain FMWT codes
                         dplyr::filter(!is.na(.data$twentymm_Meso))%>% #Only retain Taxnames corresponding to FMWT codes
                         dplyr::distinct(),
                       by = "twentymm_Meso")%>%
      dplyr::filter(!is.na(.data$Taxname))%>%
      dplyr::mutate(Source="twentymm",
                    SizeClass="Meso",
                    Station=as.character(.data$Station),
                    Taxlifestage=paste(.data$Taxname, .data$Lifestage),#add variable for data source, create variable for combo taxonomy x life stage
                    BottomDepth=.data$BottomDepth*0.3048)%>% # Convert feet to meters
      dplyr::mutate(CPUE=dplyr::case_when(
        .data$CPUE!=0 ~ .data$CPUE,
        .data$CPUE==0 & .data$Date < .data$Intro ~ 0,
        .data$CPUE==0 & .data$Date >= .data$Intro & .data$Date < .data$twentymmstart ~ NA_real_,
        .data$CPUE==0 & .data$Date >= .data$twentymmstart & .data$Date < .data$twentymmend ~ 0,
        .data$CPUE==0 & .data$Date >= .data$twentymmend & .data$Date < .data$twentymmstart2 ~ NA_real_,
        .data$CPUE==0 & .data$Date >= .data$twentymmstart2 ~ 0 #20mm dataset had one case of a taxa starting, ending, and starting again
      ))%>%
      dplyr::select(-"twentymmend", -"twentymmstart", -"twentymmstart2", -"Intro", -"twentymm_Meso")%>%
      dtplyr::lazy_dt()%>% #Speed up
      dplyr::group_by(dplyr::across(-"CPUE"))%>% #Some taxa names are repeated as in EMP so
      dplyr::summarise(CPUE=sum(.data$CPUE, na.rm=TRUE))%>% #this just adds up those duplications
      dplyr::ungroup()%>%
      tibble::as_tibble()%>%
      dplyr::mutate(Source="20mm",
                    SampleID=paste(.data$Source, .data$SampleID)) %>%#Create identifier for each sample

      dplyr::left_join(stations, by=c("Source", "Station")) #Add lat and long

    cat("\n20mm_Meso finished!\n\n")
  }

  # FRP Meso ---------------------------------------------------------------------

  if("FRP_Meso"%in%Data_sets) {

    # Import the FRP data

    #download the file
    if (!file.exists(file.path(Data_folder, "zoopsFRP2018.csv")) | Redownload_data) {
      Tryer(n=3, fun=utils::download.file, url="https://pasta.lternet.edu/package/data/eml/edi/269/2/d4c76f209a0653aa86bab1ff93ab9853",
            destfile=file.path(Data_folder, "zoopsFRP2018.csv"), mode="wb", method=Download_method)
    }

    zoo_FRP_Meso <- readr::read_csv(file.path(Data_folder, "zoopsFRP2018.csv"),
                                    col_types = "cccddddddddcccdddddc", na=c("", "NA"))

    #Already in long format
    data.list[["FRP_Meso"]] <- zoo_FRP_Meso%>%
      dplyr::mutate(Date=lubridate::parse_date_time(.data$Date, "%m/%d/%Y", tz="America/Los_Angeles"))%>%
      dplyr::mutate(Station=dplyr::recode(.data$Station, `Lindsey Tules`="Lindsey tules", LinBR="LinBr"))%>% #Rename inconsistent station names to match
      dplyr::mutate(Datetime=lubridate::parse_date_time(dplyr::if_else(is.na(.data$time),
                                                                       NA_character_,
                                                                       paste(.data$Date, .data$time)),
                                                        "%Y-%m-%d %H:%M:%S", tz="America/Los_Angeles"))%>% #Create a variable for datetime
      dplyr::mutate(Source="FRP", #add variable for data source
                    SizeClass="Meso",
                    Microcystis = dplyr::recode(.data$Microcystis, `1=absent`="1", `2=low`="2"))%>%
      dplyr::select(.data$Source, .data$Date, .data$Datetime, .data$Latitude, .data$Longitude,
                    .data$Station, CondSurf = .data$SC, .data$Secchi, .data$pH, .data$DO, .data$Turbidity, .data$Tide, .data$Microcystis, .data$SizeClass,
                    Temperature = .data$Temp, Volume = .data$volume, FRP_Meso = .data$CommonName, .data$CPUE, .data$SampleID)%>% #Select for columns in common and rename columns to match
      dplyr::group_by(dplyr::across(-.data$CPUE))%>% #Some taxa names are repeated as in EMP so
      dplyr::summarise(CPUE=sum(.data$CPUE, na.rm=T), .groups="drop")%>% #this just adds up those duplications
      tidyr::pivot_wider(names_from=.data$FRP_Meso, values_from=.data$CPUE, values_fill=list(CPUE=0))%>%
      tidyr::pivot_longer(cols=c(-.data$Source, -.data$Date, -.data$Datetime,
                                 -.data$Station, -.data$CondSurf, -.data$Secchi, -.data$pH, -.data$DO, -.data$Turbidity,
                                 -.data$Tide, -.data$Microcystis, -.data$SizeClass,-.data$Latitude, -.data$Longitude,
                                 -.data$Temperature, -.data$Volume, -.data$SampleID),
                          names_to="FRP_Meso", values_to="CPUE")%>%
      dplyr::left_join(Crosswalk%>% #Add in Taxnames, Lifestage, and taxonomic info
                         dplyr::select("FRP_Meso", "Lifestage", "Taxname", "Phylum", "Class", "Order", "Family", "Genus", "Species")%>% #only retain FRP codes
                         dplyr::filter(!is.na(.data$FRP_Meso))%>% #Only retain Taxnames corresponding to FRP codes
                         dplyr::distinct(),
                       by = "FRP_Meso")%>%
      dplyr::mutate(Taxlifestage=paste(.data$Taxname, .data$Lifestage))%>% #create variable for combo taxonomy x life stage
      dplyr::select(-"FRP_Meso")%>% #Remove FRP taxa codes
      dtplyr::lazy_dt()%>% #Speed up code
      dplyr::group_by(dplyr::across(-"CPUE"))%>% #Some taxa names are repeated as in EMP so
      dplyr::summarise(CPUE=sum(.data$CPUE, na.rm=TRUE))%>% #this just adds up those duplications
      dplyr::ungroup()%>%
      tibble::as_tibble()%>%
      dplyr::mutate(SampleID=paste(.data$Source, .data$SampleID)) #Create identifier for each sample
    cat("\nFRP_Meso finished!\n\n")
  }

  # YBFMP Meso/Micro -------------------------------------------------------------

  if("YBFMP_Meso"%in%Data_sets | "YBFMP_Micro"%in%Data_sets) {

    YBFMP_file<-"Zooplankton Data"
    YBFMP_URL<-paste0(YBFMP_pkg_url, "/", YBFMP_entities[YBFMP_file])

    #download the file
    if (!file.exists(file.path(Data_folder, YBFMP_file)) | Redownload_data) {
      Tryer(n=3, fun=utils::download.file, url=YBFMP_URL,
            destfile=file.path(Data_folder, YBFMP_file), mode="wb", method=Download_method)
    }

    zoo_YBFMP<-readr::read_csv(file.path(Data_folder, YBFMP_file),
                               col_types = readr::cols_only(Date="c", Time="c", StationCode="c",
                                                            Tide="c", WaterTemperature="d", Secchi="d",
                                                            SpCnd="d", pH="d", DO="d", Turbidity="d",
                                                            MicrocystisVisualRank="c", MeshSize="c", VolNet_ed="d",
                                                            TaxonName="c", LifeStage="c", CPUE_ed="d"))%>%
      dplyr::mutate(Index = 1:nrow(.))



    # Sum doubles with unclear life stages (both labeled as undifferentiated)
    doubles <- zoo_YBFMP %>%
      dplyr::group_by(.data$StationCode, .data$Date, .data$Time, .data$TaxonName, .data$LifeStage, .data$MeshSize) %>%
      dplyr::mutate(n =  dplyr::n()) %>%
      dplyr::filter(.data$n>1)

    Index_rm <- doubles$Index

    doubles_summed <- stats::aggregate(CPUE_ed~TaxonName, data = doubles, FUN = sum) %>%
      dplyr::right_join((doubles %>%
                           dplyr::select(-"CPUE_ed", -"Index", -"n") %>%
                           dplyr::distinct())) %>%
      dplyr::relocate("TaxonName", .after = "VolNet_ed") %>%
      dplyr::relocate("CPUE_ed", .after = "LifeStage")


    # Add zeroes, add sample ID, modify column names and order, join crosswalk taxonomy.
    data.list[["YBFMP"]] <- zoo_YBFMP %>%
      dplyr::filter(!(.data$Index %in% Index_rm)) %>%
      dplyr::select(-"Index") %>%
      dplyr::bind_rows(doubles_summed) %>% # replace doubles with summed CPUEs
      dplyr::mutate(TaxonName = replace(.data$TaxonName, .data$TaxonName == "Eucyclops phaleratus", "Ectocyclops phaleratus")) %>% # Otherwise creates doubles for Platycyclops phaleratus later on
      dplyr::mutate(YBFMP=paste(.data$TaxonName, .data$LifeStage),
                    MeshSize=dplyr::recode(.data$MeshSize, `150_micron`="Meso", `50_micron`="Micro"),
                    Source = "YBFMP",
                    SampleID = paste0(.data$Date, "_", .data$StationCode, "_", .data$MeshSize),
                    Datetime = lubridate::parse_date_time(paste(.data$Date, .data$Time), "%Y-%m-%d %H:%M:%S", tz="America/Los_Angeles"),
                    Date = lubridate::parse_date_time(.data$Date, "%Y-%m-%d", tz="America/Los_Angeles")) %>%
      dplyr:: select("Source",
                     SizeClass = "MeshSize",
                     Volume = "VolNet_ed",
                     "Date",
                     "Datetime",
                     Station = "StationCode",
                     Temperature = "WaterTemperature",
                     "Secchi", "Turbidity",
                     CondSurf = "SpCnd",
                     "pH", "DO",
                     Microcystis="MicrocystisVisualRank",
                     "SampleID",
                     "YBFMP",
                     CPUE = "CPUE_ed")%>%
      {if(!"YBFMP_Meso"%in%Data_sets){
        dplyr::filter(., .data$SizeClass!="Meso")
      }else{
        .
      }}%>%
      {if(!"YBFMP_Micro"%in%Data_sets){
        dplyr::filter(., .data$SizeClass!="Micro")
      }else{
        .
      }}%>%
      tidyr::pivot_wider(names_from=.data$YBFMP, values_from=.data$CPUE, values_fill=list(CPUE=0)) %>%
      tidyr::pivot_longer(cols=c(-.data$Source, -.data$SizeClass, -.data$Volume, -.data$Date,
                                 -.data$Datetime, -.data$Station, -.data$Temperature, -.data$CondSurf, -.data$Secchi,
                                 -.data$pH, -.data$DO, -.data$Turbidity, -.data$Microcystis,
                                 -.data$SampleID),
                          names_to="YBFMP", values_to="CPUE")%>%
      dplyr::left_join(Crosswalk %>%
                         dplyr::select(.data$YBFMP, .data$Lifestage, .data$Taxname, .data$Phylum, .data$Class,
                                       .data$Order, .data$Family, .data$Genus, .data$Species),
                       by = "YBFMP") %>%
      dplyr::mutate(Taxlifestage=paste(.data$Taxname, .data$Lifestage))%>% #create variable for combo taxonomy x life stage
      dplyr::select(-.data$YBFMP) %>% #Remove YBFMP taxa codes
      dplyr::mutate(SampleID=paste0(.data$Source, "_", .data$SampleID))  %>% #Create identifier for each sample
      dplyr::left_join(stations, by=c("Source", "Station")) #Add lat and long
    cat("\nFRP_Meso finished!\n\n")
  }

  # EMP Micro ---------------------------------------------------------------

  if("EMP_Micro"%in%Data_sets) {

    EMP_Micro_file<-"pump_matrix.csv"
    EMP_Micro_URL<-paste0(EMP_pkg_url, "/", EMP_entities[EMP_Micro_file])

    #download the file
    if (!file.exists(file.path(Data_folder, EMP_Micro_file)) | Redownload_data) {
      Tryer(n=3, fun=utils::download.file, url=EMP_Micro_URL,
            destfile=file.path(Data_folder, EMP_Micro_file), mode="wb", method=Download_method)
    }

    # Import the EMP data
    zoo_EMP_Micro<-readr::read_csv(file.path(Data_folder, EMP_Micro_file),
                                   col_types=readr::cols_only(SampleDate="c", StationNZ="c",
                                                              Chl_a="d", Secchi="d", Temperature="d",
                                                              ECSurfacePreTow="d", ECBottomPreTow="d",
                                                              Volume="d", Depth="d", LIMNOSPP="d",
                                                              LIMNOSINE="d", LIMNOTET="d", OITHDAV="d",
                                                              OITHSIM="d", OITHSPP="d", OTHCYCAD="d",
                                                              HARPACT="d", CYCJUV="d", LIMNOJUV="d",
                                                              OITHJUV="d", OTHCYCJUV="d", COPNAUP="d",
                                                              EURYNAUP="d", OTHCOPNAUP="d", PDIAPNAUP="d",
                                                              SINONAUP="d", ASPLANCH="d",
                                                              KERATELA="d",OTHROT="d", POLYARTH="d",
                                                              SYNCH="d",SYNCHBIC="d", TRICHO="d",
                                                              BARNNAUP="d"))

    # Tranform from "wide" to "long" format, add some variables,
    # alter data to match other datasets

    data.list[["EMP_Micro"]] <- zoo_EMP_Micro%>%
      dplyr::mutate(SampleDate=lubridate::parse_date_time(.data$SampleDate, "%m/%d/%Y", tz="America/Los_Angeles"))%>%
      dplyr::rename(OTHCYCADPUMP = "OTHCYCAD")%>%
      tidyr::pivot_longer(cols=c(-"SampleDate", -"StationNZ", -"Secchi", -"Chl_a", -"Temperature",
                                 -"ECSurfacePreTow", -"ECBottomPreTow", -"Depth", -"Volume"),
                          names_to="EMP_Micro", values_to="CPUE")%>% #transform from wide to long
      dplyr::mutate(Source="EMP",
                    SizeClass="Micro")%>% #add variable for data source
      dplyr::select("Source", Date = "SampleDate", Station="StationNZ", Chl = "Chl_a",
                    CondBott = "ECBottomPreTow", CondSurf = "ECSurfacePreTow", "Secchi",
                    "Temperature", BottomDepth="Depth", "SizeClass", "Volume", "EMP_Micro", "CPUE")%>% #Select for columns in common and rename columns to match
      dplyr::left_join(Crosswalk%>% #Add in Taxnames, Lifestage, and taxonomic info
                         dplyr::select("EMP_Micro", "Lifestage", "Taxname", "Phylum",
                                       "Class", "Order", "Family", "Genus", "Species",
                                       "Intro", "EMPstart", "EMPend")%>% #only retain EMP codes
                         dplyr::filter(!is.na(.data$EMP_Micro))%>% #Only retain Taxnames corresponding to EMP codes
                         dplyr::distinct(),
                       by="EMP_Micro")%>%
      dplyr::filter(!is.na(.data$Taxname))%>% #Should remove all the summed categories in original dataset
      dplyr::mutate(Taxlifestage=paste(.data$Taxname, .data$Lifestage), #create variable for combo taxonomy x life stage
                    SampleID=paste(.data$Source, .data$Station, .data$Date), #Create identifier for each sample
                    Tide="1", # All EMP samples collected at high slack
                    BottomDepth=.data$BottomDepth*0.3048)%>% # Convert to meters
      dplyr::mutate(CPUE=dplyr::case_when(
        .data$CPUE!=0 ~ .data$CPUE,
        .data$CPUE==0 & .data$Date < .data$Intro ~ 0,
        .data$CPUE==0 & .data$Date >= .data$Intro & .data$Date < .data$EMPstart ~ NA_real_,
        .data$CPUE==0 & .data$Date >= .data$EMPstart & .data$Date < .data$EMPend ~ 0,
        .data$CPUE==0 & .data$Date >= .data$EMPend ~ NA_real_
      ))%>%
      dplyr::select(-"EMP_Micro", -"EMPstart", -"EMPend", -"Intro")%>% #Remove EMP taxa codes
      dtplyr::lazy_dt()%>% #Speed up code using dtplyr package that takes advantage of data.table speed
      dplyr::group_by(dplyr::across(-"CPUE"))%>%
      dplyr::summarise(CPUE=sum(.data$CPUE, na.rm=TRUE))%>% #Some taxa now have the same names (e.g., CYCJUV and OTHCYCJUV) so we now add those categories together.
      dplyr::ungroup()%>%
      tibble::as_tibble() %>%#required to finish operation after lazy_dt()
      dplyr::left_join(stations, by=c("Source", "Station")) #Add lat and long

    cat("\nEMP_Micro finished!\n\n")
  }
  # FRP Macro ---------------------------------------------------------------

  if("FRP_Macro"%in%Data_sets) {

    # Import the FRP data

    #download the file
    if (!file.exists(file.path(Data_folder, "bugsFRP2018.csv")) | Redownload_data) {
      Tryer(n=3, fun=utils::download.file, url="https://pasta.lternet.edu/package/data/eml/edi/269/2/630f16b33a9cbf75f1989fc18690a6b3",
            destfile=file.path(Data_folder, "bugsFRP2018.csv"), mode="wb", method=Download_method)
    }

    zoo_FRP_Macro <- readr::read_csv(file.path(Data_folder, "bugsFRP2018.csv"),
                                     col_types = "ccccddddddddccdddcddc", na=c("", "NA"))

    #Already in long format
    data.list[["FRP_Macro"]] <- zoo_FRP_Macro%>%
      dplyr::filter(.data$Sampletype=="trawl")%>%
      dplyr::mutate(Date=lubridate::parse_date_time(.data$Date, "%m/%d/%Y", tz="America/Los_Angeles"))%>%
      dplyr::mutate(Station=dplyr::recode(.data$Station, `Lindsey Tules`="Lindsey tules", LinBR="LinBr", MINSLO1="MinSlo1", ProBR="ProBr", WinBR="WinBr"))%>% #Rename inconsistent station names to match
      dplyr::mutate(Datetime=lubridate::parse_date_time(dplyr::if_else(is.na(.data$time),
                                                                       NA_character_,
                                                                       paste(.data$Date, .data$time)),
                                                        "%Y-%m-%d %H:%M:%S", tz="America/Los_Angeles"))%>% #Create a variable for datetime
      dplyr::mutate(Source = "FRP",
                    SizeClass = "Macro",
                    CPUE = .data$AdjCount/.data$volume, #add variable for data source and calculate CPUE
                    Microcystis = dplyr::recode(.data$Microcystis, `1=absent`="1", `2=low`="2"))%>%
      dplyr::select(.data$Source, .data$Date, .data$Datetime, .data$Latitude, .data$Longitude,
                    .data$Station, CondSurf = .data$SC, .data$Secchi, .data$pH, .data$DO, .data$Turbidity, .data$Tide, .data$Microcystis, .data$SizeClass,
                    Temperature = .data$Temp, Volume = .data$volume, FRP_Macro = .data$CommonName, .data$CPUE, .data$SampleID)%>% #Select for columns in common and rename columns to match
      dplyr::group_by(dplyr::across(-.data$CPUE))%>% #Some taxa names are repeated as in EMP so
      dplyr::summarise(CPUE=sum(.data$CPUE, na.rm=T), .groups="drop")%>% #this just adds up those duplications
      tidyr::pivot_wider(names_from=.data$FRP_Macro, values_from=.data$CPUE, values_fill=list(CPUE=0))%>%
      tidyr::pivot_longer(cols=c(-.data$Source, -.data$Date, -.data$Datetime, -.data$Latitude, -.data$Longitude,
                                 -.data$Station, -.data$CondSurf, -.data$Secchi, -.data$pH,
                                 -.data$DO, -.data$Turbidity, -.data$Tide, -.data$Microcystis, -.data$SizeClass,
                                 -.data$Temperature, -.data$Volume, -.data$SampleID),
                          names_to="FRP_Macro", values_to="CPUE")%>%
      dplyr::left_join(Crosswalk%>% #Add in Taxnames, Lifestage, and taxonomic info
                         dplyr::select("FRP_Macro", "Lifestage", "Taxname", "Phylum", "Class", "Order", "Family", "Genus", "Species")%>% #only retain FRP codes
                         dplyr::filter(!is.na(.data$FRP_Macro))%>% #Only retain Taxnames corresponding to FRP codes
                         dplyr::distinct(),
                       by = "FRP_Macro")%>%
      dplyr::mutate(Taxlifestage=paste(.data$Taxname, .data$Lifestage))%>% #create variable for combo taxonomy x life stage
      dplyr::select(-"FRP_Macro")%>% #Remove FRP taxa codes
      dtplyr::lazy_dt()%>% #Speed up code
      dplyr::group_by(dplyr::across(-"CPUE"))%>% #Some taxa names are repeated as in EMP so
      dplyr::summarise(CPUE=sum(.data$CPUE, na.rm=T))%>% #this just adds up those duplications
      dplyr::ungroup()%>%
      tibble::as_tibble()%>%
      dplyr::mutate(SampleID=paste(.data$Source, .data$SampleID)) #Create identifier for each sample
    cat("\nFRP_Macro finished!\n\n")
  }

  # EMP Macro ---------------------------------------------------------------

  if("EMP_Macro"%in%Data_sets) {

    EMP_Macro_file<-"macro_matrix.csv"
    EMP_Macro_URL<-paste0("https://pasta.lternet.edu/package/data/eml/edi/522/", EMP_latest_revision, "/", EMP_entities[EMP_Macro_file])

    #download the file
    if (!file.exists(file.path(Data_folder, EMP_Macro_file)) | Redownload_data) {
      Tryer(n=3, fun=utils::download.file, url=EMP_Macro_URL,
            destfile=file.path(Data_folder, EMP_Macro_file), mode="wb", method=Download_method)
    }

    # Import the EMP data

    zoo_EMP_Macro<-readr::read_csv(file.path(Data_folder, EMP_Macro_file),
                                   col_types=readr::cols_only(SampleDate="c", Time="c", StationNZ="c",
                                                              Chl_a="d", Secchi="d", Temperature="d",
                                                              ECSurfacePreTow="d", ECBottomPreTow="d",
                                                              Volume="d", Depth="d", AmphipodCode="c", A_aspera="d",
                                                              A_hwanhaiensis="d", A_macropsis="d", D_holmquistae="d",
                                                              H_longirostris="d", N_kadiakensis="d", N_mercedis="d",
                                                              Unidentified_mysid="d", A_spinicorne="d", A_stimpsoni="d",
                                                              A_abdita="d", Ampithoe_sp="d", Caprelidae_sp="d",
                                                              C_alienense="d", Crangonyx_sp="d", G_daiberi="d",
                                                              G_japonica="d", Hyalella_sp="d", Monocorophium_sp="d",
                                                              Oedicerotidae_sp="d", Pleustidae="d", Unidentified_Amphipod="d",
                                                              Unidentified_Corophium="d", Unidentified_Gammarus="d", Amphipod_Total="d"))%>%
      dplyr::filter(dplyr::if_any(dplyr::everything(), ~ !is.na(.)))

    # Tranform from "wide" to "long" format, add some variables,
    # alter data to match other datasets

    data.list[["EMP_Macro"]] <- zoo_EMP_Macro%>%
      dplyr::mutate(SampleDate=lubridate::parse_date_time(.data$SampleDate, "%m/%d/%Y", tz="America/Los_Angeles"),
                    Datetime=lubridate::parse_date_time(dplyr::if_else(is.na(.data$Time), NA_character_, paste(.data$SampleDate, .data$Time)),
                                                        c("%Y-%m-%d %H:%M", "%Y-%m-%d %I:%M:%S %p"), tz="Etc/GMT+8"), #create a variable for datetime
                    Datetime=lubridate::with_tz(.data$Datetime, "America/Los_Angeles"), # Ensure everything ends up in local time
                    Unidentified_mysid=dplyr::if_else(lubridate::year(.data$SampleDate)<2014, .data$Amphipod_Total, .data$Unidentified_mysid))%>% # Transfer pre 2014 amphipod counts to Amphipod_total
      tidyr::pivot_longer(cols=c(-"SampleDate", -"Time", -"Datetime", -"StationNZ", -"Secchi", -"Chl_a", -"Temperature",
                                 -"ECSurfacePreTow", -"ECBottomPreTow", -"Volume", -"Depth", -"AmphipodCode"),
                          names_to="EMP_Macro", values_to="CPUE")%>% #transform from wide to long
      dplyr::mutate(Source="EMP",
                    SizeClass="Macro")%>% #add variable for data source
      dplyr::select("Source", Date = "SampleDate", "Datetime", Station="StationNZ", Chl = "Chl_a",
                    CondBott = "ECBottomPreTow", CondSurf = "ECSurfacePreTow", "Secchi", "SizeClass",
                    "Temperature", BottomDepth="Depth", "Volume", "AmphipodCode", "EMP_Macro", "CPUE")%>% #Select for columns in common and rename columns to match
      dplyr::left_join(Crosswalk%>% #Add in Taxnames, Lifestage, and taxonomic info
                         dplyr::select("EMP_Macro", "Lifestage", "Taxname", "Phylum", "Class",
                                       "Order", "Family", "Genus", "Species", "Intro", "EMPstart", "EMPend")%>% #only retain EMP codes
                         dplyr::filter(!is.na(.data$EMP_Macro))%>% #Only retain Taxnames corresponding to EMP codes
                         dplyr::distinct(),
                       by="EMP_Macro")%>%
      dplyr::filter(!is.na(.data$Taxname))%>% #Should remove all the summed categories in original dataset
      dplyr::mutate(Taxlifestage=paste(.data$Taxname, .data$Lifestage), #create variable for combo taxonomy x life stage
                    SampleID=paste(.data$Source, .data$Station, .data$Date), #Create identifier for each sample
                    Tide="1", # All EMP samples collected at high slack
                    BottomDepth=.data$BottomDepth*0.3048)%>% # Convert to meters
      dplyr::mutate(CPUE=dplyr::case_when(
        .data$CPUE!=0 ~ .data$CPUE,
        .data$CPUE==0 & .data$Date < .data$Intro ~ 0,
        .data$CPUE==0 & .data$Date >= .data$Intro & .data$Date < .data$EMPstart ~ NA_real_,
        .data$CPUE==0 & .data$Date >= .data$EMPstart & .data$Date < .data$EMPend ~ 0,
        .data$CPUE==0 & .data$Date >= .data$EMPend ~ NA_real_),
        CPUE=dplyr::if_else(.data$AmphipodCode!="A" & .data$Order=="Amphipoda", NA_real_, .data$CPUE))%>% # Remove any tainted amphipod data (e.g., veg in net)
      dplyr::select(-"EMP_Macro", -"EMPstart", -"EMPend", -"Intro")%>% #Remove EMP taxa codes
      dtplyr::lazy_dt()%>% #Speed up code using dtplyr package that takes advantage of data.table speed
      dplyr::group_by(dplyr::across(-"CPUE"))%>%
      dplyr::summarise(CPUE=sum(.data$CPUE, na.rm=TRUE))%>% #Some taxa now have the same names (e.g., CYCJUV and OTHCYCJUV) so we now add those categories together.
      dplyr::ungroup()%>%
      tibble::as_tibble() %>% #required to finish operation after lazy_dt()
    dplyr::left_join(stations, by=c("Source", "Station"))

    cat("\nEMP_Macro finished!\n\n")
  }
  # FMWT Macro --------------------------------------------------------------

  if("FMWT_Macro"%in%Data_sets | "STN_Macro"%in%Data_sets) {

    FMWTSTN_Macro_file <- "FMWT_MysidNetCPUE.csv"
    FMWTSTN_Macro_URL<-paste0(FMWTSTN_pkg_url, "/", FMWTSTN_entities[FMWTSTN_Macro_file])

    SMSCG_Macro_file<-SMSCG_files[grep("MysidNet", SMSCG_files)]

    #download the file
    if (!file.exists(file.path(Data_folder, FMWTSTN_Macro_file)) | Redownload_data) {
      Tryer(n=3, fun=utils::download.file, url=FMWTSTN_Macro_URL,
            destfile=file.path(Data_folder,FMWTSTN_Macro_file), mode="wb", method=Download_method)
    }

    #download the file
    if (!file.exists(file.path(Data_folder, names(SMSCG_Macro_file))) | Redownload_data) {
      Tryer(n=3, fun=utils::download.file, url=SMSCG_Macro_file,
            destfile=file.path(Data_folder, names(SMSCG_Macro_file)), mode="wb", method=Download_method)
    }


    zoo_FMWT_Macro <- readr::read_csv(file.path(Data_folder, FMWTSTN_Macro_file),
                                      col_types=readr::cols_only(Project="c", Year="d", Survey="d",
                                                                 Date="c", Station="c", Time="c",
                                                                 TideCode="c", DepthBottom="d", CondSurf="d",
                                                                 CondBott="d", TempSurf="d", Secchi="d",
                                                                 Turbidity="d", Microcystis="c", Volume="d",
                                                                 Acanthomysis_aspera="d", Hyperacanthomysis_longirostris="d", Acanthomysis_hwanhaiensis="d",
                                                                 Alienacanthomysis_macropsis="d", Deltamysis_holmquistae="d", Neomysis_kadiakensis="d",
                                                                 Neomysis_mercedis="d", Unidentified_Mysid="d", Americorophium_spinicorne="d",
                                                                 Americorophium_stimpsoni="d", Ampelisca_abdita="d", Corophium_alienense="d",
                                                                 Crangonyx_sp="d", Gammarus_daiberi="d", Grandidierella_japonica="d",
                                                                 Hyalella_sp="d", Unidentified_Amphipod="d", Unidentified_Corophium="d",
                                                                 Unidentified_Gammarus="d"))%>%
      dplyr::mutate(ID=paste(.data$Year, .data$Project, .data$Survey, .data$Station))

    zoo_SMSCG_Macro <- readr::read_csv(file.path(Data_folder, names(SMSCG_Macro_file)),
                                       col_types=readr::cols_only(Project="c", Year="d", Survey="d",
                                                                  Date="c", Station="c", Time="c",
                                                                  TideCode="c", DepthBottom="d", CondSurf="d",
                                                                  CondBott="d", TempSurf="d", Secchi="d",
                                                                  Turbidity="d", Microcystis="c", Volume="d",
                                                                  Acanthomysis_aspera="d", Hyperacanthomysis_longirostris="d", Acanthomysis_hwanhaiensis="d",
                                                                  Alienacanthomysis_macropsis="d", Deltamysis_holmquistae="d", Neomysis_kadiakensis="d",
                                                                  Neomysis_mercedis="d", Unidentified_Mysid="d", Americorophium_spinicorne="d",
                                                                  Americorophium_stimpsoni="d", Ampelisca_abdita="d", Corophium_alienense="d",
                                                                  Crangonyx_sp="d", Gammarus_daiberi="d", Grandidierella_japonica="d",
                                                                  Hyalella_sp="d", Unidentified_Amphipod="d", Unidentified_Corophium="d",
                                                                  Unidentified_Gammarus="d"))%>%
      dplyr::mutate(ID=paste(.data$Year, .data$Project, .data$Survey, .data$Station))%>%
      dplyr::filter(!.data$ID%in%unique(zoo_FMWT_Macro$ID) & .data$Project%in%c("FMWT", "STN"))

    data.list[["FMWT_Macro"]] <- dplyr::bind_rows(zoo_FMWT_Macro, zoo_SMSCG_Macro)%>%
      dplyr::select(-"ID")%>%
      dplyr::mutate(Datetime = lubridate::parse_date_time(dplyr::if_else(is.na(.data$Time), NA_character_, paste(.data$Date, .data$Time)), "%m/%d/%Y %H:%M", tz="America/Los_Angeles"),
                    Date=lubridate::parse_date_time(.data$Date, "%m/%d/%Y", tz="America/Los_Angeles"),
                    Microcystis = as.character(.data$Microcystis))%>% #create a variable for datetime
      tidyr::pivot_longer(cols=c(-"Project", -"Year", -"Survey", -"Date", -"Datetime",
                                 -"Station", -"Time", -"TideCode",
                                 -"DepthBottom", -"CondSurf", -"CondBott",
                                 -"TempSurf", -"Secchi", -"Turbidity", -"Microcystis",
                                 -"Volume"),
                          names_to="FMWT_Macro", values_to="CPUE")%>% #transform from wide to long
      dplyr::select(Source = "Project", "Date", "Datetime", "Station", Tide = "TideCode", BottomDepth = "DepthBottom",
                    "CondSurf", "CondBott", Temperature = "TempSurf", "Secchi", "Turbidity", "Microcystis", "Volume",
                    "FMWT_Macro", "CPUE")%>% #Select for columns in common and rename columns to match
      dplyr::left_join(Crosswalk%>% #Add in Taxnames, Lifestage, and taxonomic info
                         dplyr::select("FMWT_Macro", "Lifestage", "Taxname", "Phylum", "Class", "Order",
                                       "Family", "Genus", "Species", "Intro", "FMWTstart", "FMWTend")%>% #only retain FMWT codes
                         dplyr::filter(!is.na(.data$FMWT_Macro))%>% #Only retain Taxnames corresponding to FMWT codes
                         dplyr::distinct(),
                       by = "FMWT_Macro")%>%
      dplyr::filter(!is.na(.data$Taxname))%>%
      dplyr::mutate(Station=dplyr::recode(.data$Station, MONT="Mont", HONK="Honk"),
                    Taxlifestage=paste(.data$Taxname, .data$Lifestage), #create variable for combo taxonomy x life stage
                    Microcystis=dplyr::if_else(.data$Microcystis=="6", "2", .data$Microcystis), #Microsystis value of 6 only used from 2012-2015 and is equivalent to a 2 in other years, so just converting all 6s to 2s.
                    SampleID=paste(.data$Source, .data$Station, .data$Date), #Create identifier for each sample
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
      dplyr::select(-"FMWT_Macro", -"FMWTstart", -"FMWTend", -"Intro")%>% #Remove FMWT taxa codes
      dplyr::left_join(stations, by=c("Source", "Station"))%>% #at latitude and longitude.
      {if(!("FMWT_Macro"%in%Data_sets)){
        dplyr::filter(., .data$Source != "FMWT")
      } else{
        .
      }}%>%
      {if(!("STN_Macro"%in%Data_sets)){
        dplyr::filter(., .data$Source != "STN")
      } else{
        .
      }}

    cat("\nFMWT_Macro and/or STN_Macro finished!\n\n")
  }

  # Combine data ----------------------------------------
  cat("\nCombining datasets...\n")
  zoop<-dplyr::bind_rows(data.list)%>% # Combine data
    dplyr::filter(!is.na(.data$Taxname))%>% #Remove NA taxnames (should only correspond to previously summed "all" categories from input datasets)
    dplyr::mutate(SalSurf= wql::ec2pss(.data$CondSurf/1000, t=25),
                  Year=lubridate::year(.data$Date))%>%
    {if("Tide"%in%names(.)){
    dplyr::mutate(., Tide=dplyr::recode(.data$Tide, "1"="High slack", "2"="Ebb", "3"="Low slack", "4"="Flood", "1=high slack"="High slack", "2=ebb"="Ebb", "3=low slack"="Low slack", "4=flood"="Flood")) #Rename tide codes to be consistent
    } else{
    .
    }}%>%
    {if("SalBott"%in%names(.)){
    dplyr::mutate(SalBott=wql::ec2pss(.data$CondBott/1000, t=25))
    } else{
    .
    }}%>%
    dplyr::select(-tidyselect::any_of(c("Region", "CondBott", "CondSurf"))) #Remove some extraneous variables to save memory

  stationsEMPEZ<-zooper::stationsEMPEZ

  if(any(unique(stationsEMPEZ$Station)%in%unique(zoop$Station))){
    zoop<-zoop%>%
      dplyr::filter(.data$Station%in%unique(stationsEMPEZ$Station))%>%
      dplyr::select(-"Latitude", -"Longitude")%>%
      dplyr::left_join(stationsEMPEZ, by=c("Date", "Station"))%>%
      dplyr::bind_rows(zoop%>%
                         dplyr::filter(!.data$Station%in%unique(stationsEMPEZ$Station)))
  }

  zoopEnv<-zoop%>%
    dplyr::select(-"SizeClass", -"Volume", -"Lifestage", -"Taxname", -"Phylum", -"Class", -"Order",
                  -"Family", -"Genus", -"Species", -"Taxlifestage", -"CPUE")%>%
    dplyr::distinct()

  # Remove duplicated samples not caught by distinct
  dups<-dplyr::filter(zoopEnv, .data$SampleID%in%.data$SampleID[which(duplicated(.data$SampleID))])%>%
    dplyr::group_by(.data$SampleID)%>%
    dplyr::mutate(dplyr::across(where(is.numeric), mean, na.rm=T))%>%
    dplyr::mutate(dplyr::across(where(lubridate::is.POSIXct), ~suppressWarnings(dplyr::if_else(all(is.na(.x)), lubridate::parse_date_time(NA_character_, tz="America/Los_Angeles"), min(.x, na.rm=T)))))%>%
    tidyr::fill(where(is.character), .direction="downup")%>%
    dplyr::mutate(dplyr::across(where(is.character), ~unique(.x)[1]))%>%
    dplyr::ungroup()%>%
    dplyr::distinct()

  zoopEnv<-zoopEnv%>%
    dplyr::filter(!.data$SampleID%in%dups$SampleID)%>%
    dplyr::bind_rows(dups)%>%
    dplyr::mutate(dplyr::across(where(is.numeric), ~ dplyr::if_else(is.nan(.x), NA_real_, .x)))


  zoop<-zoop%>%
    dplyr::select("Source", "SizeClass", "Volume", "Lifestage", "Taxname", "Phylum", "Class",
                  "Order", "Family", "Genus", "Species", "Taxlifestage", "SampleID", "CPUE")

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
