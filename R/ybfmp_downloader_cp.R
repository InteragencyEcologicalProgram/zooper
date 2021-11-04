if("YBFMP_Meso"%in%Data_sets | "YBFMP_Micro"%in%Data_sets) {

  YBFMP_file<-"Zooplankton data"
  # YBFMP_URL<-paste0(YBFMP_pkg_url, "/", YBFMP_entities[YBFMP_file]) Don't know what this is supposed to do
  YBFMP_URL <- "https://portal.edirepository.org/nis/dataviewer?packageid=edi.494.1&entityid=9190cd46d697e59aca2de678f4ca1c95"

  #download the file
  if (!file.exists(file.path(Data_folder, YBFMP_file)) | Redownload_data) {
    Tryer(n=3, fun=utils::download.file, url=YBFMP_URL,
          destfile=file.path(Data_folder, YBFMP_file), mode="wb", method="curl")
  }


  ### Here I read in data from online as I wasn't too clear what was going on above/if I should be downloading the data first into a folder, so feel free to change. I also read in the crosswalk below because I wasn't sure how to get the updated version into the package.
  Crosswalk <- readxl::read_excel("data-raw/crosswalk.xlsx", sheet = 2) #I put this here for my testing but can be removed once the crosswalk is updated.
  zoo_YBFMP<-readr::read_csv(file.path(YBFMP_URL),
                             col_types = readr::cols_only(Date="c", Time="c", StationCode="c",
                                                          Tide="c", WaterTemperature="d", Secchi="d",
                                                          SpCnd="d", pH="d", DO="d", Turbidity="d",
                                                          MicrocystisVisualRank="d", MeshSize="c", VolNet_ed="d",
                                                          TaxonName="c", LifeStage="c", CPUE_ed="d"))

  zoo_YBFMP2 <- zoo_YBFMP %>%dplyr::mutate(YBFMP=paste(.data$TaxonName, .data$LifeStage),
                MeshSize=dplyr::recode(.data$MeshSize, `150_micron`="Meso", `50_micron`="Micro"),
                Source = "YBFMP",
                SampleID = paste0(.data$Date, "_", .data$StationCode),
                Datetime = lubridate::parse_date_time(paste(.data$Date, .data$Time), "%Y-%m-%d %H:%M:%S", tz="America/Los_Angeles")) %>%
    dplyr:: select( .data$Source, SizeClass = .data$MeshSize,
                    Volume = .data$VolNet_ed,
                    Date = .data$Date,
                    SampleTime = .data$Time,
                    Station = .data$StationCode,
                   Temperature = .data$WaterTemperature,
                   .data$Secchi, .data$Turbidity,
                   CondSurf = .data$SpCnd,
                   .data$pH, .data$DO,
                   Microcystis=.data$MicrocystisVisualRank,
                 .data$SampleID,.data$Datetime,
                 .data$YBFMP,
                   CPUE = .data$CPUE_ed)%>%
    dplyr::left_join(Crosswalk[,11:20] %>%
    select(.data$YBFMP, .data$Lifestage, .data$Taxname, .data$Phylum, .data$Class, .data$Order, .data$Family, .data$Genus, .data$Species)) %>%
    dplyr::mutate(Taxlifestage=paste(.data$Taxname, .data$Lifestage))%>% #create variable for combo taxonomy x life stage
    dplyr::select(-.data$YBFMP) %>% #Remove YBFMP taxa codes
  dplyr::mutate(SampleID=paste0(.data$Source, "_", .data$SampleID))  #Create identifier for each sample


  data.list[["YBFMP_Meso"]] <-
    filter(zoo_YBFMP2, SizeClass == "Meso")

  data.list[["YBFMP_Micro"]] <-
    filter(zoo_YBFMP2, SizeClass == "Micro")


#
#   {if(!"YBFMP_Meso"%in%Data_sets){
#       dplyr::filter(., .data$SizeClass!="Meso")
#     }}%>%
#     {if(!"YBFMP_Micro"%in%Data_sets){
#       dplyr::filter(., .data$SizeClass!="Micro")
#     }}

  cat("\nYBFMP finished!\n\n")
}
