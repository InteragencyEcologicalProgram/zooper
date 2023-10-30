require(zooper)
require(readr)
require(dplyr)


revision_url <- "https://pasta.lternet.edu/package/eml/edi/522"
EMP_latest_revision <- tail(zooper:::Tryer(n=3, fun=readLines, con=revision_url, warn = FALSE), 1)
pkg_url <- paste0("https://pasta.lternet.edu/package/data/eml/edi/522/", EMP_latest_revision)
EMP_entities <- zooper:::Tryer(n=3, fun=readLines, con=pkg_url, warn = FALSE)
name_urls <- paste("https://pasta.lternet.edu/package/name/eml/edi/522", EMP_latest_revision, EMP_entities, sep="/")
names(EMP_entities) <- purrr::map_chr(name_urls, ~Tryer(n=3, fun=readLines, con=.x, warn = FALSE))

FMWTSTN_URL<-"https://filelib.wildlife.ca.gov/Public/TownetFallMidwaterTrawl/Zoopl_TownetFMWT/"
FMWTSTN_files<-zooper:::html_file_list(FMWTSTN_URL)
SMSCG_URL<-"https://filelib.wildlife.ca.gov/Public/TownetFallMidwaterTrawl/Zooplankton_SMSCG/"
SMSCG_files<-zooper:::html_file_list(SMSCG_URL)


twentymm_URL<-"https://filelib.wildlife.ca.gov/Public/Delta%20Smelt/"
twentymm_files<-zooper:::html_file_list(twentymm_URL)

YBFMP_revision_url <- "https://pasta.lternet.edu/package/eml/edi/494"
YBFMP_latest_revision <- utils::tail(Tryer(n=3, fun=readLines, con=YBFMP_revision_url, warn = FALSE), 1)
YBFMP_pkg_url <- paste0("https://pasta.lternet.edu/package/data/eml/edi/494/", YBFMP_latest_revision)
YBFMP_entities <- Tryer(n=3, fun=readLines, con=YBFMP_pkg_url, warn = FALSE)
YBFMP_name_urls <- paste("https://pasta.lternet.edu/package/name/eml/edi/494", YBFMP_latest_revision, YBFMP_entities, sep="/")
names(YBFMP_entities) <- purrr::map_chr(YBFMP_name_urls, ~Tryer(n=3, fun=readLines, con=.x, warn = FALSE))

DOP_revision_url <- "https://pasta.lternet.edu/package/eml/edi/1187"
DOP_latest_revision <- utils::tail(Tryer(n=3, fun=readLines, con=DOP_revision_url, warn = FALSE), 1)
DOP_pkg_url <- paste0("https://pasta.lternet.edu/package/data/eml/edi/1187/", DOP_latest_revision)
DOP_entities <- Tryer(n=3, fun=readLines, con=DOP_pkg_url, warn = FALSE)
DOP_name_urls <- paste("https://pasta.lternet.edu/package/name/eml/edi/1187", DOP_latest_revision, DOP_entities, sep="/")
names(DOP_entities) <- purrr::map_chr(DOP_name_urls, ~Tryer(n=3, fun=readLines, con=.x, warn = FALSE))

Data_folder<-tempdir()


# EMP Meso ----------------------------------------------------------------

EMP_Meso_file<-"cb_matrix.csv"
EMP_Meso_URL<-paste0("https://pasta.lternet.edu/package/data/eml/edi/522/", EMP_latest_revision, "/", EMP_entities[EMP_Meso_file])
Tryer(n=3, fun=download.file, url=EMP_Meso_URL, destfile=file.path(Data_folder, EMP_Meso_file), mode="wb", method="curl")


names_EMP_Meso<-readr::read_csv(file.path(Data_folder, EMP_Meso_file), col_types = cols(.default=col_character()))%>%
  names()


# FMWT STN Meso -----------------------------------------------------------

FMWTSTN_Meso_file<-FMWTSTN_files[grep("CBNet", FMWTSTN_files)]
SMSCG_Meso_file<-SMSCG_files[grep("CBNet", SMSCG_files)]

Tryer(n=3, fun=download.file, url=FMWTSTN_Meso_file,
           destfile=file.path(Data_folder, names(FMWTSTN_Meso_file)), mode="wb", method="curl")

Tryer(n=3, fun=download.file, url=SMSCG_Meso_file,
           destfile=file.path(Data_folder, names(SMSCG_Meso_file)), mode="wb", method="curl")

names_FMWTSTN_Meso<-readr::read_csv(file.path(Data_folder, names(FMWTSTN_Meso_file)),
                                    col_types = "c")%>%
  names()

names_SMSCG_Meso<-readr::read_csv(file.path(Data_folder, names(SMSCG_Meso_file)),
                                     col_types = "c")%>%
  names()


# 20mm Meso ---------------------------------------------------------------

twentymm_Meso_file<-twentymm_files[grep("Zooplankton%20Catch%20Matrix", twentymm_files)]

Tryer(n=3, fun=download.file, url=twentymm_Meso_file,
           destfile=file.path(Data_folder, names(twentymm_Meso_file)), mode="wb", method="curl")

names_20mm_Meso<-readxl::read_excel(file.path(Data_folder, names(twentymm_Meso_file)),
                                    sheet="20-mm CB CPUE Data",
                                    col_types = "text")%>%
  names()


# FRP ----------------------------------------------------------------

Tryer(n=3, fun=utils::download.file, url="https://portal.edirepository.org/nis/dataviewer?packageid=edi.269.3&entityid=5218ffbc7b8f38959704a46ffb668ad9",
      destfile=file.path(Data_folder, "zoopsFRP2021.csv"), mode="wb", method=Download_method)
Tryer(n=3, fun=utils::download.file, url="https://portal.edirepository.org/nis/dataviewer?packageid=edi.269.3&entityid=fa84750e51c319d309f97357b7d34315",
      destfile=file.path(Data_folder, "sitesFRP2021.csv"), mode="wb", method=Download_method)
Tryer(n=3, fun=utils::download.file, url="https://portal.edirepository.org/nis/dataviewer?packageid=edi.269.3&entityid=8de785ee47220f3893654478c79b5f8f",
      destfile=file.path(Data_folder, "bugsFRP2021.csv"), mode="wb", method=Download_method)

zoo_FRP_Meso <- readr::read_csv(file.path(Data_folder, "zoopsFRP2021.csv"), na=c("", "NA"))
zoo_FRP_Macro <- readr::read_csv(file.path(Data_folder, "bugsFRP2021.csv"), na=c("", "NA"))
sites_FRP_Macro <- readr::read_csv(file.path(Data_folder, "sitesFRP2021.csv"), na=c("", "NA"))


names_FRP_Macro <- names(zoo_FRP_Macro)

names_FRP_sites <- names(sites_FRP_Macro)

names_FRP_Meso <- names(zoo_FRP_Meso)


# YBFMP Meso/Micro --------------------------------------------------------

YBFMP_file<-"Zooplankton Data"
YBFMP_URL<-paste0(YBFMP_pkg_url, "/", YBFMP_entities[YBFMP_file])
Tryer(n=3, fun=utils::download.file, url=YBFMP_URL,
      destfile=file.path(Data_folder, YBFMP_file), mode="wb", method="curl")

names_YBFMP<-readr::read_csv(file.path(Data_folder, YBFMP_file),
                           col_types = cols(.default=col_character()))%>%
  names()


# EMP Micro ---------------------------------------------------------------

EMP_Micro_file<-"pump_matrix.csv"
EMP_Micro_URL<-paste0("https://pasta.lternet.edu/package/data/eml/edi/522/", EMP_latest_revision, "/", EMP_entities[EMP_Micro_file])
Tryer(n=3, fun=download.file, url=EMP_Micro_URL,
             destfile=file.path(Data_folder, EMP_Micro_file), mode="wb", method="curl")

names_EMP_Micro<-readr::read_csv(file.path(Data_folder, EMP_Micro_file),
                                 col_types=cols(.default=col_character()))%>%
  names()


# EMP Macro ---------------------------------------------------------------

EMP_Macro_file<-"macro_matrix.csv"
EMP_Macro_URL<-paste0("https://pasta.lternet.edu/package/data/eml/edi/522/", EMP_latest_revision, "/", EMP_entities[EMP_Macro_file])

Tryer(n=3, fun=download.file, url=EMP_Macro_URL,
             destfile=file.path(Data_folder, EMP_Macro_file), mode="wb", method="curl")


names_EMP_Macro<-readr::read_csv(file.path(Data_folder, EMP_Macro_file),
                                 col_types=cols(.default=col_character()))%>%
  names()


# FMWT STN Macro ----------------------------------------------------------

FMWTSTN_Macro_file<-FMWTSTN_files[grep("MysidNet", FMWTSTN_files)]

SMSCG_Macro_file<-SMSCG_files[grep("MysidNet", SMSCG_files)]

Tryer(n=3, fun=download.file, url=FMWTSTN_Macro_file,
           destfile=file.path(Data_folder, names(FMWTSTN_Macro_file)), mode="wb", method="curl")
Tryer(n=3, fun=download.file, url=SMSCG_Macro_file,
           destfile=file.path(Data_folder, names(SMSCG_Macro_file)), mode="wb", method="curl")

names_FMWT_Macro <- readr::read_csv(file.path(Data_folder, names(FMWTSTN_Macro_file)),
                                             col_types = cols(.default=col_character()))%>%
  names()

names_SMSCG_Macro <- readr::read_csv(file.path(Data_folder, names(SMSCG_Macro_file)),
                                              col_types = cols(.default=col_character()))%>%
  select(-starts_with("..."))%>% # Remove empty columns that are being read in here
  names()

# DOP Meso and Macro -------------------------------------------------------------------

DOP_Meso_file<-"DOP_ICF_Mesozooplankton_Abundance_2017-2022"
DOP_Meso_URL<-paste0(DOP_pkg_url, "/", DOP_entities[DOP_Meso_file])

DOP_trawls_file<-"DOP_ICF_TowData_2017-2022"
DOP_trawls_URL<-paste0(DOP_pkg_url, "/", DOP_entities[DOP_trawls_file])

DOP_Macro_file<-"DOP_ICF_Macrozooplankton_Abundance_2017-2022"
DOP_Macro_URL<-paste0(DOP_pkg_url, "/", DOP_entities[DOP_Macro_file])

#download the files
  Tryer(n=3, fun=utils::download.file, url=DOP_Meso_URL,
        destfile=file.path(Data_folder, DOP_Meso_file), mode="wb", method= "curl")

  Tryer(n=3, fun=utils::download.file, url=DOP_trawls_URL,
        destfile=file.path(Data_folder, DOP_trawls_file), mode="wb", method="curl")

  Tryer(n=3, fun=utils::download.file, url=DOP_Macro_URL,
        destfile=file.path(Data_folder, DOP_Macro_file), mode="wb", method="curl")

names_DOP_Meso<-readr::read_csv(file.path(Data_folder, DOP_Meso_file)) %>%
  names()
names_DOP_trawls<-readr::read_csv(file.path(Data_folder, DOP_trawls_file)) %>%
  names()

names_DOP_Macro<-readr::read_csv(file.path(Data_folder, DOP_Macro_file)) %>%
  names()


# Tests -------------------------------------------------------------------



test_that("EMP Meso column names have not changed", {
  expect_setequal(names_EMP_Meso, c('SurveyCode', 'Year', 'Survey', 'SurveyRep', 'SampleDate', 'StationNZ', 'EZStation', 'DWRStationNo',
                                    'Core', 'Time', 'Region', 'Secchi', 'Chl_a', 'Temperature', 'ECSurfacePreTow', 'ECBottomPreTow',
                                    'Volume', 'TowDuration', 'Depth', 'ACARTELA', 'ACARTIA', 'DIAPTOM', 'EURYTEM', 'OTHCALAD', 'PDIAPFOR', 'PDIAPMAR',
                                    'SINOCAL', 'TORTANUS', 'ALLCALADULTS', 'ACANTHO', 'LIMNOSPP', 'LIMNOSINE', 'LIMNOTET', 'OITHDAV',
                                    'OITHSIM', 'OITHSPP', 'OTHCYCAD', 'ALLCYCADULTS', 'HARPACT', 'CALJUV', 'EURYJUV', 'OTHCALJUV',
                                    'PDIAPJUV', 'SINOCALJUV', 'ASINEJUV', 'ACARJUV', 'DIAPTJUV', 'TORTJUV', 'ALLCALJUV', 'CYCJUV',
                                    'LIMNOJUV', 'OITHJUV', 'OTHCYCJUV', 'ALLCYCJUV', 'COPNAUP', 'EURYNAUP', 'OTHCOPNAUP', 'PDIAPNAUP',
                                    'SINONAUP', 'ALLCOPNAUP', 'BOSMINA', 'DAPHNIA', 'DIAPHAN', 'OTHCLADO', 'ALLCLADOCERA', 'ASPLANCH',
                                    'KERATELA', 'OTHROT', 'POLYARTH', 'SYNCH', 'SYNCHBIC', 'TRICHO', 'ALLROTIFER', 'BARNNAUP', 'CRABZOEA'))

})

test_that("FMWTSTN Meso column names have not changed", {
  expect_setequal(names_FMWTSTN_Meso, c('Project', 'Year', 'Survey', 'Month', 'Date', 'Station', 'Time', 'TowDuration', 'Region',
                                        'TideCode', 'DepthBottom', 'CondSurf', 'PPTSurf', 'CondBott', 'PPTBott', 'TempSurf',
                                        'TempBottom', 'Secchi', 'Turbidity', 'Microcystis', 'TotalMeter', 'MeterEstimate', 'Volume',
                                        'ACARTELA', 'ACARTIA', 'DIAPTOM', 'EURYTEM', 'OTHCALAD', 'PDIAPFOR', 'PDIAPMAR', 'SINOCAL',
                                        'TORTANUS', 'ALLCALADULTS', 'ACANTHO', 'LIMNOSPP', 'LIMNOSINE', 'LIMNOTET', 'OITHDAV', 'OITHSIM', 'OITHSPP',
                                        'OTHCYCAD', 'ALLCYCADULTS', 'HARPACT', 'EURYJUV', 'OTHCALJUV', 'PDIAPJUV', 'SINOCALJUV',
                                        'ASINEJUV', 'ACARJUV', 'DIAPTJUV', 'TORTJUV', 'ALLCALJUV', 'LIMNOJUV', 'OITHJUV', 'OTHCYCJUV',
                                        'ALLCYCJUV', 'EURYNAUP', 'OTHCOPNAUP', 'PDIAPNAUP', 'SINONAUP', 'ALLCOPNAUP', 'BOSMINA',
                                        'DAPHNIA', 'DIAPHAN', 'OTHCLADO', 'ALLCLADOCERA', 'ASPLANCH', 'KERATELA', 'OTHROT', 'POLYARTH',
                                        'SYNCH', 'TRICHO', 'ALLROTIFERS', 'BARNNAUP', 'CRABZOEA', 'OSTRACOD', 'CUMAC'))
})

test_that("SMSCG Meso column names have not changed", {
  expect_setequal(names_SMSCG_Meso, c('Project' , 'Year' , 'Survey' , 'Month' , 'Date' , 'Station' , 'Time' , 'TowDuration' , 'Region' ,
                                      'TideCode' , 'DepthBottom' , 'CondSurf' , 'PPTSurf' , 'CondBott' , 'PPTBott' , 'TempSurf' ,
                                      'TempBottom' , 'Secchi' , 'Turbidity' , 'Microcystis' , 'TotalMeter', 'MeterEstimate' , 'Volume' , 'ACARTELA' ,
                                      'ACARTIA' , 'DIAPTOM' , 'EURYTEM' , 'OTHCALAD' , 'PDIAPFOR' , 'PDIAPMAR' , 'SINOCAL' , 'TORTANUS' ,
                                      'ALLCALADULTS' , 'ACANTHO' , 'LIMNOSPP' , 'LIMNOSINE' , 'LIMNOTET' , 'OITHDAV' , 'OITHSIM' ,
                                      'OITHSPP' , 'OTHCYCAD' , 'ALLCYCADULTS' , 'HARPACT' , 'EURYJUV' , 'OTHCALJUV' , 'PDIAPJUV' ,
                                      'SINOCALJUV' , 'ASINEJUV' , 'ACARJUV' , 'DIAPTJUV' , 'TORTJUV' , 'ALLCALJUV' , 'LIMNOJUV' ,
                                      'OITHJUV' , 'OTHCYCJUV' , 'ALLCYCJUV' , 'EURYNAUP' , 'OTHCOPNAUP' , 'PDIAPNAUP' , 'SINONAUP' ,
                                      'ALLCOPNAUP' , 'BOSMINA' , 'DAPHNIA' , 'DIAPHAN' , 'OTHCLADO' , 'ALLCLADOCERA' , 'ASPLANCH' ,
                                      'KERATELA' , 'OTHROT' , 'POLYARTH' , 'SYNCH' , 'TRICHO' , 'ALLROTIFERS' , 'BARNNAUP' , 'CRABZOEA' ,
                                      'OSTRACOD' , 'CUMAC'))
})

test_that("20mm Meso column names have not changed", {
  expect_setequal(names_20mm_Meso, c('SampleDate', 'Survey', 'Station', 'TowNum', 'TowTime', 'Temp', 'TopEC', 'BottomEC', 'Secchi',
                                     'Turbidity', 'Tide', 'BottomDepth', 'Duration', 'MeterCheck', 'Volume', 'Dilution',
                                     'Acanthocyclops', 'Acanthomysis aspera', 'Acartia copepodid', 'Acartia spp_', 'Acartiella',
                                     'Acartiella copepodid', 'Annelid worms', 'Asplanchna', 'barnacle nauplii', 'Bosmina',
                                     'calanoid copepodid', 'Ceriodaphnia', 'Chironomid larvae', 'copepod nauplii', 'crab zoea',
                                     'Cumaceans', 'cyclopoid copepodid', 'Daphnia', 'Diaphanosoma', 'Diaptomus copepodid',
                                     'Diaptomus spp_', 'Eurytemora copepodid', 'Eurytemora nauplii', 'Eurytemora spp_',
                                     'Gammarus', 'Harpacticoids', 'Keratella', 'Limnoithona juvenile', 'Limnoithona sinensis',
                                     'Limnoithona spp_', 'Limnoithona tetraspina', 'Oithona davisae', 'Oithona juvenile',
                                     'Oithona similis', 'Oithona spp_', 'Osphranticum', 'Ostracods', 'Other calanoid',
                                     'Other cladocera', 'Other cyclopoid', 'Other Insect larvae', 'Other rotifer', 'Polyarthra',
                                     'Pseudodiaptomus copepodid', 'Pseudodiaptomus euryhalinus', 'Pseudodiaptomus forbesii',
                                     'Pseudodiaptomus marinus', 'Pseudodiaptomus nauplii', 'Pseudodiaptomus spp_', 'Sinocalanus',
                                     'Sinocalanus copepodid', 'Sinocalanus nauplii', 'Synchaeta', 'Synchaeta bicornis',
                                     'Tortanus', 'Tortanus copepodid', 'Tortanus dextrilobatus', 'Tortanus discaudatus',
                                     'Trichocerca', 'UnID calanoid', 'UnID cladocera', 'Unid rotifer', 'Sum all Pseudodiaptomus Adult',
                                     'Sum all Calanoid Copepodids (juv)', 'Sum all Cyclopoid Copepodids (juv)',
                                     'Sum all Copepod Nauplii', 'Sum all Limnoithona Adult', 'Sum all Tortanus Adult', 'Sum all Rotifers'))
})

test_that("FRP Meso column names have not changed", {
  expect_setequal(names_FRP_Meso, c( "SampleID_key","SampleID_frp","CommonName", "VisitNo","Location", "Date",
                                     "subsample" ,
                                      "Count" , "AdjCount" ,"CPUE","Flagged_Data", "StartTime",
                                     "EndTime",  "LatitudeStart", "LatitudeEnd", "LongitudeStart", "LongitudeEnd",
                                     "DepthOfSample", "DepthOfWater", "NetMeterEnd", "TowDirection", "NetMeterStart",
                                     "PercentOpen", "DetritalVolume", "GearTypeAbbreviation", "effort",   "LAB_NAME",
                                      "Comments"))
})

test_that("YBFMP column names have not changed", {
  expect_setequal(names_YBFMP, c('Date', 'Time', 'Datetime', 'StationCode', 'WeatherCode', 'Tide', 'WY', 'WaterTemperature',
                                 'Secchi', 'Conductivity', 'SpCnd', 'pH', 'DO', 'Turbidity', 'MicrocystisVisualRank',
                                 'ConditionCode', 'FieldComments', 'LabComments', 'MeshSize', 'FlowMeterSpeed', 'SetTime',
                                 'FlowMeterStart', 'FlowMeterEnd', 'Flowdiff', 'Flowdiff_ed', 'VolMeso', 'SubMeso',
                                 'VolMicro', 'SubMicro', 'Subsample', 'PropSubsampled', 'VolNet', 'VolNet_ed', 'OrganismID',
                                 'TaxonName', 'TaxonRank', 'LifeStage', 'Count', 'CPUE', 'CPUE_ed', 'Flag_PQC', 'Comment_PQC',
                                 'Flag_QC1', 'Comment_QC1', 'Flag_QC2', 'Comment_QC2', 'Flag_QC3', 'Comment_QC3'))
})

test_that("EMP Micro column names have not changed", {
  expect_setequal(names_EMP_Micro, c('SurveyCode', 'Year', 'Survey', 'SurveyRep', 'SampleDate', 'StationNZ', 'EZStation',
                                     'DWRStationNo', 'Core', 'Region', 'Secchi', 'Chl_a', 'Temperature', 'ECSurfacePreTow',
                                     'ECBottomPreTow', 'Volume', "Depth", 'LIMNOSINE', 'LIMNOSPP', 'LIMNOTET', 'TotalLimno',
                                     'OITHDAV', 'OITHSIM', 'OITHSPP', 'OTHCYCAD', 'ALLCYCADULTS', 'HARPACT', 'CYCJUV',
                                     'LIMNOJUV', 'OITHJUV', 'OTHCYCJUV', 'ALLCYCJUV', 'COPNAUP', 'EURYNAUP', 'OTHCOPNAUP',
                                     'PDIAPNAUP', 'SINONAUP', 'ALLCOPNAUP', 'ASPLANCH', 'KERATELA', 'OTHROT', 'POLYARTH',
                                     'SYNCH', 'SYNCHBIC', 'TRICHO', 'ALLROTIFER', 'BARNNAUP'))
})

test_that("FRP Macro column names have not changed", {
  expect_setequal(names_FRP_Macro, c("SampleID_key","SampleID_frp", "CommonName", "VisitNo", "subsample",
                                     "Count", "AdjCount", "Flagged_Data", "StartTime", "EndTime",
                                     "LatitudeStart", "LatitudeEnd", "LongitudeStart", "LongitudeEnd",  "DepthOfSample",
                                     "DepthOfWater", "NetMeterEnd", "TowDirection", "NetMeterStart",        "Boulder",
                                     "Cobble", "Gravel", "Organics", "Sand", "Silt",
                                     "PercentOpen", "DetritalVolume", "GearTypeAbbreviation", "LAB_NAME",   "Location",
                                     "Date","Comments", "effort", "CPUE"))
})

test_that("FRP site data column names have not changed", {
  expect_setequal(names_FRP_sites, c("VisitNo", "Location",
                                     "Date","Comments", "effort","CPUE" ,       "Temp",
                                     "SC", "pH", "DO", "Turbidity", "Chlorophyll",
                                     "Phycocyanin", "FDOM","Secchi",  "Microcystis", "Tide",
                                     "Weather","WindWaves"))

})

test_that("EMP Macro column names have not changed", {
  expect_setequal(names_EMP_Macro, c('SurveyCode', 'Year', 'Survey', 'SurveyRep', 'SampleDate', 'StationNZ', 'EZStation',
                                     'DWRStationNo', 'Core', 'Region', 'Time', 'TowDuration', 'Depth', 'Secchi', 'Chl_a', 'Temperature', 'ECSurfacePreTow',
                                     'ECBottomPreTow', 'Volume', 'AmphipodCode', 'A_aspera', 'A_hwanhaiensis', 'A_macropsis',
                                     'D_holmquistae', 'H_longirostris', 'N_kadiakensis', 'N_mercedis', 'Unidentified_mysid',
                                     "Mysid_Total", "A_spinicorne", "A_stimpsoni", "A_abdita", "Ampithoe_sp", "Caprelidae_sp",
                                     "C_alienense", "Crangonyx_sp", "G_daiberi", "G_japonica", "Hyalella_sp", "Monocorophium_sp",
                                     "Oedicerotidae_sp", "Pleustidae", "Unidentified_Amphipod", "Unidentified_Corophium",
                                     "Unidentified_Gammarus", "Amphipod_Total"))
})

test_that("FMWT Macro column names have not changed", {
  expect_setequal(names_FMWT_Macro, c('Project', 'Year', 'Survey', 'Month', 'Date', 'Station', 'Time', 'MeterEstimate',
                                            'TowDuration', 'Region', 'TideCode', 'DepthBottom', 'CondSurf',
                                            'PPTSurf', 'CondBott', 'PPTBott', 'TempSurf', 'TempBottom', 'Secchi', 'Turbidity', 'Microcystis',
                                            'TotalMeter', 'Volume', 'Acanthomysis_aspera', 'Hyperacanthomysis_longirostris',
                                            'Acanthomysis_hwanhaiensis', 'Alienacanthomysis_macropsis', 'Deltamysis_holmquistae',
                                            'Neomysis_kadiakensis', 'Neomysis_mercedis', 'Unidentified_Mysid', 'Americorophium_spinicorne',
                                            'Americorophium_stimpsoni', 'Ampelisca_abdita', 'Corophium_alienense', 'Crangonyx_sp',
                                            'Gammarus_daiberi', 'Hyalella_sp', 'Unidentified_Amphipod', 'Unidentified_Corophium',
                                            'Unidentified_Gammarus', 'Grandidierella_japonica'))
})

test_that("SMSCG Macro Mysid column names have not changed", {
  expect_setequal(names_SMSCG_Macro, c('Project', 'Year', 'Survey', 'Month', 'Date', 'Station', 'Time', 'MeterEstimated',
                                             'TowDuration', 'Region', 'TideCode', 'DepthBottom', 'CondSurf',
                                             'PPTSurf', 'CondBott', 'PPTBott', 'TempSurf', 'TempBottom', 'Secchi', 'Turbidity', 'Microcystis',
                                             'TotalMeter', 'Volume', 'Acanthomysis_aspera', 'Hyperacanthomysis_longirostris',
                                             'Acanthomysis_hwanhaiensis', 'Alienacanthomysis_macropsis', 'Deltamysis_holmquistae',
                                             'Neomysis_kadiakensis', 'Neomysis_mercedis', 'Unidentified_Mysid', 'Americorophium_spinicorne',
                                             'Americorophium_stimpsoni', 'Ampelisca_abdita', 'Corophium_alienense', 'Crangonyx_sp',
                                             'Gammarus_daiberi', 'Hyalella_sp', 'Unidentified_Amphipod', 'Unidentified_Corophium',
                                             'Unidentified_Gammarus', 'Grandidierella_japonica'))})


  test_that("DOP Meso column names have not changed", {
    expect_setequal(names_DOP_Meso, c( "ICF_ID", "Acanthocyclops_spp_adult","Acanthocyclops_vernalis_adult",
                                       "Acanthocyclops_vernalis_copepodid","Acartia_spp_adult","Acartia_spp_copepodid" ,
                                       "Acartiella_sinensis_adult",          "Acartiella_sinensis_copepodid",      "Asplanchna_spp",
                                        "Barnacle_UNID_nauplii",              "Bosmina_longirostris", "Brachionidae_UNID",
                                        "Brachionus_spp", "Calanoid_UNID_adult" , "Calanoid_UNID_copepodid",
                                        "Camptocercus_spp" ,  "Chydoridae_UNID",                    "Chydorus_spp",
                                        "Cladocera_UNID", "Copepod_UNID_nauplii", "Crab_UNID_zoea",
                                        "Cyclopoid_UNID_adult","Cyclopoid_UNID_copepodid",           "Daphnia_spp" ,
                                       "Daphniidae_UNID" ,"Diaptomidae_UNID_adult" ,"Diaptomidae_UNID_copepodid" ,
                                        "Ditrichocorycaeus_affinis_adult",    "Eurytemora_affinis_adult", "Eurytemora_affinis_copepodid",
                                       "Eurytemora_spp_nauplii" , "Harpacticoid_UNID",  "Holopedium_gibberum" ,
                                       "Ilyocryptus_spp","Keratella_spp","Labidocera_spp_adult",
                                       "Labidocera_spp_copepodid", "Leptodora_spp", "Limnoithona_sinensis_adult",
                                       "Limnoithona_sinensis_copepodid",     "Limnoithona_spp_adult",              "Limnoithona_spp_copepodid",
                                       "Limnoithona_tetraspina_adult",      "Limnoithona_tetraspina_copepodid",   "Macrothrix_spp",
                                       "Moina_spp",                          "Oithona_davisae_adult",             "Oithona_davisae_copepodid",
                                       "Oithona_similis_adult",             "Oithona_similis_copepodid",         "Oithona_spp_adult",
                                       "Oithona_spp_copepodid",             "Osphranticum_labronectum_adult",    "Osphranticum_labronectum_copepodid",
                                       "Ostracoda_UNID",                    "Paracalanus_parvus_adult",          "Paracalanus_parvus_copepodid",
                                       "Platyias_spp",                      "Podonidae_UNID",                    "Polyarthra_spp",
                                       "Pseudodiaptomus_euryhalinus_adult", "Pseudodiaptomus_forbesi_adult",     "Pseudodiaptomus_forbesi_copepodid",
                                       "Pseudodiaptomus_marinus_adult",     "Pseudodiaptomus_marinus_copepodid", "Pseudodiaptomus_spp_adult",
                                       "Pseudodiaptomus_spp_copepodid",     "Pseudodiaptomus_spp_nauplii",       "Rotifer_UNID",
                                       "Scapholeberis_spp",                 "Sididae_UNID",                      "Sinocalanus_doerrii_adult",
                                       "Sinocalanus_doerrii_copepodid",     "Sinocalanus_doerrii_nauplii",       "Synchaeta_bicornis",
                                       "Synchaeta_spp",                     "Tortanus_dextrilobatus_adult",      "Tortanus_discaudatus_adult",
                                       "Tortanus_spp_copepodid",            "Trichocerca_spp"))
})

test_that("DOP Macro column names have not changed", {
    expect_setequal(names_DOP_Macro, c( "ICF_ID", "Alienacanthomysis_macropsis",   "Americorophium_spinicorne",     "Americorophium_spp",
                                       "Americorophium_stimpsoni",      "Ampelisca_abdita",              "Amphipod_UNID",                 "Ampithoe_spp",
                                       "Ampithoe_valida",               "Corophiidae_UNID",              "Crangonyx_spp",                 "Cumacean_UNID",
                                       "Deltamysis_holmquistae",        "Dexaminidae_UNID",              "Eogammarus_spp",                "Exopalaemon_spp",
                                       "Gammarus_daiberi",              "Grandidierella_japonica",       "Grandifoxus_grandis",           "Hyalella_spp",
                                       "Hyperacanthomysis_longirostris","Isopoda_UNID",                  "Monocorophium_acherusicum",     "Mysid_UNID",
                                       "Neomysis_kadiakensis",          "Neomysis_mercedis",             "Oedicerotidae_UNID",            "Orientomysis_aspera",
                                       "Orientomysis_hwanhaiensis",     "Pleustidae_UNID",               "Shrimp_UNID_larvae",
                                       "Sinocorophium_alienense",
                                       "Tanaidacea_UNID","Sinocorophium_alienense"))
})

test_that("DOP Trawl column names have not changed", {
  expect_setequal(names_DOP_trawls, c("ICF_ID", "Date" ,"Start_Time",
                                     "Station_Code" ,"Habitat","Region_GIS" ,
                                      "Latitude","Longitude","Start_Depth" ,
                                      "Temperature" , "Conductivity","Turbidity",
                                      "pH", "Salinity", "DO"  ,
                                      "Chl_a","Secchi" , "NO3",
                                      "NH4", "PO4","DOC",
                                      "End_Depth", "End_Time","Microcystis_2017" ,
                                      "Microcystis","Warp_Length","Warp_Angle"  ,
                                      "TowTime","Macrozooplankton_Volume" ,"Mesozooplankton_Volume" ))
})
