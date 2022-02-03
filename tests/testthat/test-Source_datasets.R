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


# FRP Meso ----------------------------------------------------------------

Tryer(n=3, fun=download.file, url="https://pasta.lternet.edu/package/data/eml/edi/269/2/d4c76f209a0653aa86bab1ff93ab9853",
           destfile=file.path(Data_folder, "zoopsFRP2018.csv"), mode="wb", method="curl")

names_FRP_Meso <- readr::read_csv(file.path(Data_folder, "zoopsFRP2018.csv"),
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


# FRP Macro ---------------------------------------------------------------

Tryer(n=3, fun=download.file, url="https://pasta.lternet.edu/package/data/eml/edi/269/2/630f16b33a9cbf75f1989fc18690a6b3",
           destfile=file.path(Data_folder, "bugsFRP2018.csv"), mode="wb", method="curl")

names_FRP_Macro <- readr::read_csv(file.path(Data_folder, "bugsFRP2018.csv"),
                                   col_types = cols(.default=col_character()))%>%
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
                                      'TempBottom' , 'Secchi' , 'Turbidity' , 'Microcystis' , 'TotalMeter' , 'Volume' , 'ACARTELA' ,
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
  expect_setequal(names_FRP_Meso, c('SampleID', 'Date', 'time', 'Latitude', 'Longitude', 'Temp', 'SC', 'pH', 'DO', 'Secchi',
                                    'Turbidity', 'Tide', 'Microcystis', 'CommonName', 'subsample', 'volume', 'Count',
                                    'AdjCount', 'CPUE', 'Station'))
})

test_that("EMP Micro column names have not changed", {
  expect_setequal(names_EMP_Micro, c('SurveyCode', 'Year', 'Survey', 'SurveyRep', 'SampleDate', 'StationNZ', 'EZStation',
                                     'DWRStationNo', 'Core', 'Region', 'Secchi', 'Chl_a', 'Temperature', 'ECSurfacePreTow',
                                     'ECBottomPreTow', 'Volume', "Depth", 'LIMNOSINE', 'LIMNOSPP', 'LIMNOTET', 'TotalLimno',
                                     'OITHDAV', 'OITHSIM', 'OITHSPP', 'OTHCYCAD', 'ALLCYCADULTS', 'HARPACT', 'CYCJUV',
                                     'LIMNOJUV', 'OITHJUV', 'OTHCYCJUV', 'ALLCYCJUV', 'COPNAUP', 'EURYNAUP', 'OTHCOPNAUP',
                                     'PDIAPNAUP', 'SINONAUP', 'ALLCOPNAUP', 'ASPLANCH', 'KERATELA', 'OTHROT', 'POLYARTH',
                                     'SYNCH', 'SYNCHBIC', 'TRICHO', 'ALLROTIFERS', 'BARNNAUP'))
})

test_that("FRP Macro column names have not changed", {
  expect_setequal(names_FRP_Macro, c('SampleID', 'Date', 'time', 'Sampletype', 'Latitude', 'Longitude', 'Temp',
                                     'SC', 'pH', 'DO', 'Secchi', 'Turbidity', 'Tide', 'Microcystis', 'volume', 'subsample',
                                     'VegWeight', 'CommonName', 'Count', 'AdjCount', 'Station'))
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
  expect_setequal(names_FMWT_Macro, c('Project', 'Year', 'Survey', 'Month', 'Date', 'Station', 'Time', 'MeterEstimated',
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
                                             'Unidentified_Gammarus', 'Grandidierella_japonica'))
})
