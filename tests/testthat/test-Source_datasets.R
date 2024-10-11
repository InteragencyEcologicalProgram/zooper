suppressWarnings({
  require(zooper)
  require(readr)
  require(dplyr)
})

Download_method<-"auto"


URLs<-zoop_urls(c("EMP", "FMWT", "STN",
                  "20mm", "FRP", "YBFMP", "DOP"))

Data_folder<-tempdir()


# EMP Meso ----------------------------------------------------------------

Tryer(n=3, fun=download.file, url=URLs$EMP$Meso, destfile=file.path(Data_folder, "EMP_Meso.csv"), mode="wb", method=Download_method)


names_EMP_Meso<-readr::read_csv(file.path(Data_folder, "EMP_Meso.csv"), col_types = cols(.default=col_character()))%>%
  names()


# FMWT STN Meso -----------------------------------------------------------

Tryer(n=3, fun=download.file, url=URLs$FMWTSTN$Meso,
      destfile=file.path(Data_folder, "FMWTSTN_Meso.csv"), mode="wb", method=Download_method)

Tryer(n=3, fun=download.file, url=URLs$SMSCG$Meso,
      destfile=file.path(Data_folder, "SMSCG_Meso.csv"), mode="wb", method=Download_method)

names_FMWTSTN_Meso<-readr::read_csv(file.path(Data_folder, "FMWTSTN_Meso.csv"),
                                    col_types = "c")%>%
  names()

names_SMSCG_Meso<-readr::read_csv(file.path(Data_folder, "SMSCG_Meso.csv"),
                                  col_types = "c")%>%
  names()


# 20mm Meso ---------------------------------------------------------------

Tryer(n=3, fun=download.file, url=URLs$twentymm$Meso,
      destfile=file.path(Data_folder, "twentymm_Meso.csv"), mode="wb", method=Download_method)

names_20mm_Meso<-readxl::read_excel(file.path(Data_folder, "twentymm_Meso.csv"),
                                    sheet="20-mm CB CPUE Data",
                                    col_types = "text")%>%
  names()


# FRP ----------------------------------------------------------------

Tryer(n=3, fun=utils::download.file, url=URLs$FRP$Meso,
      destfile=file.path(Data_folder, "zoopsFRP.csv"), mode="wb", method=Download_method)
Tryer(n=3, fun=utils::download.file, url=URLs$FRP$Macro,
      destfile=file.path(Data_folder, "macroinvert_FRP.csv"), mode="wb", method=Download_method)
Tryer(n=3, fun=utils::download.file, url=URLs$FRP$site,
      destfile=file.path(Data_folder, "sitesFRP.csv"), mode="wb", method=Download_method)

names_FRP_Meso <- readr::read_csv(file.path(Data_folder, "zoopsFRP.csv"), na=c("", "NA"))%>%
  names()
names_FRP_Macro <- readr::read_csv(file.path(Data_folder, "macroinvert_FRP.csv"), na=c("", "NA"))%>%
  names()
names_FRP_sites <- readr::read_csv(file.path(Data_folder, "sitesFRP.csv"), na=c("", "NA"))%>%
  names()

# YBFMP Meso/Micro --------------------------------------------------------

Tryer(n=3, fun=utils::download.file, url=URLs$YBFMP,
      destfile=file.path(Data_folder, "YBFMP.csv"), mode="wb", method=Download_method)

names_YBFMP<-readr::read_csv(file.path(Data_folder, "YBFMP.csv"),
                             col_types = cols(.default=col_character()))%>%
  names()


# EMP Micro ---------------------------------------------------------------

Tryer(n=3, fun=download.file, url=URLs$EMP$Micro,
      destfile=file.path(Data_folder, "EMP_Micro.csv"), mode="wb", method=Download_method)

names_EMP_Micro<-readr::read_csv(file.path(Data_folder, "EMP_Micro.csv"),
                                 col_types=cols(.default=col_character()))%>%
  names()


# EMP Macro ---------------------------------------------------------------

Tryer(n=3, fun=download.file, url=URLs$EMP$Macro,
      destfile=file.path(Data_folder, "EMP_Macro.csv"), mode="wb", method=Download_method)


names_EMP_Macro<-readr::read_csv(file.path(Data_folder, "EMP_Macro.csv"),
                                 col_types=cols(.default=col_character()))%>%
  names()


# EMP Lengths ---------------------------------------------------------------

Tryer(n=3, fun=download.file, url=URLs$EMP$Lengths,
      destfile=file.path(Data_folder, "EMP_Lengths.csv"), mode="wb", method=Download_method)


names_EMP_Lengths<-readr::read_csv(file.path(Data_folder, "EMP_Lengths.csv"),
                                 col_types=cols(.default=col_character()))%>%
  select(-where(~all(is.na(.x))))%>% # drop empty columns
  names()

# FMWT STN Macro ----------------------------------------------------------

Tryer(n=3, fun=download.file, url=URLs$FMWTSTN$Macro,
      destfile=file.path(Data_folder, "FMWTSTN_Macro.csv"), mode="wb", method=Download_method)
Tryer(n=3, fun=download.file, url=URLs$SMSCG$Macro,
      destfile=file.path(Data_folder, "SMSCG_Macro.csv"), mode="wb", method=Download_method)

names_FMWT_Macro <- readr::read_csv(file.path(Data_folder, "FMWTSTN_Macro.csv"),
                                    col_types = cols(.default=col_character()))%>%
  names()

names_SMSCG_Macro <- readr::read_csv(file.path(Data_folder, "SMSCG_Macro.csv"),
                                     col_types = cols(.default=col_character()))%>%
  select(-starts_with("..."))%>% # Remove empty columns that are being read in here
  names()

# DOP Meso and Macro -------------------------------------------------------------------

#download the files
Tryer(n=3, fun=utils::download.file, url=URLs$DOP$Meso,
      destfile=file.path(Data_folder, "DOP_Meso.csv"), mode="wb", method= Download_method)

Tryer(n=3, fun=utils::download.file, url=URLs$DOP$trawls,
      destfile=file.path(Data_folder, "DOP_trawls.csv"), mode="wb", method=Download_method)

Tryer(n=3, fun=utils::download.file, url=URLs$DOP$Macro,
      destfile=file.path(Data_folder, "DOP_Macro.csv"), mode="wb", method=Download_method)

names_DOP_Meso<-readr::read_csv(file.path(Data_folder, "DOP_Meso.csv")) %>%
  names()
names_DOP_trawls<-readr::read_csv(file.path(Data_folder, "DOP_trawls.csv")) %>%
  names()

names_DOP_Macro<-readr::read_csv(file.path(Data_folder, "DOP_Macro.csv")) %>%
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
  expect_setequal(names_20mm_Meso, c('SampleDate', 'Year', 'Survey', 'Station', 'TowNum', 'TowTime', 'Temp', 'TopEC', 'BottomEC', 'Secchi',
                                     'NTU','FNU', 'Tide', 'BottomDepth', 'Duration', 'MeterCheck', 'Volume', 'Dilution',
                                     'Acanthocyclops', 'Acanthomysis aspera', 'Acartia copepodid', 'Acartia spp_', 'Acartiella',
                                     'Acartiella copepodid', 'Annelid worms', 'Asplanchna', 'Barnacle nauplii', 'Bosmina',
                                     'Calanoid copepodid', 'Ceriodaphnia', 'Chironomid larvae', 'Copepod nauplii', 'Crab zoea',
                                     'Cumaceans', 'Cyclopoid copepodid', 'Daphnia', 'Diaphanosoma', 'Diaptomus copepodid',
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
                                     "Count" , "AdjCount" ,"CPUE","Flagged_Data_zoops", "StartTime",
                                     "EndTime",  "LatitudeStart", "LatitudeEnd", "LongitudeStart", "LongitudeEnd",
                                    "DepthOfWater", "NetMeterEnd", "TowDirection", "NetMeterStart",
                                    "GearTypeAbbreviation", "effort",   "LAB_NAME",
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
                                     "Count", "AdjCount", "Flagged_Data_macroinvert", "StartTime", "EndTime",
                                     "LatitudeStart", "LatitudeEnd", "LongitudeStart", "LongitudeEnd",  "DepthOfSample",
                                     "DepthOfWater", "NetMeterEnd", "TowDirection", "NetMeterStart",        "Boulder",
                                     "Cobble", "Gravel", "Organics", "Sand", "Silt", "DetritalVolume",
                                     "GearTypeAbbreviation", "LAB_NAME",   "Location",
                                     "Date","Comments", "effort", "CPUE"))
})

test_that("FRP site data column names have not changed", {
  expect_setequal(names_FRP_sites, c('VisitNo', 'Location', 'Date', 'Temp', 'SC', 'pH', 'DO',
                                     'Turbidity', 'Chlorophyll', 'Phycocyanin', 'FDOM', 'Secchi',
                                     'Microcystis', 'Tide', 'Weather', 'WindWaves', 'Flagged_Data'))

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

test_that("FRP Length column names have not changed", {
  expect_setequal(names_EMP_Lengths, c('SampleDate', 'StationNZ', 'SpeciesName', 'Sex', 'Size', 'Frequency', 'AdjustedFreq'))
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
