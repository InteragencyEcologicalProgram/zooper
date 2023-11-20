library(zooper)

Data_sets <- c("EMP_Meso", "FMWT_Meso", "STN_Meso",
               "20mm_Meso", "FRP_Meso","EMP_Micro",
               "FRP_Macro", "EMP_Macro", "FMWT_Macro",
               "STN_Macro", "YBFMP_Meso", "YBFMP_Micro",
               "DOP_Meso", "DOP_Macro")

Data <- Zoopdownloader(Data_folder = tempdir(), Return_object = TRUE,
                       Save_object = FALSE, Redownload_data = TRUE,
                       Biomass=TRUE, Data_sets = Data_sets)

No_coords2<-dplyr::filter(Data$Environment, is.na(Latitude) & !stringr::str_detect(Station, "NZEZ"))%>%
  dplyr::mutate(Station=paste(Source, Station))

No_coords_EZ<-dplyr::filter(Data$Environment, is.na(Latitude) & stringr::str_detect(Station, "NZEZ") & Date>min(stationsEMPEZ$Date))%>%
  dplyr::mutate(Station=paste(Station, Date))

No_stations<-dplyr::filter(Data$Environment, is.na(Station))%>%
  dplyr::mutate(ID=paste(Source, Date))%>%
  dplyr::pull(ID)

test_that("Dowloaded data includes all datasets", {
  expect_setequal(unique(paste(Data$Zooplankton$Source, Data$Zooplankton$SizeClass, sep="_")), Data_sets)
})

test_that("No samples duplicated", {
  expect_equal(length(unique(paste(Data$Zooplankton$SampleID, Data$Zooplankton$Taxlifestage, Data$Zooplankton$SizeClass))), nrow(Data$Zooplankton))
})

test_that("Same samples present in Zooplankton and Environment datasets", {
  expect_setequal(unique(Data$Zooplankton$SampleID), unique(Data$Environment$SampleID))
})

test_that("Not all CPUEs are 0", {
  expect_gt(nrow(dplyr::filter(Data$Zooplankton, CPUE>0)), 0)
})

test_that("Only the expected stations are missing coordinates", {
  expect_setequal(unique(No_coords2$Station), c("20mm 798", "20mm 799", "20mm 794", "20mm 795", "20mm 796", "20mm 797", "DOP 19-11-LSC-01", "DOP 19-11-LSC-02"))
})

test_that("Only the expected EZ stations are missing coordinates", {
  expect_setequal(unique(No_coords_EZ$Station), c("NZEZ6 2004-12-21", "NZEZ2 2007-08-21", "NZEZ6 2007-08-21"))
})

test_that("All rowshave station names", {
  expect_setequal(No_stations, character(0))
})

test_that("Date and Datetime and displaying the same dates", {
  expect_true(all(lubridate::as_date(Data$Environment$Date)==lubridate::as_date(Data$Environment$Datetime) | is.na(Data$Environment$Datetime)))
})

test_that("Bottom depths are within reasonable limits", {
  expect_true(all(Data$Environment$BottomDepth > 0.2 | is.na(Data$Environment$BottomDepth)))
  expect_true(all(Data$Environment$BottomDepth < 35 | is.na(Data$Environment$BottomDepth)))
})

test_that("There are no NA Volumes, Dates, or Stations in Zoopdownloader output", {
  expect_equal(length(which(is.na(Data$Zooplankton$Volume))), 0)
  expect_equal(length(which(is.na(Data$Environment$Date))), 0)
})

test_that("TowType only has the expected levels", {
  expect_setequal(Data$Environment$TowType, c("Surface", "Bottom", "Oblique", "Vertical pump"))
})

# BPUE

test_that("When biomass is 0, CPUE is 0", {
  expect_equal(unique(filter(Data$Zooplankton, SizeClass=="Macro" & BPUE==0)$CPUE), 0)
})

test_that("When CPUE is 0, Biomass is 0", {
  expect_equal(unique(filter(Data$Zooplankton, SizeClass=="Macro" & CPUE==0 &
                               !is.na(Volume) & Source=="EMP" &
                               Taxlifestage%in%c("Hyperacanthomysis longirostris Adult", "Neomysis mercedis Adult"))$BPUE), 0)
})

test_that("The only Macro masses are from EMP", {
  expect_equal(unique(filter(Data$Zooplankton, SizeClass=="Macro" &  !is.na(BPUE))$Source), "EMP")
})

test_that("The only Macro species with biomasses are those we expect", {
  expect_setequal(unique(filter(Data$Zooplankton, SizeClass=="Macro" &  !is.na(BPUE))$Taxlifestage),
                  c("Hyperacanthomysis longirostris Adult", "Neomysis mercedis Adult"))
})
