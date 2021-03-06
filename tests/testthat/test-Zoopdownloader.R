library(zooper)

Data_sets <- c("EMP_Meso", "FMWT_Meso", "STN_Meso",
              "20mm_Meso", "FRP_Meso","EMP_Micro",
              "FRP_Macro", "EMP_Macro", "FMWT_Macro", "STN_Macro"
              )

Data <- Zoopdownloader(Data_folder = tempdir(), Return_object = TRUE,
                         Save_object = FALSE, Redownload_data = TRUE,
                         Data_sets = Data_sets)

No_coords2<-dplyr::filter(Data$Environment, is.na(Latitude) & !stringr::str_detect(Station, "NZEZ"))%>%
  dplyr::mutate(Station=paste(Source, Station))

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
  expect_setequal(unique(No_coords2$Station), c("20mm 798", "20mm 799", "20mm 794", "20mm 795", "20mm 796", "20mm 797"))
})

test_that("Date and Datetime and displaying the same dates", {
  expect_true(all(lubridate::as_date(Data$Environment$Date)==lubridate::as_date(Data$Environment$Datetime) | is.na(Data$Environment$Datetime)))
})

test_that("Bottom depths are within reasonable limits", {
  #expect_true(all(Data$Environment$BottomDepth > 0.5 | is.na(Data$Environment$BottomDepth)))
  expect_true(all(Data$Environment$BottomDepth < 35 | is.na(Data$Environment$BottomDepth)))
})

test_that("There are no NA Volumes in Zoopdownloader output", {
  expect_equal(length(which(is.na(Data$Zooplankton$Volume))), 0)
})
