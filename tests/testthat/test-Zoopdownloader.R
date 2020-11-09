library(zooper)

Data_sets <- c("EMP_Meso", "FMWT_Meso", "STN_Meso",
              "20mm_Meso", "FRP_Meso","EMP_Micro",
              "FRP_Macro", "EMP_Macro", "FMWT_Macro", "STN_Macro"
              )
Data_sets2 <- Data_sets
Data_sets2[which(Data_sets2=="20mm_Meso")] <- "twentymm_Meso"

Data <- Zoopdownloader(Data_folder = tempdir(), Return_object = TRUE,
                         Save_object = FALSE, Redownload_data = TRUE,
                         Data_sets = Data_sets)

No_coords2<-filter(Data$Environment, is.na(Latitude) & !str_detect(Station, "NZEZ"))%>%
  mutate(Station=paste(Source, Station))

test_that("Dowloaded data includes all datasets", {
  expect_setequal(unique(paste(Data$Zooplankton$Source, Data$Zooplankton$SizeClass, sep="_")), Data_sets2)
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
  expect_setequal(unique(No_coords2$Station), c("FMWT 520", "twentymm 798", "twentymm 799", "twentymm 794", "twentymm 795", "twentymm 796", "twentymm 797"))
})

test_that("Date and Datetime and displaying the same dates", {
  expect_true(all(as_date(Data$Environment$Date)==as_date(Data$Environment$Datetime) | is.na(Data$Environment$Datetime)))
})
