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
