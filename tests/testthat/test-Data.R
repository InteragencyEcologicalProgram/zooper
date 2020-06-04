library(zooper)

Data_sets <- c("EMP_Meso", "FMWT_Meso", "STN_Meso",
               "twentymm_Meso", "FRP_Meso","EMP_Micro",
               "FRP_Macro", "EMP_Macro", "FMWT_Macro"#, "STN_Macro"
               )

test_that("zoopComb includes all datasets", {
  expect_setequal(unique(paste(zoopComb$Source, zoopComb$SizeClass, sep="_")), Data_sets)
})

test_that("No samples are duplicated in zoopComb", {
  expect_equal(length(unique(paste(zoopComb$SampleID, zoopComb$Taxlifestage, zoopComb$SizeClass))), nrow(zoopComb))
})

test_that("No samples are duplicated in zoopEnvComb", {
  expect_equal(length(unique(zoopEnvComb$SampleID)), nrow(zoopEnvComb))
})

test_that("Same samples present in Zooplankton and Environment datasets", {
  expect_setequal(unique(zoopComb$SampleID), unique(zoopEnvComb$SampleID))
})

test_that("Not all CPUEs are 0", {
  expect_gt(nrow(dplyr::filter(zoopComb, CPUE>0)), 0)
})
