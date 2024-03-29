suppressWarnings({
  library(zooper)
  require(dplyr)
  require(stringr)
  require(lubridate)
})

Data_sets <- c("EMP_Meso", "FMWT_Meso", "STN_Meso",
               "20mm_Meso", "FRP_Meso","EMP_Micro",
               "FRP_Macro", "EMP_Macro", "FMWT_Macro",
               "STN_Macro", "YBFMP_Meso", "YBFMP_Micro", "DOP_Meso", "DOP_Macro"
)

No_coords<-filter(zoopEnvComb, is.na(Latitude) & !str_detect(Station, "NZEZ"))%>%
  mutate(Station=paste(Source, Station))

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

test_that("Only the expected stations are missing coordinates", {
  expect_setequal(unique(No_coords$Station), c("20mm 798", "20mm 799", "20mm 794", "20mm 795",
                                               "20mm 796", "20mm 797", "DOP 19-11-LSC-01", "DOP 19-11-LSC-02"))
})

test_that("Date and Datetime and displaying the same dates", {
  expect_true(all(as_date(zoopEnvComb$Date)==as_date(zoopEnvComb$Datetime) | is.na(zoopEnvComb$Datetime)))
})

test_that("Timezone is correct", {
  expect_true(all(tz(zoopEnvComb$Datetime)=="America/Los_Angeles"))
})

test_that("Very few times are before 5AM or after 6PM", {
  expect_lt(nrow(filter(zoopEnvComb, hour(Datetime)<5)), 60)
  expect_lt(nrow(filter(zoopEnvComb, hour(Datetime)>18)), 60)
})

test_that("Bottom depths are within reasonable limits", {
  expect_true(all(zoopEnvComb$BottomDepth > 0.2 | is.na(zoopEnvComb$BottomDepth)))
  expect_true(all(zoopEnvComb$BottomDepth < 35 | is.na(zoopEnvComb$BottomDepth)))
})
#One DOP station has a depth of 0.21 m.

test_that("There are no NA Volumes in zoopComb", {
  expect_equal(length(which(is.na(zoopComb$Volume))), 0)
})


test_that("TowType only has the expected levels", {
  expect_setequal(zoopEnvComb$TowType, c("Surface", "Bottom", "Oblique", "Vertical pump"))
})
