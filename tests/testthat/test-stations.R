Stations<-zooper::stations
Stations_EMPEZ<-zooper::stationsEMPEZ

test_that("There are no NAs in the stations Source or Station name", {
  expect_equal(length(which(is.na(Stations$Source))), 0)
  expect_equal(length(which(is.na(Stations$Station))), 0)
})

test_that("There are 0 NA values in the stationsEMPEZ dataset", {
  expect_equal(length(which(is.na(stationsEMPEZ))), 0)
})
