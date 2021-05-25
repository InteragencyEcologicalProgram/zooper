Stations<-zooper::stations
Stations_EMPEZ<-zooper::stationsEMPEZ

test_that("There are no NAs in the stations Source or Statio name", {
  expect_equal(length(which(is.na(Stations$Source))), 0)
  expect_equal(length(which(is.na(Stations$Station))), 0)
})

test_that("There are no NAs anywhere in the stationsEMPEZ dataset", {
  expect_equal(length(which(is.na(stationsEMPEZ))), 0)
})
