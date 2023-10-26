library(zooper)
require(magrittr)

# Should have 10 - Intro_lag years for species C
Crosswalk1 <- tibble::tibble(
  Taxname = c("A", "B", "C", "D", "E", "F", "B"),
  Lifestage = c("Adult", "Larva", "Juvenile", "Adult", "Adult", "Adult", "Larva"),
  Survey_Meso = c("G", "H", "I", "J", "K", "L", "M"),
  Intro= readr::parse_date(c("1990", NA_character_, "1990", NA_character_, NA_character_, NA_character_, NA_character_), format="%Y"),
  Surveystart = readr::parse_date(c("1990", "1985", "2000", NA_character_, "1970", NA_character_, NA_character_), format="%Y"),
  Surveyend = readr::parse_date(c(NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, "2005"), format="%Y")
)%>%
  dplyr::mutate_at(dplyr::vars(c("Surveystart", "Surveyend")), ~tidyr::replace_na(., lubridate::ymd("2500-01-01")))%>% #Change any NAs for starts or ends to 2500 (i.e. never started or ended)%>%
  dplyr::mutate(Intro=tidyr::replace_na(.data$Intro, lubridate::ymd("1800-01-01"))) #Change any NAs in Intro date to 1800 (i.e., always been around)

# Should have a NULL result
Crosswalk2 <- tibble::tibble(
  Taxname = c("A", "B", "C", "D", "E", "F", "B"),
  Lifestage = c("Adult", "Larva", "Juvenile", "Adult", "Adult", "Adult", "Larva"),
  Survey_Meso = c("G", "H", "I", "J", "K", "L", "M"),
  Intro= readr::parse_date(c("1990", NA_character_, "2000", NA_character_, NA_character_, NA_character_, NA_character_), format="%Y"),
  Surveystart = readr::parse_date(c("1990", "1985", "2000", NA_character_, "1970", NA_character_, NA_character_), format="%Y"),
  Surveyend = readr::parse_date(c(NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, "2005"), format="%Y")
)%>%
  dplyr::mutate_at(dplyr::vars(c("Surveystart", "Surveyend")), ~tidyr::replace_na(., lubridate::ymd("2500-01-01")))%>% #Change any NAs for starts or ends to 2500 (i.e. never started or ended)%>%
  dplyr::mutate(Intro=tidyr::replace_na(.data$Intro, lubridate::ymd("1800-01-01"))) #Change any NAs in Intro date to 1800 (i.e., always been around)

# Should have 5 years for species E from 2010-2015
Crosswalk3 <- tibble::tibble(
  Taxname = c("A", "B", "C", "D", "E", "F", "B", "Z"),
  Lifestage = c("Adult", "Larva", "Juvenile", "Adult", "Adult", "Adult", "Larva", "Adult"),
  Survey_Meso = c("G", "H", "I", "J", "K", "L", "M", "N"),
  Intro= readr::parse_date(c("1990", NA_character_, "1998", NA_character_, NA_character_, NA_character_, NA_character_, "1990"), format="%Y"),
  Surveystart = readr::parse_date(c("1990", "1985", "2000", NA_character_, "1970", NA_character_, NA_character_, "2000"), format="%Y"),
  Surveyend = readr::parse_date(c(NA_character_, NA_character_, NA_character_, NA_character_, "2010", NA_character_, "2005", NA_character_), format="%Y"),
  Surveystart2 = readr::parse_date(c(NA_character_, NA_character_, NA_character_, NA_character_, "2015", NA_character_, NA_character_, NA_character_), format="%Y")
)%>%
  dplyr::mutate_at(dplyr::vars(c("Surveystart", "Surveyend", "Surveystart2")), ~tidyr::replace_na(., lubridate::ymd("2500-01-01")))%>% #Change any NAs for starts or ends to 2500 (i.e. never started or ended)%>%
  dplyr::mutate(Intro=tidyr::replace_na(.data$Intro, lubridate::ymd("1800-01-01"))) #Change any NAs in Intro date to 1800 (i.e., always been around)

Start_year <- lubridate::year(readr::parse_date("1970", format="%Y"))

BadYears1.1<-Uncountedyears("Survey", "Meso", Crosswalk1, Start_year, 1)
BadYears1.2<-Uncountedyears("Survey", "Meso", Crosswalk1, Start_year, 2)
BadYears1.3<-Uncountedyears("Survey", "Meso", Crosswalk1, Start_year, 3)
BadYears2<-Uncountedyears("Survey", "Meso", Crosswalk2, Start_year, 2)
BadYears3<-Uncountedyears("Survey", "Meso", Crosswalk3, Start_year, 2)

test_that("Uncountedyears produces correct number of rows", {
  expect_equal(nrow(BadYears1.1), 4)
  expect_equal(nrow(BadYears1.2), 4)
  expect_equal(nrow(BadYears1.3), 4)
  expect_equal(nrow(BadYears2), 3)
  expect_equal(nrow(BadYears3), 5)
})

test_that("Uncountedyears produces correctly formatted output", {
  expect_type(BadYears1.1$Years[[1]], "integer")
  expect_type(BadYears1.2$Years[[1]], "integer")
  expect_type(BadYears1.3$Years[[1]], "integer")
  expect_type(BadYears3$Years[[1]], "integer")
})

test_that("Uncountedyears produces correct years", {
  expect_setequal(BadYears1.1$Years[[1]], as.integer(1970:1984))
  expect_setequal(BadYears1.2$Years[[1]], as.integer(1970:1984))
  expect_setequal(BadYears1.3$Years[[1]], as.integer(1970:1984))
  expect_setequal(BadYears3$Years[[1]], as.integer(c(1970:1984)))
  expect_setequal(BadYears3$Years[[3]], as.integer(c(2010:2014)))
})
