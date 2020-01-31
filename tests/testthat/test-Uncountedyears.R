library(zooper)

datasets<-zoopComb%>%
  dplyr::select(.data$Source, .data$SizeClass)%>%
  dplyr::filter(.data$Source%in%c("EMP", "FMWT", "twentymm"))%>%
  dplyr::distinct()

BadYears<-purrr::map2_dfr(datasets$Source, datasets$SizeClass, ~ Uncountedyears(Source = .x,
                                                                                Size_class = .y,
                                                                                Crosswalk=crosswalk,
                                                                                Start_year = startDates%>%
                                                                                  dplyr::filter(.data$Source==.x & .data$SizeClass==.y)%>%
                                                                                  dplyr::pull(.data$Startdate)%>%
                                                                                  lubridate::year(),
                                                                                Intro_lag = 2))

test_that("Uncountedyears produces correctly formatted output", {
  expect_gt(nrow(BadYears), 0)
  expect_type(BadYears$Years[[1]], "integer")
})
