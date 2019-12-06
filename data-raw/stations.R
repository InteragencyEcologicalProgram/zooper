## code to prepare `stations` dataset goes here

stations <- readxl::read_excel(file.path("data-raw", "stations.xlsx"), sheet="lat_long")
usethis::use_data(stations, overwrite = TRUE)
