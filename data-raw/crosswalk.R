## code to prepare `crosswalk` dataset goes here

crosswalk<-readxl::read_excel(file.path("data-raw", "crosswalk.xlsx"), sheet = "Hierarchy2")
usethis::use_data(crosswalk, overwrite = TRUE)
