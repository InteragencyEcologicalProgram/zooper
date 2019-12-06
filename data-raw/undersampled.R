## code to prepare `undersampled` dataset goes here

undersampled<-readxl::read_excel(file.path("data-raw", "undersampled.xlsx"))
usethis::use_data(undersampled, overwrite = TRUE)
