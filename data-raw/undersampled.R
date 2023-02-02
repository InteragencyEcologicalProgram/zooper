## code to prepare `undersampled` dataset goes here

undersampled<-readr::read_csv(file.path("data-raw", "undersampled.csv"),
                              col_types=readr::cols_only(SizeClass="c", Taxname="c", Lifestage="c"))
usethis::use_data(undersampled, overwrite = TRUE)
