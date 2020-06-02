## code to prepare `completeTaxaList` dataset goes here

completeTaxaList<-zooper:::Datareducer(crosswalk, c("Phylum", "Class", "Order", "Family", "Genus", "Species", "Taxname"))

usethis::use_data(completeTaxaList, overwrite = TRUE)
