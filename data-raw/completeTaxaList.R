## code to prepare `completeTaxaList` dataset goes here

completeTaxaList<-zooper:::Datareducer(zooper::crosswalk, c("Phylum", "Class", "Order", "Family", "Genus", "Species", "Taxname"))

usethis::use_data(completeTaxaList, overwrite = TRUE)
