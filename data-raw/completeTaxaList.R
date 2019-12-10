## code to prepare `completeTaxaList` dataset goes here

completeTaxaList<-crosswalk%>%
  dplyr::select(Phylum, Class, Order, Family, Genus, Species, Taxname)%>%
  dplyr::distinct()%>%
  tidyr::pivot_longer(tidyselect::everything(), names_to = "Level", values_to = "Taxa")%>%
  dplyr::filter(!is.na(Taxa))%>%
  dplyr::pull(Taxa)%>%
  unique()

usethis::use_data(completeTaxaList)
