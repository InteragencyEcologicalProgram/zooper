## code to prepare `zoopComb and zoopEnvComb` datasets goes here

zoop<-Zoopdownloader(Data_folder=tempdir(), Save_object=FALSE, Return_object=TRUE)

zoopComb <- zoop$Zooplankton
zoopEnvComb <- zoop$Environment

usethis::use_data(zoopComb, zoopEnvComb, overwrite = TRUE)
