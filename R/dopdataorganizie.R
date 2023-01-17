#Download and fiddle with DOP data

library(tidyverse)
library(readxl)

#get all of DOP's accessory environmental data
DOPtrawls = read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.1187.1&entityid=723823274d15e71e110276956d930c40")

#mesozooplankton
DOPmeso = read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.1187.1&entityid=563e0c6a6888737205134941e89f0cda")

#macrozooplankton
DOPmacro = read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.1187.1&entityid=037165b40f4f08545ddf6e374434bac7")

crosswalk = read_excel("crosswalk_test.xlsx", sheet = "Hierarchy2")



#DOP data from wide to long, then merge with trawl data
DOPlong = pivot_longer(DOPmeso, cols = !ICF_ID, names_to = "DOP_Meso", values_to = "CPUE")

#make sure all names are in the crosswalk
DOPtaxa = unique(DOPlong$DOP_Meso)
DOPtaxa %in% crosswalk$DOP_Meso
DOPtaxa[which(!DOPtaxa %in% crosswalk$DOP_Meso)]

#merge with trawl data and crosswalk
DOPlong2 = left_join(DOPtrawls, DOPlong) %>%
 left_join(crosswalk)

#same with the macrozooplankton data
DOPmacrolong = pivot_longer(DOPmacro, cols = !ICF_ID,  names_to = "DOP_Macro", values_to = "CPUE")


#make sure all names are in the crosswalk
DOPtaxam = unique(DOPmacrolong$DOP_Macro)
DOPtaxam %in% crosswalk$DOP_Macro
DOPtaxam[which(!DOPtaxam %in% crosswalk$DOP_Macro)]


#Is the DOP data missing anything?

DOP = filter(zoopComb, Source == "DOP")

DOP2 = group_by(DOP, Taxlifestage) %>%
  summarize(N = n(), sum = sum(CPUE, na.rm = T))
