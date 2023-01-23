#Download and fiddle with DOP data

library(tidyverse)
library(readxl)
library(lubridate)

#get all of DOP's accessory environmental data
DOPtrawls = read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.1187.3&entityid=a1abf498fc55b569977d252563f8fcab")

#mesozooplankton
DOPmeso = read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.1187.3&entityid=36a54d0243fada06c94344477c198bc4")

#macrozooplankton
DOPmacro = read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.1187.3&entityid=5dfc521f61f7494521f3d4b8df05256b")

crosswalk = read_excel("data-raw/crosswalk.xlsx", sheet = "Hierarchy2")



#DOP data from wide to long, then merge with trawl data
DOPlong = pivot_longer(DOPmeso, cols = !ICF_ID, names_to = "DOP_Meso", values_to = "CPUE")

#make sure all names are in the crosswalk
DOPtaxa = unique(DOPlong$DOP_Meso)
DOPtaxa %in% crosswalk$DOP_Meso
DOPtaxa[which(!DOPtaxa %in% crosswalk$DOP_Meso)]

#merge with trawl data and crosswalk
DOPlong2 = left_join(DOPtrawls, DOPlong) %>%
 left_join(crosswalk) %>%
  filter(!is.na(CPUE))

#same with the macrozooplankton data
DOPmacrolong = pivot_longer(DOPmacro, cols = !ICF_ID,  names_to = "DOP_Macro", values_to = "CPUE")


#make sure all names are in the crosswalk
DOPtaxam = unique(DOPmacrolong$DOP_Macro)
DOPtaxam %in% crosswalk$DOP_Macro
DOPtaxam[which(!DOPtaxam %in% crosswalk$DOP_Macro)]

testy = left_join(DOPtrawls, DOPmeso)

#Is the DOP data missing anything?

DOP = filter(zoopComb, Source == "DOP")

DOP2 = group_by(DOP, Taxlifestage) %>%
  summarize(N = n(), sum = sum(CPUE, na.rm = T))


length(DOPtrawls$ICF_ID %in% DOPmeso$ICF_ID)
length(DOPmeso$ICF_ID %in% DOPtrawls$ICF_ID)

sub = filter(testy, is.na(Acanthocyclops_spp_adult))

testy2 = left_join(DOPtrawls, DOPmacro)

sub2 = filter(testy2, is.na(Eogammarus_spp), !is.na(Americorophium_spp))

##############################
#make graphs cuase they are fun!

ggplot(DOPlong2, aes(x = Habitat, y = CPUE, fill = Taxname)) + geom_col(position = "fill")

DOPlong2 = mutate(DOPlong2, Year = year(Date), Month = month(Date))

ggplot(DOPlong2, aes(x = Habitat, y = CPUE, fill = Genus)) + geom_col(position = "fill") +
  facet_wrap(~Year)

#Huh. What happened to Limnoithona?
Limno = filter(DOPlong2, Genus == "Limnoithona")
ggplot(Limno, aes(x = Habitat, y = CPUE, fill = Species)) + geom_col()+
  facet_wrap(~Year)

#maybe sample number changed?

Limnomean = group_by(Limno, Month, Year, Habitat, Species) %>%
  summarize(CPUE = mean(CPUE))
ggplot(Limnomean, aes(x = Habitat, y = CPUE, fill = Species)) + geom_col()+
  facet_grid(Month~Year)
