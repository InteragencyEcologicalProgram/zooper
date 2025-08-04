suppressWarnings({
  require(dplyr)
  require(tidyr)
  require(lubridate)
})

crosswalk_starts<-zooper::crosswalk%>%
  select(contains(c("EMP", "FMWT", "STN", "twentymm", "DOP", "FRP")))%>%
  mutate(EMP=if_else(!is.na(EMP_Micro) | !is.na(EMP_Meso) | !is.na(EMP_Macro), "Y", NA_character_),
         FMWT=if_else(!is.na(FMWT_Meso) | !is.na(FMWT_Macro) | !is.na(STN_Meso) | !is.na(STN_Macro), "Y", NA_character_),
         twentymm=if_else(!is.na(twentymm_Meso), "Y", NA_character_),
         DOP=if_else(!is.na(DOP_Meso) | !is.na(DOP_Macro), "Y", NA_character_),
         FRP=if_else(!is.na(FRP_Meso) | !is.na(FRP_Macro), "Y", NA_character_))

taxlifestages<-unique(paste(zooper::crosswalk$Taxname, zooper::crosswalk$Lifestage))

starts<-zooper::startDates%>%
  group_by(Source)%>%
  summarise(Startdate=year(min(Startdate)), .groups="drop")%>%
  pivot_wider(names_from=Source, values_from=Startdate)

test_that("Crosswalk levels are entered correctly", {
  expect_setequal(unique(zooper::crosswalk$Level), c("Genus", "Species", "Class", "Order", "Family", "Phylum", NA))
})

test_that("Crosswalk start dates are entered for every taxa", {
  expect_true(all(is.finite(filter(crosswalk_starts, !is.na(EMP))$EMPstart)))
  expect_true(all(is.finite(filter(crosswalk_starts, !is.na(FMWT))$FMWTstart)))
  expect_true(all(is.finite(filter(crosswalk_starts, !is.na(twentymm))$twentymmstart)))
  expect_true(all(is.finite(filter(crosswalk_starts, !is.na(DOP))$DOPstart)))
  expect_true(all(is.finite(filter(crosswalk_starts, !is.na(FRP))$FRPstart)))
})

test_that("Crosswalk start dates are reasonable for every taxa", {
  expect_true(all(year(filter(crosswalk_starts, !is.na(EMP))$EMPstart)<=year(Sys.Date())))
  expect_true(all(year(filter(crosswalk_starts, !is.na(FMWT))$FMWTstart)<=year(Sys.Date())))
  expect_true(all(year(filter(crosswalk_starts, !is.na(twentymm))$twentymmstart)<=year(Sys.Date())))
  expect_true(all(year(filter(crosswalk_starts, !is.na(DOP))$DOPstart)<=year(Sys.Date())))
  expect_true(all(year(filter(crosswalk_starts, !is.na(FRP))$FRPstart)<=year(Sys.Date())))
})

test_that("Crosswalk start dates are not earlier than survey start dates", {
  expect_true(all(is.na(crosswalk_starts$twentymmstart) | starts$`20mm` <= year(crosswalk_starts$twentymmstart)))
  expect_true(all(is.na(crosswalk_starts$twentymmstart2) | starts$`20mm` <= year(crosswalk_starts$twentymmstart2)))
  expect_true(all(is.na(crosswalk_starts$DOPstart) | starts$DOP <= year(crosswalk_starts$DOPstart)))
  expect_true(all(is.na(crosswalk_starts$EMPstart) | starts$EMP <= year(crosswalk_starts$EMPstart)))
  expect_true(all(is.na(crosswalk_starts$FMWTstart) | starts$FMWT <= year(crosswalk_starts$FMWTstart)))
  expect_true(all(is.na(crosswalk_starts$FRPstart) | starts$FRP <= year(crosswalk_starts$FRPstart)))
})

test_that("No survey species codes are repeated in the crosswalk", {
  expect_true(all(!duplicated(na.omit(zooper::crosswalk$EMP_Meso))))
  expect_true(all(!duplicated(na.omit(zooper::crosswalk$EMP_Micro))))
  expect_true(all(!duplicated(na.omit(zooper::crosswalk$EMP_Macro))))
  expect_true(all(!duplicated(na.omit(zooper::crosswalk$EMP_Lengths))))
  expect_true(all(!duplicated(na.omit(zooper::crosswalk$STN_Meso))))
  expect_true(all(!duplicated(na.omit(zooper::crosswalk$STN_Macro))))
  expect_true(all(!duplicated(na.omit(zooper::crosswalk$FMWT_Meso))))
  expect_true(all(!duplicated(na.omit(zooper::crosswalk$FMWT_Macro))))
  expect_true(all(!duplicated(na.omit(zooper::crosswalk$twentymm_Meso))))
  expect_true(all(!duplicated(na.omit(zooper::crosswalk$FRP_Meso))))
  expect_true(all(!duplicated(na.omit(zooper::crosswalk$FRP_Macro))))
  expect_true(all(!duplicated(na.omit(zooper::crosswalk$YBFMP))))
  expect_true(all(!duplicated(na.omit(zooper::crosswalk$DOP_Meso))))
  expect_true(all(!duplicated(na.omit(zooper::crosswalk$DOP_Macro))))
})

test_that("All Taxlifestage values in zoopComb appear in crossswalk", {
  expect_true(all(unique(zooper::zoopComb$Taxlifestage)%in%taxlifestages))
})

taxname_check<-zooper::crosswalk%>%
  filter(!is.na(Level))%>%
  rowwise()%>%
  mutate(verify=pick(Phylum, Class, Order, Family, Genus, Species)[[Level]])%>%
  ungroup()

test_that("All Taxnames correspond to the specified taxonomic level", {
  expect_equal(taxname_check$Taxname, taxname_check$verify)
})

taxname_level<-zooper::crosswalk%>%
  distinct(Taxname, Level, Phylum, Class, Order, Family, Genus, Species)%>%
  pull(Taxname)%>%
  duplicated()%>%
  which()

test_that("Taxnames are entered with a consistent taxonomic level", {
  expect_equal(length(taxname_level), 0)
})

