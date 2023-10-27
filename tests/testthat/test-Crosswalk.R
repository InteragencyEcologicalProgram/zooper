require(dplyr)

crosswalk_starts<-zooper::crosswalk%>%
  select(contains(c("EMP", "FMWT", "STN", "twentymm", "DOP", "FRP")))%>%
  mutate(EMP=if_else(!is.na(EMP_Micro) | !is.na(EMP_Meso) | !is.na(EMP_Macro), "Y", NA_character_),
         FMWT=if_else(!is.na(FMWT_Meso) | !is.na(FMWT_Macro) | !is.na(STN_Meso) | !is.na(STN_Macro), "Y", NA_character_),
         twentymm=if_else(!is.na(twentymm_Meso), "Y", NA_character_),
         DOP=if_else(!is.na(DOP_Meso) | !is.na(DOP_Macro), "Y", NA_character_),
         FRP=if_else(!is.na(FRP_Meso) | !is.na(FRP_Macro), "Y", NA_character_))

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
