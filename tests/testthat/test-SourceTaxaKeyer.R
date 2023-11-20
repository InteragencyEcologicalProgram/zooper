suppressWarnings({
  library(zooper)
  library(dplyr)
})

Data <-tibble::tibble(
  Source = c("EMP", "EMP", "EMP", "FMWT", "FMWT", "STN"),
  SizeClass = c("Micro", "Meso", "Macro", "Macro", "Meso", "Macro"))

Crosswalk <- tibble::tibble(
  EMP_Micro = c("Tax1", "Tax2", "Tax3", rep(NA, 7)),
  EMP_Meso = c(rep(NA, 2), "Tax3", "Tax4", "Tax5", rep(NA, 5)),
  EMP_Macro = c(rep(NA, 5), "Tax6", "Tax7", "Tax8", NA, NA),
  FMWT_Macro = c(rep(NA, 5), "Tax6", "Tax7", NA, "Tax9", NA),
  FMWT_Meso = c(rep(NA, 3), "Tax4", "Tax5", rep(NA, 4), "Tax10"),
  STN_Macro = c(rep(NA, 6), "Tax7", NA, "Tax9", NA),
  STN_Meso = c(rep(NA, 2), "Tax3", "Tax4", "Tax5", rep(NA, 5)),
  Phylum = "Phy1",
  Class = c("Class1", "Class2", "Class3", "Class1", "Class2", "Class3", "Class1", "Class2", "Class3", "Class4"),
  Order = c("Order1", "Order2", "Order3", "Order1", "Order2", "Order3", "Order1", "Order2", "Order3", "Order4"),
  Family = c("Fam1", "Fam2", "Fam3", "Fam1", "Fam2", "Fam3", "Fam4", "Fam5", "Fam6", "Fam7"),
  Genus = paste0("Gen", 1:10),
  Taxname = paste0("Spec", 1:10),
  Lifestage = rep(c("Adult", "Larva"), 5)
)

SourceTaxaKey <- SourceTaxaKeyer(Data = Data, Crosswalk = Crosswalk)

test_that("SourceTaxaKeyer returns correct columns", {
  expect_equal(names(SourceTaxaKey), c("Phylum", "Class", "Order", "Family", "Genus", "Taxname", "Lifestage", "Source", "SizeClass"))
})

test_that("SourceTaxaKeyer returns correct Source x SizeClass combinations", {
  expect_equal(unique(paste(SourceTaxaKey$Source, SourceTaxaKey$SizeClass, sep="_")), unique(paste(Data$Source, Data$SizeClass, sep="_")))
})

test_that("SourceTaxaKeyer returns correct number of rows for each Source x SizeClass combination", {
  expect_equal(SourceTaxaKey%>%group_by(Source, SizeClass)%>%summarise(N=n())%>%pull(N), c(rep(3, nrow(Data)-1), 2))
})

test_that("SourceTaxaKeyer returns correct number Taxnames", {
  expect_equal(SourceTaxaKey%>%select(Taxname, Source, SizeClass),
               tibble::tibble(Taxname=paste0("Spec", c(1,2,3,3,4,5,6,7,8,6,7,9,4,5,10,7,9)),
                              Source=c(rep("EMP", 9), rep("FMWT", 6), rep("STN",2)),
                              SizeClass=c(rep("Micro", 3), rep("Meso", 3), rep("Macro", 3),
                                          rep("Macro", 3), rep("Meso", 3), rep("Macro", 2))))
})
