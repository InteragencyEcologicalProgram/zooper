library(zooper)

Crosswalk <- tibble::tibble(
  EMP_Micro = c("Tax1", "Tax2", "Tax3", rep(NA, 7)),
  EMP_Meso = c(rep(NA, 2), "Tax3", "Tax4", "Tax5", rep(NA, 5)),
  EMP_Macro = c(rep(NA, 5), "Tax6", "Tax7", "Tax8", NA, NA),
  FMWT_Macro = c(rep(NA, 5), "Tax6", "Tax7", NA, "Tax9", NA),
  FMWT_Meso = c(rep(NA, 3), "Tax4", "Tax5", rep(NA, 4), "Tax10"),
  TNS_Macro = c(rep(NA, 6), "Tax7", NA, "Tax9", NA),
  TNS_Meso = c(rep(NA, 2), "Tax3", "Tax4", "Tax5", rep(NA, 5)),
  Phylum = "Phy1",
  Class = c("Class1", "Class2", "Class3", "Class1", "Class2", "Class3", "Class1", "Class2", "Class3", "Class4"),
  Order = c("Order1", "Order2", "Order3", "Order1", "Order2", "Order3", "Order1", "Order2", "Order3", "Order4"),
  Family = c("Fam1", "Fam2", "Fam3", "Fam1", "Fam2", "Fam3", "Fam4", "Fam5", "Fam6", "Fam7"),
  Genus = paste0("Gen", 1:10),
  Species = paste0("Spec", 1:10),
  Taxname = paste0("Spec", 1:10),
  Lifestage = rep(c("Adult", "Larva"), 5)
)

test1 <- Taxnamefinder(Crosswalk, "Phy1")

test2 <- Taxnamefinder(Crosswalk, c("Fam1", "Fam2"))

test3 <- Taxnamefinder(Crosswalk, c("Class1", "Fam7", "Order3", "Order1"))

test_that("Taxnamefinder produces character vector", {
  expect_type(test1, "character")
  expect_type(test2, "character")
  expect_type(test3, "character")
})


test_that("Taxnamefinder produces correct length character vector", {
  expect_length(test1, length(unique(Crosswalk$Taxname)))
  expect_length(test2, 4)
  expect_length(test3, 7)
})

test_that("Taxnamefinder produces correct output", {
  expect_setequal(test1, paste0("Spec", 1:10))
  expect_setequal(test2, c("Spec1", "Spec2", "Spec4", "Spec5"))
  expect_setequal(test3, c("Spec1", "Spec4", "Spec7", "Spec10", "Spec3", "Spec6", "Spec9"))
})
