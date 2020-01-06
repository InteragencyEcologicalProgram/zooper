context("Commontaxer")
library(zooper)

SourceTaxaKey <- tibble::tibble(
  Source = rep(c("EMP", "FMWT", "FRP"), each=6),
  Lifestage = c("Adult", "Larvae", "Juvenile", "Adult", "Adult", "Egg",
                "Adult", "Larvae", "Juvenile", "Larvae", "Juvenile", "Adult",
                "Adult", "Larvae", "Juvenile", "Juvenile", "Juvenile", "Adult"),
  SizeClass = "Micro",
  Taxname = c("Acartiella sinensis", "Acartiella sinensis", "Sinocalanus doerrii", "Acanthocyclops vernalis", "Pseudodiaptomus marinus", "Pseudodiaptomus forbesi",
              "Acartiella sinensis", "Acartiella sinensis", "Pseudodiaptomus forbesi", "Acanthocyclops vernalis", "Eurytemora affinis", "Pseudodiaptomus marinus",
              "Acartiella sinensis", "Acartiella sinensis", "Pseudodiaptomus forbesi", "Acanthocyclops vernalis", "Pseudodiaptomus marinus", "Pseudodiaptomus marinus")
)
Commontax <- Commontaxer(SourceTaxaKey, "Taxname", Size_class = rlang::set_names(c("Micro")))

test_that("Commontaxer returns correct columns", {
  expect_equal(names(Commontax), c("Taxname", "Lifestage"))
})

test_that("Commontaxer returns correct output", {
  expect_equal(Commontax, tibble::tibble(Taxname=c("Acartiella sinensis", "Acartiella sinensis", "Pseudodiaptomus marinus"), Lifestage = c("Adult", "Larvae", "Adult")))
})
