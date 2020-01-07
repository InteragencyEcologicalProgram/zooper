library(zooper)

Taxlifestage_list <- list(ID=c("Taxa 1 Adult", "Taxa 2 Adult", "Taxa 3 Adult", "Taxa 1 Larvae", "Taxa 4 Adult", "Taxa 5 Juvenile"))
Remove_taxa <- list(ID=c("Taxa 1", "Taxa 3"))
out <- Wordremover("ID", Taxlifestage_list, Remove_taxa)

test_that("Wordremover outputs length 1 character vector", {
  expect_length(out, 1)
  expect_type(out, "character")
})


test_that("Wordremover correctly removes words", {
  expect_equal(out, "Taxa 2 Adult, Taxa 4 Adult, Taxa 5 Juvenile")
})
