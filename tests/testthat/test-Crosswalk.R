
test_that("Crosswalk levels are entered correctly", {
  expect_setequal(unique(zooper::crosswalk$Level), c("Genus", "Species", "Class", "Order", "Family", "Phylum", NA))
})
