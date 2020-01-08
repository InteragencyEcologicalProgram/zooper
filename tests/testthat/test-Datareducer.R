library(zooper)

words <- tibble::tibble(
  A = c("A", NA, "B", "C", NA),
  B = c("D", "A", NA, "F", "C"),
  C = c(NA, "G", "A", "P", "Q"),
  D = c("S", "T", "U", "V", "A")
)

Awords <- Datareducer(words, c("A"))
ABwords <- Datareducer(words, c("A", "B"))
ABCwords <- Datareducer(words, c("A", "B", "C"))
ABCDwords <- Datareducer(words, c("A", "B", "C", "D"))

test_that("Datareducer produces character vector", {
  expect_type(Awords, "character")
  expect_type(ABwords, "character")
  expect_type(ABCwords, "character")
  expect_type(ABCDwords, "character")
})


test_that("Datareducer produces correct length character vector", {
  expect_length(Awords, 3)
  expect_length(ABwords, 5)
  expect_length(ABCwords, 8)
  expect_length(ABCDwords, 12)
})

test_that("Datareducer correctly reduces data to character vectors", {
  expect_setequal(Awords, c("A", "B", "C"))
  expect_setequal(ABwords, c("A", "B", "C", "D", "F"))
  expect_setequal(ABCwords, c("A", "B", "C", "D", "F", "G", "P", "Q"))
  expect_setequal(ABCDwords, c("A", "B", "C", "D", "F", "G", "P", "Q", "S", "T", "U", "V"))
})
