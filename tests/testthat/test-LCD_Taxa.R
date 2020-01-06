context("LCD_Taxa")
library(zooper)
df <- tibble::tibble(
  Order = rep(c("Calanoida", "Cyclopoida", "Cladocera"), each=10),
  Taxname = c(rep(c("Tortanus discaudatus", "Acartiella sinensis", "Pseudodiaptomus euryhalinus", "Leptodiaptomus siciloides", "Diaptomus dorsalis"), each=2),
              rep("Limnoithona sinensis", 10),
              rep(c("Bosmina longirostris", "Pseudochydorus globosus"), each=5)),
  CPUE = c(rep(2, 10),
           rep(4, 10),
           rep(5, 10)),
  Phylum = "Arthropoda",
  ID = rep(c("A", "B"), 15)
)

Order_sums <- LCD_Taxa(df = df, Taxalevel = "Order", Groupers = c("Phylum", "Order"))

test_that("LCD_taxa removes groups with only 1 unique Taxname", {
  expect_false("Cyclopoida"%in%Order_sums$Order)
})

test_that("LCD_taxa removes columns specified in Groupers, but retains the Taxalevel", {
  expect_false("Phylum"%in%names(Order_sums))
  expect_true("Order"%in%names(Order_sums))
})

test_that("LCD_taxa produces correct number of rows", {
  expect_equal(nrow(Order_sums), 4)
})

test_that("LCD_taxa produces correct total CPUE", {
  expect_equal(sum(Order_sums$CPUE), 70)
})

test_that("LCD_taxa returns correct output", {
  expect_equal(Order_sums, tibble::tibble(Order=rep(c("Calanoida", "Cladocera"), each=2), CPUE = rep(c(10,25), each=2), Taxname = Order, ID = rep(c("A", "B"), 2)))
})
