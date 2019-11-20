context("table_addColumn")

test_that("table_addColumn() works", {
  locationTbl <- get(data("wa_monitors_500"))
  testTbl <- table_addColumn(locationTbl, "siteName")
  expect_equal(c(names(locationTbl),"siteName"), names(testTbl))
})

