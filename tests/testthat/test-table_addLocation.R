context("table_addLocation")

test_that("table_addLocation() works", {
  skip_on_cran()
  skip_on_travis()
  
  mazama_initialize()
  locationTbl <- get(data("wa_monitors_500"))
  # Add a new record
  testTbl <- table_addLocation(locationTbl, -111.111, 44.444, 500, verbose = FALSE)
  expect_equal(nrow(locationTbl) + 1, nrow(testTbl))
  
  # Don't add it a second time
  testTbl <- table_addLocation(testTbl, -111.111, 44.444, 500, verbose = FALSE)
  expect_equal(nrow(locationTbl) + 1, nrow(testTbl))
})
