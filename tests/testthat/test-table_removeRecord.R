context("table_removeRecord")

test_that("table_removeRecord() works", {
  locationTbl <- get(data("wa_monitors_500"))
  
  # First three from wa_monitors_500
  locationID <- c("8b12e57dee8fc50c", "48ff86b963def74a", "aa4dc983f85e1698")
  
  testTbl <- table_removeRecord(locationTbl, locationID, verbose = FALSE)
  expect_equal(nrow(locationTbl) - 3, nrow(testTbl))
})
