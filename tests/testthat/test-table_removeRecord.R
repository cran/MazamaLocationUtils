context("table_removeRecord")

test_that("table_removeRecord() works", {
  
  locationTbl <- get(data("wa_monitors_500"))
  
  # First three from wa_monitors_500
  locationID <- c("ddbb565d51fe74ba", "f4ac27b3de8b9c19", "efce6225e8b2b1b8")
  
  testTbl <- table_removeRecord(locationTbl, locationID, verbose = FALSE)
  expect_equal(nrow(locationTbl) - 3, nrow(testTbl))
  
})
