context("table_removeRecord")

test_that("table_removeRecord() works", {
  
  locationTbl <- get(data("wa_monitors_500"))
  
  # First three from wa_monitors_500
  locationID <- c("c2913q48uk", "c28f8z9xq8", "c23hfxrdne")
  
  testTbl <- table_removeRecord(locationTbl, locationID, verbose = FALSE)
  expect_equal(nrow(locationTbl) - 3, nrow(testTbl))
  
})
