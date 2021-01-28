context("table_findOverlappingLocations")

test_that("bad/missing input generates an error", {
  
  # tbl not a dataframe
  expect_error(table_findOverlappingLocations(tbl <- c("vector", "instead"), radius = 10))
  # radius not numeric
  expect_error(table_findOverlappingLocations(tbl <- wa_airfire_meta, radius = "bad radius"))
  
  # tbl missing
  expect_error(table_findOverlappingLocations(tbl <-NULL, radius = 10))
  # radius missing
  expect_error(table_findOverlappingLocations(tbl <- wa_airfire_meta, radius = NULL))
  
})

test_that("table missing latitude or longitude generates an error", {
  
  # No latitude
  testTbl <- wa_airfire_meta %>% dplyr::select(-latitude)
  expect_error(table_findOverlappingLocations(tbl <- testTbl, radius = 10))
  
  # No longitude
  testTbl <- wa_airfire_meta %>% dplyr::select(-longitude)
  expect_error(table_findOverlappingLocations(tbl <- testTbl, radius = 10))
  
})

test_that("passing radius = 1000 returns empty tibble", {
  
  result <- table_findOverlappingLocations(wa_airfire_meta, radius = 1000)
  
  # Is it the right type?
  expect_equal(class(result), c("tbl_df", "tbl", "data.frame"))
  
  # Is it empty?
  expect_equal(nrow(result), 0)
  
})

test_that("passing radius = 2000 returns expected output", {
  
  result <- table_findOverlappingLocations(wa_airfire_meta, radius = 2000)
  
  # Is it the right type?
  expect_equal(class(result), c("tbl_df", "tbl", "data.frame"))
  
  # Is it the expected tibble?
  correctTbl <- dplyr::tibble(row1 = c(2, 12, 2), row2 = c(34, 34, 9), distance = c(2497, 3329, 3762))
  expect_equal(result$row1, correctTbl$row1)
  expect_equal(result$row2, correctTbl$row2)
  expect_equal(round(result$distance, 0), correctTbl$distance)
  
})