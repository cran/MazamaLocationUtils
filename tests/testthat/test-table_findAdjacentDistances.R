context("table_findAdjacentDistances")

test_that("bad/missing input generates an error", {
  
  # tbl not a dataframe
  expect_error(table_findAdjacentDistances(locationTbl = c("vector", "instead"), distanceThreshold = 10))
  # distanceThreshold not numeric
  expect_error(table_findAdjacentDistances(locationTbl = wa_airfire_meta, distanceThreshold = "bad radius"))
  
  # tbl missing
  expect_error(table_findAdjacentDistances(NULL, distanceThreshold = 10))
  # radius missing
  expect_error(table_findAdjacentDistances(locationTbl = wa_airfire_meta, distanceThreshold = NULL))
  
})

test_that("table missing latitude or longitude generates an error", {
  
  # No latitude
  testTbl <- wa_airfire_meta %>% dplyr::select(-latitude)
  expect_error(table_findAdjacentDistances(locationTbl = testTbl, distanceThreshold = 10))
  
  # No longitude
  testTbl <- wa_airfire_meta %>% dplyr::select(-longitude)
  expect_error(table_findAdjacentDistances(locationTbl = testTbl, distanceThreshold = 10))
  
})

test_that("passing distanceThreshold = 1000 returns empty tibble", {
  
  result <- table_findAdjacentDistances(wa_airfire_meta, distanceThreshold = 1000)
  
  # Is it the right type?
  expect_equal(class(result), c("tbl_df", "tbl", "data.frame"))
  
  # Is it empty?
  expect_equal(nrow(result), 0)
  
})

test_that("passing distanceThreshold = 4000 returns expected output", {
  
  result <- table_findAdjacentDistances(wa_airfire_meta, distanceThreshold = 4000)
  
  # Is it the right type?
  expect_equal(class(result), c("tbl_df", "tbl", "data.frame"))
  
  # Is it the expected tibble?
  correctTbl <- dplyr::tibble(row1 = c(46, 30, 39, 49), row2 = c(74, 46, 70, 74), distance = c(2412, 3329, 3599, 3628))
  expect_equal(result$row1, correctTbl$row1)
  expect_equal(result$row2, correctTbl$row2)
  expect_equal(round(result$distance, 0), correctTbl$distance)
  
})