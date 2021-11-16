context("table_initializeExisting")

test_that("existing locationID is ignored", {

  skip_on_cran()
  skip_on_travis()

  mazama_initialize()
  data("wa_airfire_meta")

  meta <-
    dplyr::as_tibble(wa_airfire_meta[1:3,]) %>%
    dplyr::mutate(
      locationID = location_createID(.data$longitude, .data$latitude)
    )

  locationTbl <- table_initializeExisting(meta, distanceThreshold = 500)
  
  expect_equal(nrow(locationTbl), 3)

})

