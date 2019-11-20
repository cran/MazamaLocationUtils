context("table_updateSingleRecord")

test_that("table_updateSingleRecord() works", {
  locationTbl <- get(data("wa_monitors_500"))
  
  wenatcheeRecord <- locationTbl %>% dplyr::filter(city == "Wenatchee")
  wenatcheeID <- wenatcheeRecord$locationID
  wenatcheeOldName <- wenatcheeRecord$locationName
  
  testTbl <- table_updateSingleRecord(
    locationTbl,
    locationList = list(
      locationID = wenatcheeID,
      locationName = "Wenatchee-Fifth St"
    )
  )

  wenatcheeNewName <- 
    testTbl %>% 
    dplyr::filter(city == "Wenatchee") %>%
    dplyr::pull(locationName)
  
  expect_equal(wenatcheeOldName, "us.wa_8e5431")
  expect_equal(wenatcheeNewName, "Wenatchee-Fifth St")
})

