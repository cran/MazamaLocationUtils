context("clusterByDistance")

test_that("clustering works for small tables", {
  
  # ?cluster::pam says: 
  #   maxClusters = positive integer specifying the number of clusters, less than the number of observations.
  
  # Table with only 2 rows
  tbl <- dplyr::tibble(longitude = c(-127.1, -127.3, -127.5), latitude = c(45.1, 45.3, 45.5))
  
  expect_no_error({
    clusteredTbl <- 
      clusterByDistance(
        tbl,
        clusterDiameter = 1000,
        lonVar = "longitude",
        latVar = "latitude",
        maxClusters = 50
      )
  })
  
})
