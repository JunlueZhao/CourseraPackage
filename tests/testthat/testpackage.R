test_that("fars_read does NOT work", {
  filename <- system.file("/extdata/accident_2015.csv.bz2",
                          package="CourseraPackage")
  actual <- fars_read(filename)
  expect_equal(as.numeric(unlist(actual[5, "ST_CASE"])), 10005)
  expect_equal(as.numeric(unlist(actual[1, "COUNTY"])), 127)
  expect_equal(as.numeric(unlist(actual[7, "CITY"])), 1730)
  expect_equal(as.numeric(unlist(actual[10, "DAY"])), 5)
  expect_equal(as.numeric(unlist(actual[5, "PERMVIT"])), 2)
  expect_equal(as.numeric(unlist(actual[9, "VE_FORMS"])), 1)
})


test_that("make_filename does NOT work", {
  year <- 2018
  actual <- make_filename(year)
  expect_equal(actual, "accident_2018.csv.bz2")
})
