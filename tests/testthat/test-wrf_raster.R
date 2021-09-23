test_that("wrf_raster works", {
  f <- paste(system.file("extdata", package = "eixport"),"/wrfinput_d02", sep="")
  r <- wrf_raster(f,'XLAT')
  expect_equal(dim(r)[1], 51)
})
