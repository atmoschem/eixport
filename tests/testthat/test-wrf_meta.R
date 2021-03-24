test_that("wrf_meta works", {

  f  <- paste0(system.file("extdata", package = "eixport"),"/wrfinput_d02")
  df <- wrf_meta(f)
  DX <- df$global[7,2]

  expect_equal(DX,"3000")
})
