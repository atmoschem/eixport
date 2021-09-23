context("get_edgar")

 test_that("get_edgar stops", {
   expect_error( get_edgar(dataset = "v50_AP",
                           destpath = tempdir(),
                           sector = c("TRO"),
                           pol = "CO",
                           year = 2012, ask = F) ,
                ".?")
   expect_error( get_edgar(dataset = "v50_AP",
                           destpath = tempdir(),
                           sector = c("CHE"),
                           pol = "CO3",
                           year = 2012, ask = F) ,
                 ".?")

   expect_error( get_edgar(dataset = "v50_AP",
                           destpath = tempdir(),
                           sector = c("CHE"),
                           pol = "CO",
                           year = 20150, ask = T) ,
                 ".?")
 })

