context("get_edgar")

get_edgar(dataset = "v50_AP",
          destpath = tempdir(),
          sector = c("CHE"),
          pol = "CO",
          year = 2012,
          ask = F)

get_edgar(dataset = "v50_AP",
          destpath = tempdir(),
          sector = c("CHE"),
          pol = "CO",
          year = 2012:2013,
          ask = F,
          n = 2)


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

