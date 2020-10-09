context("wrf_put")

file = paste0(system.file("extdata", package = "eixport"),"/wrfinput_d02")
wrf_summary(file = file,
            name = c("XLAT", "XLONG"),
            fn = "mean")
