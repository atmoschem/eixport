context("wrf_put")

file <- "/home/sergio/R/x86_64-pc-linux-gnu-library/4.0/eixport/extdata/wrfinput_d02"
wrf_summary(file = file,
            name = c("XLAT", "XLONG"),
            fn = "mean")
