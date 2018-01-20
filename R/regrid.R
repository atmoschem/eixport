#' Read EDGAR emissions for exporting to other formats
#'
#' @description Return a raster
#'
#' @param edgar Edgar NetCDF.
#' @param gtarget sf grid, which can be produced using wrf_grid.
#' @param ncol Number of columns of gtarget.
#' @param nrow Number of rows of gtarget.
#'
#' @importFrom  raster rasterize raster resample
#' @export
#' @examples \dontrun{
#' # Do not run
#' # IN DEVELOPMENT
#'}
regrid <- function(edgar, gtarget, ncol, nrow){
  gsf1sp <- as(gtarget, "Spatial")
  gsf1r <- rasterize(gsf1sp,
                     raster(gsf1sp,
                            ncol=ncol,
                            nrow=nrow))
  rco_01_gsf1r <- resample(rco_01, gsf1r)
  return(rco_01_gsf1r)
}
