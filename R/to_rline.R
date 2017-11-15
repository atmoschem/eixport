#' Export emissions to other formats
#'
#' @description Export Emissions object to format of file 'Sources.txt' of the
#' model R-LINE
#'
#' @param x Emissions object as spatial feature sf.
#' @param X_b initial x coordinates.
#' @param Y_b initial y coordinates.
#' @param Z_b initial meters above sea level (m).
#' @param X_e final x coordinates.
#' @param Y_e final y coordinates.
#' @param Z_e final meters above sea level (m).
#' @param dCL offset distance for each source relative to the centerline.
#' @param sigmaz0 vertical dispersion (m).
#' @param lanes number of lanes at each street.
#' @param Emis Column with the emissions whose unit must be g/ms.
#' @param Hw1 Height of the barrier 1 (m).
#' @param dw1 Distance to barrier 1 (m).
#' @param Hw2 height of the barrier 2 (m).
#' @param dw2 Distance to barrier 2 (m).
#' @param Depth Depth of the depression. USed for depressed roadway (m).
#' @param Wtop width of the opening at the top of the depression (m).
#' @param Wbottom width of the roadway at the bottom of the depression (m).
#' @param experimental Boolean argument to denotes the use of the experimental
#' features (TRUE) or not (FALSE).
#' @return Data-frame file with format for R-LINE model.
#' @note Michelle G. Snyder, Akula Venkatram, David K. Heist, Steven G. Perry,
#' William B. Petersen, Vlad Isakov, RLINE: A line source dispersion model
#' for near-surface releases, In Atmospheric Environment, Volume 77, 2013,
#' Pages 748-756, ISSN 1352-2310, https://doi.org/10.1016/j.atmosenv.2013.05.074.
#' @export
#' @examples \dontrun{
#' # Do not run
#' data(emisco)
#' f1 <- to_rline(x = emisco,
#'                X_b = emisco$xmin,
#'                Y_b = emisco$ymin,
#'                Z_b =0,
#'                X_e = emisco$xmin,
#'                Y_e = emisco$ymin,
#'                Z_e =0,
#'                dCL = 0,
#'                Emis = emisco$V1,
#'                sigmaz0 = 2,
#'                lanes = emisco$lanes)
#' write.table(x = f1, file = "~/Source.txt", row.names = FALSE)
#' }
to_rline <- function (x,
                      X_b,
                      Y_b,
                      Z_b,
                      X_e,
                      Y_e,
                      Z_e,
                      dCL,
                      sigmaz0,
                      lanes,
                      Emis,
                      Hw1,
                      dw1,
                      Hw2,
                      dw2,
                      Depth,
                      Wtop,
                      Wbottom,
                      experimental = FALSE){
  if (missing(x) | is.null(x)) {
    stop (print("No 'Emissions' object"))
  } else if (experimental){
  df <- data.frame(
    Group = 1:length(X_b),
    X_b = X_b,
    Y_b = Y_b,
    Z_b = ifelse(length(Z_b) == 1, rep(Z_b, length(X_b)), Z_b),
    X_e = X_e,
    Y_e = Y_e,
    Z_e = ifelse(length(Z_e) == 1, rep(Z_e, length(X_e)), Z_e),
    dCL = dCL,
    sigmaz0 =  ifelse(length(sigmaz0) == 1, rep(sigmaz0, length(sigmaz0)), sigmaz0),
    lanes = lanes,
    Emis = Emis,
    Hw1 = Hw1,
    dw1 = dw1,
    Hw2 = Hw2,
    dw2 = dw2,
    Depth = Depth,
    Wtop = Wtop,
    Wbottom = Wbottom)

  return(df)
  } else if(experimental == FALSE){
    df <- data.frame(
      Group = 1:length(X_b),
      X_b = X_b,
      Y_b = Y_b,
      Z_b = ifelse(length(Z_b) == 1, rep(Z_b, length(X_b)), Z_b),
      X_e = X_e,
      Y_e = Y_e,
      Z_e = ifelse(length(Z_e) == 1, rep(Z_e, length(X_e)), Z_e),
      dCL = dCL,
      sigmaz0 =  ifelse(length(sigmaz0) == 1, rep(sigmaz0, length(sigmaz0)), sigmaz0),
      lanes = lanes,
      Emis = Emis,
      Hw1 = rep(0, length(X_e)),
      dw1 = rep(0, length(X_e)),
      Hw2 = rep(0, length(X_e)),
      dw2 = rep(0, length(X_e)),
      Depth = rep(0, length(X_e)),
      Wtop = rep(0, length(X_e)),
      Wbottom = rep(0, length(X_e)))
    return(df)
  }
}
