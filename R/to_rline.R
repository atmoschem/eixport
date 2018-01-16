#' Export emissions to other formats
#'
#' @description Export Emissions object according format of file 'Sources.txt'
#' of the model R-LINE
#'
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
#' f1 <- to_rline(X_b = emisco$xmin,
#'                Y_b = emisco$ymin,
#'                Z_b =0,
#'                X_e = emisco$xmin,
#'                Y_e = emisco$ymin,
#'                Z_e =0,
#'                dCL = 0,
#'                Emis = emisco$V8,
#'                sigmaz0 = 2,
#'                lanes = emisco$lanes)
#' write.table(x = f1, file = "~/Source.txt", row.names = FALSE)
#' }
to_rline <- function (X_b,
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
  if (missing(Emis) | is.null(Emis)) {
    stop (print("No 'Emissions'"))
  } else if (experimental){
  dfa <- data.frame(Group = format(1:length(X_b), width = 5))
  dfb <- format(cbind(X_b = X_b, Y_b = Y_b), width = 10)
  dfc <-   format(data.frame(Z_b = ifelse(length(Z_b) == 1,
                                          rep(Z_b, length(X_b)), Z_b)),
                  width = 5)
  dfd <- format(cbind(X_e = X_e, Y_e = Y_e), width = 10)

  dfe <- format(data.frame(Z_e = ifelse(length(Z_e) == 1,
                                        rep(Z_e, length(X_e)), Z_e)),
                width = 5)
  dff <- format(data.frame(dCL = dCL), width = 5)
  dfg <- format(data.frame(sigmaz0 =  ifelse(length(sigmaz0) == 1,
                                             rep(sigmaz0, length(sigmaz0)),
                                             sigmaz0)),
                width = 9)
  dfh <- format(data.frame(lanes = lanes), wdith = 7)
  dfi <- format(data.frame(Emis = Emis), wdith = 6)
  dfj <- format(data.frame(Hw1 = Hw1,
                           dw1 = dw1,
                           Hw2 = Hw2,
                           dw2 = dw2), wdith = 5)
  dfk <- format(data.frame(Depth = Depth), width = 7)
  dfl <- format(data.frame(Wtop = Wtop), width = 7)
  dfm <- format(data.frame(Wbottom = Wbottom), width = 9)
  df <- cbind(dfa, dfb, dfc, dfd, dfe, dff, dfg, dfh, dfi, dfj, dfk, dfl, dfm)

  return(df)
  } else if(experimental == FALSE){
    dfa <- data.frame(Group = format(1:length(X_b), width = 5))
    dfb <- format(cbind(X_b = X_b, Y_b = Y_b), width = 10)
    dfc <-   format(data.frame(Z_b = ifelse(length(Z_b) == 1,
                                            rep(Z_b, length(X_b)), Z_b)),
                    width = 5)
    dfd <- format(cbind(X_e = X_e, Y_e = Y_e), width = 10)

    dfe <- format(data.frame(Z_e = ifelse(length(Z_e) == 1,
                                          rep(Z_e, length(X_e)), Z_e)),
                  width = 5)
    dff <- format(data.frame(dCL = dCL), width = 5)
    dfg <- format(data.frame(sigmaz0 =  ifelse(length(sigmaz0) == 1,
                                               rep(sigmaz0, length(sigmaz0)),
                                               sigmaz0)),
                  width = 9)
    dfh <- format(data.frame(lanes = lanes), wdith = 7)
    dfi <- format(data.frame(Emis = Emis), wdith = 6)
    dfj <- format(data.frame(Hw1 = rep(0, length(X_e)),
                             dw1 = rep(0, length(X_e)),
                             Hw2 = rep(0, length(X_e)),
                             dw2 = rep(0, length(X_e))),
                  wdith = 5)
    dfk <- format(data.frame(Depth = rep(0, length(X_e))),
                  width = 7)
    dfl <- format(data.frame(Wtop = rep(0, length(X_e))),
                  width = 7)
    dfm <- format(data.frame(Wbottom = rep(0, length(X_e))),
                  width = 9)
    df <- cbind(dfa, dfb, dfc, dfd, dfe, dff, dfg, dfh, dfi, dfj, dfk, dfl, dfm)
    return(df)
  }
}
