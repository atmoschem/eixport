#' Export emissions to other formats
#'
#' @description Export emissions object according to format of file 'Sources.txt'
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
#' @param experimental Boolean argument to denote the use of the experimental
#' features (TRUE) or not (FALSE).
#' @return Data frame with format for R-LINE model.
#' @note Michelle G. Snyder, Akula Venkatram, David K. Heist, Steven G. Perry,
#' William B. Petersen, Vlad Isakov, RLINE: A line source dispersion model
#' for near-surface releases, In Atmospheric Environment, Volume 77, 2013,
#' Pages 748-756, ISSN 1352-2310, https://doi.org/10.1016/j.atmosenv.2013.05.074.
#' @export
#' @examples \dontrun{
#' # Do not run
#' data(emisco)
#' Source <- to_rline(X_b = emisco$xmin,
#'                Y_b = emisco$ymin,
#'                Z_b =0,
#'                X_e = emisco$xmin,
#'                Y_e = emisco$ymin,
#'                Z_e =0,
#'                dCL = 0,
#'                Emis = emisco$V8,
#'                sigmaz0 = 2,
#'                lanes = emisco$lanes)
#' head(Source)
#' write.table(x = Source, file = paste0(tempdir(), "/Sources.txt"),
#' row.names = FALSE, sep = " ", quote = FALSE)
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
    stop (print("No 'Emissions'")) # nocov
  } else if (experimental){
    df <- data.frame(
      Group = as.character(format(1:length(X_b), width = 5)))
    df$X_b  <- as.character(format(X_b, width = 10))
    df$Y_b  <- as.character(format(Y_b, width = 10))
    df$Z_b <- as.character(format(ifelse(length(Z_b) == 1,
                                         rep(Z_b, length(X_b)),
                                         Z_b),
                                  width = 5))
    df$X_e  <- as.character(format(X_e, width = 10))
    df$Y_e  <- as.character(format(Y_e, width = 10))
    df$Z_e <- as.character(format(ifelse(length(Z_e) == 1,
                                         rep(Z_e, length(X_e)),
                                         Z_e),
                                  width = 5))
    df$dCL <- as.character(format(dCL, width = 5))
    df$sigma0 <- as.character(format(ifelse(length(sigmaz0) == 1,
                                            rep(sigmaz0, length(X_e)),
                                            sigmaz0),
                                     width = 9))
    df$lanes <- as.character(format(lanes, width = 7))
    df$Emis <- as.character(format(Emis, width = 6))
    df$Hw1 <- as.character(format(Hw1, width = 5))
    df$dw1 <- as.character(format(dw1, width = 5))
    df$Hw2 <- as.character(format(Hw2, width = 5))
    df$dw2 <- as.character(format(dw2, width = 5))
    df$Depth <- as.character(format(Depth, width = 7))
    df$Wtop <- as.character(format(Wtop, width = 7))
    df$Wbottom <- as.character(format(Wbottom, width = 9))
    return(df)
  } else if(experimental == FALSE){
    df <- data.frame(
      Group = as.character(format(1:length(X_b), width = 5)))
    df$X_b  <- as.character(format(X_b, width = 10))
    df$Y_b  <- as.character(format(Y_b, width = 10))
    df$Z_b <- as.character(format(ifelse(length(Z_b) == 1,
                                         rep(Z_b, length(X_b)),
                                         Z_b),
                                  width = 5))
    df$X_e  <- as.character(format(X_e, width = 10))
    df$Y_e  <- as.character(format(Y_e, width = 10))
    df$Z_e <- as.character(format(ifelse(length(Z_e) == 1,
                                         rep(Z_e, length(X_e)),
                                         Z_e),
                                  width = 5))
    df$dCL <- as.character(format(dCL, width = 5))
    df$sigma0 <- as.character(format(ifelse(length(sigmaz0) == 1,
                                            rep(sigmaz0, length(X_e)),
                                            sigmaz0),
                                     width = 9))
    df$lanes <- as.character(format(lanes, width = 7))
    df$Emis <- as.character(format(Emis, width = 6))
    df$Hw1 <- as.character(format(0, width = 5))
    df$dw1 <- as.character(format(0, width = 5))
    df$Hw2 <- as.character(format(0, width = 5))
    df$dw2 <- as.character(format(0, width = 5))
    df$Depth <- as.character(format(0, width = 7))
    df$Wtop <- as.character(format(0, width = 7))
    df$Wbottom <- as.character(format(0, width = 9))
    return(df)
  }
}
