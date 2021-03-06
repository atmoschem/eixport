% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/to_rline.R
\name{to_rline}
\alias{to_rline}
\title{Export emissions to other formats}
\usage{
to_rline(
  Emis,
  Z_b,
  Z_e,
  dCL,
  sigmaz0,
  lanes,
  Hw1,
  dw1,
  Hw2,
  dw2,
  Depth,
  Wtop,
  Wbottom,
  experimental = FALSE,
  crs
)
}
\arguments{
\item{Emis}{Column with the emissions whose unit must be g/ms.}

\item{Z_b}{initial meters above sea level (m).}

\item{Z_e}{final meters above sea level (m).}

\item{dCL}{offset distance for each source relative to the centerline.}

\item{sigmaz0}{vertical dispersion (m).}

\item{lanes}{number of lanes at each street.}

\item{Hw1}{Height of the barrier 1 (m).}

\item{dw1}{Distance to barrier 1 (m).}

\item{Hw2}{height of the barrier 2 (m).}

\item{dw2}{Distance to barrier 2 (m).}

\item{Depth}{Depth of the depression. USed for depressed roadway (m).}

\item{Wtop}{width of the opening at the top of the depression (m).}

\item{Wbottom}{width of the roadway at the bottom of the depression (m).}

\item{experimental}{Boolean argument to denote the use of the experimental
features (TRUE) or not (FALSE).}

\item{crs}{Numeric; Coordenade Reference System to project data or not.}
}
\value{
Data frame with format for R-LINE model.
}
\description{
Export emissions object according to format of file 'Sources.txt'
of the model R-LINE
}
\note{
Michelle G. Snyder, Akula Venkatram, David K. Heist, Steven G. Perry,
William B. Petersen, Vlad Isakov, RLINE: A line source dispersion model
for near-surface releases, In Atmospheric Environment, Volume 77, 2013,
Pages 748-756, ISSN 1352-2310, https://doi.org/10.1016/j.atmosenv.2013.05.074.
}
\examples{
{
data(emisco)
emisco <- st_explode(emisco)
emisco$V8 <- units::set_units(emisco$V8, "g/ms")
Source <- to_rline(Emis = emisco["V8"],
                   Z_b =0,
                   Z_e =0,
                   dCL = 0,
                   sigmaz0 = 2,
                   lanes = 1)
head(Source)
write.table(x = Source,
            file = paste0(tempdir(), "/Sources.txt"),
            row.names = FALSE,
            sep = " ",
            quote = FALSE)
}
}
