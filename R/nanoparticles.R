#' Diameter of ZnO nanoparticles from optical band gap
#'
#' This formula is simply the inverse of \code{\link{bandgap}}.
#' Which can be useful to calculate diameter from optically determined band gap.
#'
#' @param bandgap numeric, in eV (vector)
#'    Band gap may not be smaller than the bulk band gap of the material (3.30 eV for ZnO)
#' @param sc semiconductor, string (function only supports ZnO)
#'
#' @return diameter, in nanometres (vector)
#' @export
diameter <- function(bandgap, sc = "ZnO") {
   c <- 3.30
   b <- 0.293
   a <- 3.940

   # check the inputs
   if (sc != "ZnO") stop("Error: At this time, this function only supports ZnO")
   if (any(bandgap < c)) stop("Error: bandgap must be greater or equal to the bulk band gap of ZnO (3.30 eV)")

   # calculate diameter
   diameter <- (2 * a) / (-b + sqrt(b^2 - 4 * a * (c - bandgap)))
   return(diameter)
}



#' Band gap of ZnO nanoparticles from particle diameter
#'
#' This formula is based on the empirical relation (in turn derived from X-ray
#' diffraction data, i.e., Scherrer relationship) for ZnO thin-films,
#' previously published in:
#' Jacobsson & Edvinsson (2011). Inorganic Chemistry, 50(19), 9578-9586.
#' https://doi.org/10.1021/ic201327n
#'
#' @param diameter numeric, in nanometre (vector)
#' @param sc semiconductor, string (function only supports ZnO)
#'
#' @return bandgap, in eV (vector)
#' @export
bandgap <- function(diameter, sc = "ZnO") {
   c <- 3.30
   b <- 0.293
   a <- 3.940

   # check the inputs
   if (sc != "ZnO") stop("Error: At this time, this function only supports ZnO")

   # calculate bandgap
   bandgap <- c + (b / diameter) + (a / diameter^2)
   return(bandgap)
}
