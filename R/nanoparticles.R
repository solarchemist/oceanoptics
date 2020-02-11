#' Diameter of ZnO nanoparticles from optical band gap
#'
#' This formula is simply the inverse of \code{\link{bandgap}}.
#' Which can be useful to calculate diameter from optically determined band gap.
#'
#' @param bandgap numeric, in eV (vector)
#'    Band gap may not be smaller than the bulk band gap of the material (3.30 eV for ZnO)
#' @param sc semiconductor, string (function only supports ZnO)
#' @param a parameter a, numeric (optional, default a = 3.940)
#' @param b parameter b, numeric (optional, default b = 0.293)
#' @param c parameter c, numeric (optional, default c = 3.30)
#'
#' @return diameter, in nanometres (vector)
#' @export
diameter <- function(bandgap, sc = "ZnO", a, b, c) {
   # set parameters a, b, c if the user did not pass them as arguments
   if (missing(a)) {a <- 3.940}
   if (missing(b)) {b <- 0.293}
   if (missing(c)) {c <- 3.30}
   # check the other arguments
   if (sc != "ZnO") stop("Error: At this time, this function only supports ZnO")

   # handling of band gap values less than ZnO bulk
   unphysical.bandgaps <- which(bandgap < 3.30)
   if (length(unphysical.bandgaps) > 0) {
      # print an informative warning message
      warning(common::simpleCap(common::numbers2words(length(unphysical.bandgaps))),
              " (", length(unphysical.bandgaps), ")",
              ifelse(length(unphysical.bandgaps) > 1,
                     " supplied band gap values are less than ",
                     " supplied band gap value is less than "),
              c, " eV\n",
              ifelse(length(unphysical.bandgaps) > 1,
                     "Treating them as NAs and moving on.",
                     "Treating it as NA and moving on."))
      bandgap[unphysical.bandgaps] <- as.numeric(NA)
   }

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
#' @param a parameter a, numeric (optional, default a = 3.940)
#' @param b parameter b, numeric (optional, default b = 0.293)
#' @param c parameter c, numeric (optional, default c = 3.30)
#'
#' @return bandgap, in eV (vector)
#' @export
bandgap <- function(diameter, sc = "ZnO", a, b, c) {
   # set parameters a, b, c if the user did not pass them as arguments
   if (missing(a)) {a <- 3.940}
   if (missing(b)) {b <- 0.293}
   if (missing(c)) {c <- 3.30}
   # check the other arguments
   if (sc != "ZnO") stop("Error: At this time, this function only supports ZnO")

   # calculate bandgap
   bandgap <- c + (b / diameter) + (a / diameter^2)
   return(bandgap)
}
