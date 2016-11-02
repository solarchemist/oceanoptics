#' PC degradation by change of abs peak over time
#'
#' Calculate photocatalytic degradation from the
#' change of one absorption peak at various time.
#'
#' @param peak.exp   dataframe with wavelength and intensity data
#'
#' @details peak.exp specification:
#'    df with wavelength and intensity data for one absorption peak per sample
#'    with the following REQUIRED columns (yes I know this is an unusual way to do it...)
#'    >> sampleid
#'    >> minutes
#'    >> idunique
#'    >> n_spectra
#'    >> wavelength
#'    >> intensity
#'
#' @return dataframe with the following columns:
#'    $ sampleid     : chr
#'    $ minutes      :
#'    $ idunique     :
#'    $ n_spectra    :
#'    $ wl.low       :
#'    $ wl.high      :
#'    $ wl.mean      :
#'    $ abs.mean     :
#'    $ degrad.ratio :
#'    $ degrad.log   :
#' @export
peak2degrad <- function(peak.exp) {
   # Create counter for for-loop assignments
   n_idunique <- length(unique(peak.exp$idunique))
   # Create a dataframe (peak.mean) to hold the mean values
   peak.mean <- data.frame(matrix(data = NA, nrow = n_idunique, ncol = 10))
   names(peak.mean) <- c("sampleid",
                         "minutes",
                         "idunique",
                         "n_spectra",
                         "wl.low",
                         "wl.mean",
                         "wl.high",
                         "abs.mean",
                         "degrad.ratio",
                         "degrad.log")
   for (s in 1:n_idunique) {
      peak.mean$minutes[s] <- unique(peak.exp$minutes)[s]
      peak.mean$idunique[s] <- unique(peak.exp$idunique)[s]
      # This approach (above) works for minutes and idunique since
      # no two minutes or idunique values are the same.
      # For the n_spectra, we use the idunique value to subset the wanted part of the df
      peak.mean$n_spectra[s] <- unique(peak.exp$n_spectra[which(peak.exp$idunique == unique(peak.exp$idunique)[s])])
      # Mean wavelength across the peak (across the range of peak.approx)
      peak.mean$wl.mean[s] <- mean(peak.exp$wavelength[which(peak.exp$idunique == unique(peak.exp$idunique)[s])])
      peak.mean$wl.low[s] <- min(peak.exp$wavelength[which(peak.exp$idunique == unique(peak.exp$idunique)[s])])
      peak.mean$wl.high[s] <- max(peak.exp$wavelength[which(peak.exp$idunique == unique(peak.exp$idunique)[s])])
      # Mean intensity of the absorption peak across the peak.approx range
      peak.mean$abs.mean[s] <- mean(peak.exp$intensity[which(peak.exp$idunique == unique(peak.exp$idunique)[s])])
      # Calculate the degradation ratio
      peak.mean$degrad.ratio[s] <- peak.mean$abs.mean[s] / peak.mean$abs.mean[1]
   }
   peak.mean$sampleid <- unique(peak.exp$sampleid)
   # Calculate log degradation ratio
   peak.mean$degrad.log <- log10(peak.mean$degrad.ratio)
   #
   return(peak.mean)
}









#' Calculate average spectra (very specific, check before using)
#'
#' This function calculates a new average spectrum
#' for each "minutes" based on all its "rep".
#'
#' @details For this specific situation:
#'    minutes    rep
#'    5          01     --> spectrum
#'    5          02     --> spectrum
#'    10         01     --> spectrum
#'    10         02     --> spectrum
#'    10         03     --> spectrum
#'    So, averages over "rep". To distinguish from non-averaged
#'    spectra, we set new rep to the string "avg" (below).
#'
#' @param data.vis  df with wavelength and intensity data
#'    with the following REQUIRED columns
#'    >> minutes    :
#'    >> rep        :
#'    >> idunique   :
#'    >> n_spectra  :
#'    >> wavelength :
#'    >> intensity  :
#'
#' @return dataframe with the same columns as input df
#' @export
AverageSpectra <- function(data.vis) {
   for (i in 1:length(unique(data.vis$minutes))) {
      # Work on one minutes value at a time
      data.sub <- data.vis[which(data.vis$minutes == unique(data.vis$minutes)[i]), ]
      # Number of of rows top copy
      n_rows <- dim(data.sub)[1] / length(unique(data.sub$rep))
      # data.tmp to replaced with averaged data
      data.tmp <- data.sub[1:n_rows, ]
      # Store number of spectra used to c a l c u l at e the current average
      data.tmp$n_spectra <- length(unique(data.sub$rep))
      # C a lc u l a t e the average
      data.tmp$intensity <-
         stats::aggregate(data.sub$intensity ~ data.sub$wavelength, FUN = mean)[, 2]
      # Create new " rep "
      data.tmp$rep <- "avg"
      # Create new unique id
      data.tmp$idunique <- paste(data.tmp$minutes, data.tmp$rep, sep = "-")
      # Append to data . vis
      data.vis <- rbind(data.vis, data.tmp)
   }
   # return
   return(data.vis)
}
