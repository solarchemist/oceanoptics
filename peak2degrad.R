
##################################################
################# peak2degrad ####################
##################################################
peak2degrad <- function(peak.exp) {
   ## Description:
   ##   Calculate photocatalytic degradation from the 
   ##   change of one absorption peak at different time
   ## Usage:
   ##   peak2degrad(peak.exp)
   ## Arguments:
   ##   peak.exp: df with wavelength and intensity data for one absorption peak per sample
   ##             with the following REQUIRED columns
   ##             >> sampleid   : 
   ##             >> minutes    : 
   ##             >> idunique   : 
   ##             >> n_spectra  : 
   ##             >> wavelength : 
   ##             >> intensity  : 
   ## Value:
   ##   Dataframe with the following columns:
   ##   $ sampleid     : chr
   ##   $ minutes      :
   ##   $ idunique     :
   ##   $ n_spectra    :
   ##   $ wl.low       :
   ##   $ wl.high      :
   ##   $ wl.mean      :
   ##   $ abs.mean     :
   ##   $ degrad.ratio :
   ##   $ degrad.log   :
   #
   

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

