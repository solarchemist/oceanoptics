##################################################
################ AverageSpectra ##################
##################################################
AverageSpectra <- function(data.vis) {
   ## Description :
   ##    For this s i t u at i o n :
   ##    minutes    rep
   ##    5          01     --> spectrum
   ##    5          02     --> spectrum
   ##    10         01     --> spectrum
   ##    10         02     --> spectrum
   ##    10         03     --> spectrum
   ##    this function calculates a new average spectrum
   ##    for each "minutes" based on all its "rep".
   ##    So, averages over "rep". To distinguish from non-averaged
   ##    spectra, we set new rep to the string "avg" (below).
   ##    << This is a very non-generic function>>
   ## Usage:
   ##    AverageSpectra(peak.exp)
   ## Arguments:
   ##    data.vis: df with wavelength and intensity data
   ##              with the following REQUIRED columns
   ##              >> minutes    :
   ##              >> rep        :
   ##              >> idunique   :
   ##              >> n_spectra  :
   ##              >> wavelength :
   ##              >> intensity  :
   ## Value:
   ##    Dataframe with the same columns as the input dataframe
   #
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
         aggregate(data.sub$intensity ~ data.sub$wavelength, FUN = mean)[, 2]
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
