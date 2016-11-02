#' Read OceanOptics ASCII datafile to dataframe
#'
#' Read OceanOptics processed spectrum files
#' and collect the wavelength and intensity data
#' plus some of the metadata that the spectrum
#' file contains.
#'
#' @details The internals of this function need to be rewritten.
#'    Also, the argument "version" is kind of weird and hard to
#'    understand, even for me.
#'
#' @param datafile  string with full path to experimental file
#' @param version   either "1" or "2"
#'
#' @return dataframe
#' @export
OO2df <- function(datafile, version = "1") {
   # options(stringsAsFactors=FALSE)

   data.start.rexp <- ">+Begin[\\s\\w]*<+"
   data.end.rexp <- ">+End[\\s\\w]*<+"

   # Specification of header parameters to include in output dataframe
   header.param.rexp <-
      c(DateTime           = "^Date:",
        IntegrationTime    = "^Integration Time \\(usec\\):",
        n_Averaged         = "^Spectra Averaged:",
        Boxcar             = "^Boxcar Smoothing:",
        CorrElectricDark   = "^Correct for Electrical Dark:",
        StrobeLampEnabled  = "^Strobe/Lamp Enabled:",
        CorrDetectorNonLin = "^Correct for Detector Non-linearity:",
        CorrStrayLight     = "^Correct for Stray Light:",
        n_Pixels           = "^Number of Pixels")

   # Read the input file
   dfile <- file(datafile, "r")
   # Note that readLines apparently completely skips empty lines.
   # That causes line numbers to not match between source and f vector.
   f <- readLines(dfile, n=-1) # read _all_ lines from data file
   close(dfile)
   #
   if (version == "1") {
      # Run the original version of this function
      # which assumes a filename of the format <0305-7B0802-001>
      # CANNOT HANDLE MULTIPLE SPECTRA OF THE SAME SAMPLE
      # -------------------------------------------------
      # Fetch a sampleid for the current job
      sampleid <- common::ProvideSampleId(datafile)

      #    # Look for header start rows
      #    range.header.start.rows <- which(regexpr(range.header.start.rexp, f) == 1)
      #    # Look for header end rows
      #    range.header.end.rows <- which(regexpr(range.header.end.rexp, f) == 1)

      # Look for data start marker line
      range.data.start.rows <- which(regexpr(data.start.rexp, f, perl = TRUE) == 1) + 1
      # Look for data end marker line
      range.data.end.rows <- which(regexpr(data.end.rexp, f, perl = TRUE) == 1) - 1

      # Calculate number of ranges
      ranges.total <-
         ifelse(length(range.data.start.rows) == length(range.data.end.rows),
                length(range.data.start.rows),
                NA) #why would they not be equal?
      if (is.na(ranges.total)) {
         # Obviously something bad happened.
         # Do something about it. echo an error message perhaps.
         # But why would they not be equal?

      }

      # Header always precedes start of data
      range.header.end.rows <- range.data.start.rows - 2
      if (ranges.total > 1) {
         range.header.start.rows <- c(1, range.data.end.rows[2:length(range.data.end.rows)])
      } else {
         # Data in fact only contains one range
         range.header.start.rows <- 1
      }

      # Extract headers (as-is) and put them in a list (by range)
      headers.raw <- list()
      for (range in 1:ranges.total) {
         headers.raw[[range]] <- f[range.header.start.rows[range]:range.header.end.rows[range]]
      }

      ####

      # Extract data (as-is) and put it an list (by range)
      data.raw <- list()
      for (range in 1:ranges.total) {
         data.raw[[range]] <- f[range.data.start.rows[range]:range.data.end.rows[range]]
         # Replace commas by dots
         data.raw[[range]] <- gsub(",", "\\.", data.raw[[range]])
      }

      # Collect data and header parameters in dataframes, by range in a list
      data <- list()
      for (range in 1:ranges.total) {
         zz <- textConnection(data.raw[[range]], "r")
         data[[range]] <- data.frame(stringsAsFactors = FALSE,
                                     sampleid,
                                     common::int2padstr(range, "0", 3),
                                     matrix(scan(zz, what = numeric()), ncol = 2, byrow = T))
         close(zz)
         # Collect header parameters
         for (param in 1:length(header.param.rexp)) {
            data[[range]] <-
               cbind(stringsAsFactors = FALSE,
                     data[[range]],
                     # Matches any word, digit, plus, or minus character
                     # surrounded by parentheses at the end of the string
                     sub("\\s\\([\\w\\d\\+\\-]+\\)$", "",
                         strsplit(headers.raw[[range]][which(regexpr(unname(header.param.rexp[param]),
                                                                     headers.raw[[range]]) == 1)], ": ")[[1]][2],
                         perl = TRUE))
         }
         names(data[[range]]) <-
            c("sampleid", "range", "wavelength", "intensity", names(header.param.rexp))
      }

      # Create a unified dataframe
      data.df <- data[[1]]
      if (ranges.total > 1) {
         for (range in 2:ranges.total) {
            data.df <- rbind(data.df, data[[range]])
         }
      }

      # DOES NOT WORK AS INTENDED - Convert the DateTime column to more legibly (and compact) format
      ##data.df$DateTime <-
      ##   format(as.POSIXct(gsub("\\s[A-Z]{4}\\s", " ", data.df$Date),
      ##                     format = "%a %b %d %H:%M:%S %Y"),
      ##          format = "%Y-%m-%d %H:%M:%S")
      # return
      return(data.df)
      # END OF VERSION==1
   }
   #
   if (version == "2") {
      # Run the improved code that handles multiples spectra per sample
      # NOTE: assumes filename formatted as <0305-7B0802-21-02>
      # explanation of the format: <<date-sampleid-minutes-number>>
      # -----------------------------------------------------------
      # Fetch a sampleid for the current job
      sampleid <- common::ProvideSampleId(pathexpfile=datafile, implementation="dirname")
      # Look for data start marker line
      data.start.row <- which(regexpr(data.start.rexp, f, perl = TRUE) == 1) + 1
      # Look for data end marker line
      data.end.row <- which(regexpr(data.end.rexp, f, perl = TRUE) == 1) - 1
      # Look for header rows (header always precedes start of data)
      header.end.row <- data.start.row - 2
      header.start.row <- 1
      # Extract rows of header (as-is) and save them to a variable
      headers.raw <- f[header.start.row:header.end.row]
      # Extract rows of data (as-is) and save them to a variable
      data.raw <- f[data.start.row:data.end.row]
      # IMPORTANT STEP: replace commas in source-file by dots
      data.raw <- gsub(",", "\\.", data.raw)
      # Get minutes and number from filename
      # gsub removes the file extension (including the dot)
      fname <- gsub("\\.[^.]+$", "", basename(datafile))
      fnamesplit <- strsplit(x=fname, split="-")
      # A quick check: length of split should be 4
      if (length(fnamesplit[[1]]) != 4) {
         # Throw some kind of warning
         warning("Filename of source not of the expected format! Errors will follow.")
      }
      # Collect data in dataframe
      zz <- textConnection(data.raw, "r")
      data <- data.frame(stringsAsFactors = FALSE,
                         sampleid,
                         as.numeric(fnamesplit[[1]][3]), #minutes
                         fnamesplit[[1]][4], #rep
                         paste(fnamesplit[[1]][3], fnamesplit[[1]][4], sep = "-"),
                         as.numeric(NA),
                         matrix(scan(zz,
                                     what = numeric()),
                                ncol = 2,
                                byrow = T))
      close(zz)
      # Collect header parameters for inclusion in dataframe
      for (i in 1:length(header.param.rexp)) {
         data <- cbind(data,
                       # Remove any text in parentheses at the end of the string
                       sub("\\s\\([\\w\\d\\+\\-]+\\)$", "",
                           # split the header row at the comma mark (to separate label and data)
                           strsplit(headers.raw[which(regexpr(unname(header.param.rexp[i]),
                                                              headers.raw) == 1)],
                                    ": ")[[1]][2],
                           perl = TRUE))
      }
      names(data) <-
         c("sampleid",
           "minutes",
           "rep",
           "idunique",
           "n_spectra",
           "wavelength",
           "intensity",
           names(header.param.rexp))
      # return
      return(data)

      # END OF VERSION==2
   }
}
