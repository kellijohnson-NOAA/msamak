#' @param dirom
#' @param n
#' @param fndat_in
#' @param verbose A logical value defining how output should be printed to
#'   the screen.

em <- function(dirom, n,
  fndat_in = "ms.dat", verbose = FALSE) {

  if (length(n) == 1) {
    n <- rep(n, times = length(grep("rndm", names(formals(om)))))
  }

  emname <- paste(n, collapse = "_")
  direm <- file.path(dirom, emname)
  copyfiles <- list.files(dirom, pattern = "ms\\.[[:alpha:]]{3}",
    full.names = TRUE)
  dir.create(direm, showWarnings = verbose)
  ignore <- file.copy(copyfiles, direm, recursive = TRUE)
  if (any(!ignore)) {
    stop(paste("Some of the OM files: ",
         paste(copyfiles[!ignore], collapse = ", "),
         "were not copied into the EM folder"))
  }

  # Read in used dat file
  infile <- list.files(direm, pattern = fndat_in, full.names = TRUE)
  ignore <- file.copy(infile, file.path(dirname(infile), "sample.dat"))
  data <- dat_read(infile)

  if (n[1] != FALSE) data$obs_srv$se <- 1 / n[1]
  if (n[2] != FALSE) {
    for (ifsh in seq(sum(data$nfsh))) {
      data$comp_fsh[[ifsh]][, "nsamp"] <- n[2]
    }
  }
  if (n[3] != FALSE) {
    for (ifsh in seq_along(data$species)) {
      data$comp_srv[[ifsh]][, "nsamp"] <- n[3]
    }
  }
  if (n[4] != FALSE) {
    data$stoms_wt <- lapply(data$stoms_wt, function(x) {
      x <- matrix(n[4], ncol = NCOL(x), nrow = NROW(x))
      return(x)
    })
    data$stoms_ln_prey <- lapply(data$stoms_ln, function(x) {
      x <- matrix(n[4], ncol = NCOL(x), nrow = NROW(x))
      return(x)
    })
  }

  # Write dat file to disk
  dat_write(info = data, fn_out = infile)
  currentdirectory <- getwd()
  on.exit(setwd(currentdirectory))
  setwd(direm)
  executable <- list.files(getwd(), "\\.exe")
  output <- system(executable, intern = TRUE)
}
