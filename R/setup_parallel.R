#' Setup parallel processing
setup_parallel <- function(number = NULL) {
  if(!requireNamespace("foreach", quietly = TRUE)) {
    stop("The foreach package is required for parallel scenario simulations.",
      call. = FALSE)
  }
  if(!requireNamespace("doParallel", quietly = TRUE)) {
    stop("The foreach package is required for parallel scenario simulations.",
      call. = FALSE)
  }
  # to satisfy R CMD check:
  getDoParWorkers <- NULL

  if (is.null(number)) {
  numcores <- Sys.getenv("NUMBER_OF_PROCESSORS")
  mode(numcores) <- "numeric"
  numcores <- numcores - 1
  } else {
    numcores <- number
  }
  return(numcores)
}
