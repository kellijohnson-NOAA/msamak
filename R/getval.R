#' @param value A character value specifying the name of the object you want to
#'   read from the \code{repname} file.
#' @param dir
#' @param dirom
#' @param repname A character value specifying the name of the report file to be
#'   read in from each directory.
#' @example
#' test <- getval()
#' names(test)
getval <- function(value = "pred_rec", dir = NULL,
  dirom = "om", repname = "ms.rep") {
  if (is.null(dir)) dir <- getwd()

  # Get OM values
  environmentom <- new.env()
  sys.source(file.path(dir, dirom, repname), envir = environmentom)
  om <- get(value, environmentom)

  # List to hold all values
  ts <- list()
  nameiteration <- "om"
  nameem <- "om"
  ts[[1]] <- om
  names(ts)[1] <- "om"

  # Get iteration values
  iterations <- dir(dir, pattern = "[[:digit:]]+$")
  environmenttemp <- new.env()
  for(i in iterations) {
    ems <- dir(file.path(dir, i), pattern = "[[:digit:]]+")
    nameiteration <- c(nameiteration, i)
    for(ii in ems) {
      sys.source(file.path(dir, i, ii, repname), environmenttemp)
      ts[[length(ts) + 1]] <- get(value, environmenttemp)
      nameem <- c(nameem, ii)
    }
  }
  attributes(ts) <- list("iteration" = nameiteration, "em" = nameem)
  return(ts)
}
