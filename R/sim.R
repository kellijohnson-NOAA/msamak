#' @param interation
#' @param nom A single value or a vector specifying sample sizes
#'   to use in the call to \code{om}.
#' @param nem A list for each em, where each element of the list
#'   contains single value or a vector specifying sample sizes
#' @param verbose

sim <- function(iteration = 1:5,
  nom = 100, nem = list(1, 100, 200, 1000),
  dirom = "om",
  verbose = FALSE) {

  originaldir <- getwd()
  on.exit(setwd(originaldir))

  library(foreach)
  library(doParallel)

  if(length(nom) == 1) {
    nom <- rep(nom, times = length(grep("rndm", names(formals(om)))))
  }

  # Run OM
  for (i in iteration) {
    set.seed(i + 100)
    om(dirom = dirom, dirout = as.character(i), fndat_in = "ms.dat",
      fndat_out = "ms.dat",
      rndm.index = nom[1], rndm.compfsh = nom[2], rndm.compsrv = nom[3],
      rndm.diets = nom[4], verbose = verbose)
  }

  # Set up parallel
  numcores <- setup_parallel()
  cl <- parallel::makeCluster(numcores)
  doParallel::registerDoParallel(cl)
  on.exit(stopCluster(cl))

  # Run EM
  for (i in iteration) {
    foreach::foreach(it = seq_along(nem),
      .export = c("dat_read", "dat_write", "em", "om",
                  "setup_parallel")) %dopar% {
      em(as.character(i),
         n = nem[[it]],
         fndat_in = "ms.dat",
         verbose = verbose
        )
    }
  }


}
