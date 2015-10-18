#' @param dirom
#' @param dirout
#' @param fndat_in
#' @param fndat_out
#' @param rndm.index Can be one of three types of values, if \code{NULL}
#'   then the index is not randomized. If a single numeric value, then this
#'   value is used as the standard error when randomizing the index data.
#'   Finally if \code{TRUE} then the randomized values for each index survey
#'   is based on the input standard error.
#' @param rndm.compfsh Can be one of three types of values, if \code{NULL}
#'   then the fishery compositions are not randomized.
#'   If a single numeric value, then this value is used as the
#'   number of samples per year when randomizing the fishery composition data.
#'   Finally if \code{TRUE} then the randomized values for each year off
#'   fishery composition data is based on the input value.
#' @param rndm.compsrv Can be one of three types of values, if \code{NULL}
#'   then the survey compositions are not randomized.
#'   If a single numeric value, then this value is used as the
#'   number of samples per year when randomizing the survey composition data.
#'   Finally if \code{TRUE} then the randomized values for each year off
#'   survey composition data is based on the input value.
#' @param rndm.diets Can be one of three types of values, if \code{NULL}
#'   then the diet data are not randomized.
#'   If a single numeric value, then this value is used as the
#'   number of samples per year when randomizing the diet data.
#'   Finally if \code{TRUE} then the randomized values for each year off
#'   diet data is based on the input value.
#' @param disc.phases A logical value specifying estimation if \code{TRUE}
#'   and generating if \code{FALSE}. Default is \code{TRUE}.
#' @param verbose A logical value defining how output should be printed to
#'   the screen.

om <- function(dirom = "model", dirout = "sample",
  fndat_in = "ms.dat", fndat_out = "sample.dat",
  rndm.index = TRUE, rndm.compfsh = TRUE, rndm.compsrv = TRUE,
  rndm.diets = TRUE,
  disc.phases = TRUE,
  verbose = FALSE) {

  # File structure
  infile <- list.files(dirom, pattern = fndat_in, full.names = TRUE)
  copyfiles <- list.files(dirom, pattern = "ms\\.[[:alpha:]]{3}",
    full.names = TRUE)
  # path to read from to get estimated values
  estimates <- list.files(dirom, pattern = "rep", full.names = TRUE)
  # path to write generating files to
  if (file.exists(dirout)) {
    stop(paste("The directory", dirout, "already exists, stopping om"))
  }
  dir.create(dirout, recursive = TRUE, showWarnings = verbose)
  # Copy the files from the OM to the new folder
  ignore <- file.copy(copyfiles, dirout, recursive = TRUE)
  if (any(!ignore)) {
    stop(paste("Some of the OM files: ",
         paste(copyfiles[!ignore], collapse = ", "),
         "were not copied into the sampling folder"))
  }

  # Data
  # Read in estimated values
  source(estimates)
  data <- dat_read(infile)
  est.ON <- TRUE          # T if estimating parameters
  maxDietN <- FALSE       # F for full diet weightings, F for max 20

  # Manipulate data

  # Read from pin file
  if (est.ON) {
    data$pin <- 1
    # Reset Phases to zero
    # Set to 1 to override phases
    data$ResetPhasesToZero <- 0
  }

  # predation: "1, 1" or "1, 0" for est_0
  # Discount first phase
  data$Disc_first_phase <- 1
  if (disc.phases) {
    # Discount any phase by resetting phases to zero
    data$Disc_any_phases <- 1
  } else {
    # Discount any phase by resetting phases to zero
    data$Disc_any_phases <- 0
  }

  # Catches
  # Assign tiny predicted catches to zero
  catch_e <- lapply(pred_catch,
    function(x) {x[x < 1e-8] <- 0; return(x)})

  # Diet
  # Set diet data to estimated values
  data$diet_l_dat <- T_hat
  # Reduce Q_hat to those years in the model and then to those years
  # where data is available for each species
  counter <- 0
  temp <- lapply(split(data$yrs_stomwts, rep(1:nspp, data$nyrs_stomwts)),
   function(x) data$styr:data$endyr %in% x)
  for (rsp in seq(nspp)) {
  for (ksp in seq(nspp + 1)){
    counter <- counter + 1
    data$diet_w_dat[[counter]] <-
      Q_hat[[counter]][, (nyrs_pred - nyrs+1):nyrs_pred]
    # now reduce 25 years to actual data years
    data$diet_w_dat[[counter]] <-
      data$diet_w_dat[[counter]][, temp[[rsp]]]
    }
  }

  # Manipulate years
  yrs_srv <- split(data$obs_srv$year, data$obs_srv$species)

  # Sampling
  # Survey
  if(!is.null(rndm.index)) {
    # Set the sd based on input value or truth
    if (rndm.index != TRUE) {
      data$obs_srv$se <- rndm.index / 100
    }
    # Get predicted values
    data$obs_srv$obs <- unlist(pred_srv)[
      unlist(lapply(yrs_srv, function(y) data$styr:data$endyr %in% y))]
    # Sample from those predicted values and the input se
    data$obs_srv$obs <- mapply(rnorm, mean = data$obs_srv$obs,
      sd = data$obs_srv$se, MoreArgs = list(n = 1))
    if(any(data$obs_srv$obs < 0)) data$obs_srv$obs[data$obs_srv$obs < 0] <- 0
  } # end if rndm.index
  # Fishery compositions
  if(!is.null(rndm.compfsh)) {
    #ec_fsh is the estimated fishery compositions
    for(ifsh in seq(nfsh)) {
      if (rndm.compfsh != TRUE) {
        data$comp_fsh[[ifsh]]$nsamp <- rndm.compfsh
      }
      for(iyr in seq(data$nyrs_fsh_comp[ifsh])) {
        ncols <- NCOL(data$comp_fsh[[ifsh]])
        data$comp_fsh[[ifsh]][iyr, 3:ncols] <- prop.table(rmultinom(1,
          size = sapply(data$comp_fsh, "[", "nsamp")[[ifsh]][iyr],
          prob = ec_fsh[[ifsh]][iyr, ]))
      }
    }
  } # end if rndm.compfsh
  # Survey compositions
  if(!is.null(rndm.compsrv)) {
    #ec_srv is the estimated survey compositions
    for(isrv in seq(nsrv)) {
      if (rndm.compsrv != TRUE) {
        data$comp_srv[[isrv]]$nsamp <- rndm.compsrv
      }
      for(iyr in seq(data$nyrs_srv_age)) {
        ncols <- NCOL(data$comp_srv[[isrv]])
        data$comp_srv[[isrv]][iyr, 3:ncols] <- prop.table(rmultinom(1,
          size = sapply(data$comp_srv, "[", "nsamp")[[isrv]][iyr],
          prob = ec_srv[[isrv]][iyr, ]))
      }
    }
  } # end if rndm.compsrv
  # Diet data
  if (!is.null(rndm.diets)) {
    rksp <- 0
    for (isp in seq(nspp)) {
      # Prey weights
      splits <- split(1:length(data$diet_w_dat), rep(1:nspp, each = nspp + 1))
      if (rndm.diets != TRUE) {
        stoms_w_N <- lapply(stoms_w_N, function(x) {
          x <- matrix(rndm.diets, ncol = NCOL(x), nrow = NROW(x))
          return(x)
        })
        stoms_l_N <- lapply(stoms_l_N, function(x) {
          x <- matrix(rndm.diets, ncol = NCOL(x), nrow = NROW(x))
          return(x)
        })
        data$stoms_wt <- stoms_w_N
        data$stoms_ln <- stoms_l_N
      }
        for(ir in seq(NROW(data$diet_w_dat[[splits[[isp]][1]]]))) {
        for(ic in seq(NCOL(data$diet_w_dat[[splits[[isp]][1]]]))) {
          temp <- sapply(lapply(data$diet_w_dat[splits[[isp]]], as.data.frame),
           "[", ir, ic)
          if (sum(temp) < 1e-10) next
          if (stoms_w_N[[isp]][ir, ic] == 0) {
            temp <- rep(0, nspp + 1)
          } else {
          # Resample the diets for a predator length bin in a given year
          # for each prey type (1 per species and other)
          # A minimal sample size of 1 if no samples were taken for that
          # predator x lengthbin x year category
          # DK used the sample size of all stomachs for that predator x
          # length bin combination over all years which I think is wrong.
          temp <- prop.table(rmultinom(1,
            size = stoms_w_N[[isp]][ir, ic],
            prob = temp))
          }
          for (list in seq(1+nspp)) {
            data$diet_w_dat[[splits[[isp]][list]]][ir, ic] <- temp[list]
          }
        }}
      # Prey lengths
      for(ksp in seq(nspp)) {
        rksp <- rksp + 1
        for(rln in 1:nrow(data$diet_l_dat[[rksp]])) {
          if(sum(data$diet_l_dat[[rksp]][rln, ]) > 1e-10) {
            # produce randomized counts
            data$diet_l_dat[[rksp]][rln, ] <- prop.table(rmultinom(1,
              size = sum(stoms_l_N[[rksp]][rln, ]),
              prob = data$diet_l_dat[[rksp]][rln, ]))
          }
        }
      }
    }
  } # end if rndm.diets

  data$i_wt_yrs_all <- rep(data$nyrs_stomwts, each = 4)

  # Write the output to the disk
  dat_write(info = data, fn_out = file.path(dirout, fndat_out))
}
