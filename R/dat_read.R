#' @param fn_in
#' @param verbose A logical value defining how output should be printed to
#'   the screen.
dat_read <- function(fn_in, verbose = FALSE) {

  # An internal function to get information out of the data file.
  getval <- function(head, tail = NULL, time = c(1, 1), data = datin,
    names = info$species) {
    top <- grep(head, data, ignore.case = TRUE)
    top <- top[time[1]]
    if (is.null(tail)) {
      while(substring(gsub("^[[:space:]]+", "", data[top]), 1, 1) == "#") {
      top <- top + 1
     }
    }
    if (!is.null(tail)) {
      end <- grep(tail, data, ignore.case = TRUE)
      end <- end[time[2]]
      while (substring(gsub("^[[:space:]]+", "", data[end]), 1, 1) == "#") end <- end - 1
    } else end <- top
    value <- data[top:end]
    value <- value[value != ""]
    value <- value[value != "#"]
    value <- gsub("^ ", "", value)
    value <- strsplit(value, "[[:space:]]+")
    if (length(value) == 1) {
      value <- lapply(value, function(x) suppressWarnings(as.numeric(x)))
      value <- value[[1]]
    } else {
        splits <- which(grepl("#", sapply(value, "[", 1)))
        bad <- sapply(sapply(value[splits], "[", 2),
          function(x) grepl("[[:digit:]]+$", x))
        if(any(bad)) value <- value[-splits[bad]]
        splits <- which(grepl("#", sapply(value, "[", 1)))
        namevals <- value[splits]
        namevals <- sapply(namevals, function(x) x[grep(head, x) + 1])
        namevals <- gsub("\\(|\\)", "", namevals)
        final <- list()
        for(i in 1:length(splits)) {
          if (i == length(splits)) {
            final[[i]] <- value[(splits[i] + 1):length(value)]
          } else {
            final[[i]] <- value[(splits[i] + 1):(splits[i + 1] - 1)]
          }
        }
        if (length(final) == 1) {
          final <- lapply(final[[1]],function(x) x[x != ""])
          value <- as.numeric(unlist(sapply(final, function(x) {
            remove <- grep("#", x)
            if (length(remove) > 0) x <- x[-c(remove[1]:length(x))]
            return(x)
          })))
        } else {
          value <- lapply(final, function(x) {
            x <- lapply(x, function(y) {
              remove <- grep("#", y)
              if (length(remove) > 0) y <- y[-c(remove[1]:length(y))]
              return(y)
            })
            x <- do.call("rbind", x)
            x <- apply(x, 2, as.numeric)
            })
          names(value) <- namevals
        }
      }
    return(value)
  }

  # Read in the file
  if (!file.exists(fn_in)) {
    stop(paste(fn_in, "does not exist"))
  }
  datin <- readLines(con = fn_in, warn = verbose)
  # Store information in info
  info <- list()

  # Header
  line <- grep(paste(rep("#", 40), collapse = ""), datin)
  header <- datin[(line[1] + 1):(line[2] - 1)]
  header <- do.call("rbind", strsplit(gsub("#+", "", header), ": "))
  info$title <- header[grep("title", header[, 1], ignore.case = TRUE), 2]
  info$fn <- header[grep("file", header[, 1], ignore.case = TRUE), 2]
  info$date <- header[grep("date", header[, 1], ignore.case = TRUE), 2]
  info$author <- header[grep("author", header[, 1], ignore.case = TRUE), 2]

  # Commands
  info$debug <- getval("Debug")
  info$pin <- getval("Pin", time = 1)
  info$ResetPhasesToZero <- getval("ResetPhasesToZero")
  info$Disc_first_phase <- getval("Disc_first_phase")
  info$Disc_any_phases <- getval("Disc_any_phases")
  info$Initial_phase <- getval("Initial_phase")
  info$Terminal_phase <- getval("Terminal_phase")

  # Specs
  info$with_pred <- getval("with_pred")
  info$resp_type <- getval("resp_type")
  temp <- getval("styr", time = 1)
  info$styr <- temp[1]
  info$endyr <- temp[2]
  info$species <- datin[grep("species names", datin) + 1]
  info$species <- strsplit(gsub("#|[[:space:]]+", "", info$species), "%")[[1]]
  info$nspp <- length(info$species)
  info$comp_type <- getval("comp_type")
  info$oldest_age <- getval("oldest_age")
  info$l_bins <- getval("l_bins", time = 1)
  info$nfsh_spp <- getval("nfsh_spp")
  info$spp_fsh <- getval("spp_fsh")

  # Catch
  info$catch <- datin[(grep("catch_bio", datin) + 1):
                (grep("wt_fsh", datin)[1] - 1)]
  info$catch <- apply(do.call("rbind", setNames(
    strsplit(info$catch[substring(info$catch, 1, 1) != "#"], " "),
    sapply(strsplit(gsub("# ", "",
      info$catch[substring(info$catch, 1, 1) == "#"]), " "), "[", 1))),
    1, as.numeric)

  # Fishery weight at age (kg)
  info$wt_fsh <- getval("wt_fsh", "nyrs_fsh_comp")

  # Fishery compositions
  info$nyrs_fsh_comp <- getval("nyrs_fsh_comp")
  info$comp_fsh <- list()
  temp <- list(getval("#yrs_fsh_comp| yrs_fsh_comp", "nsmpl_fsh"),
               getval("nsmpl_fsh", "oc_fsh"),
               getval("oc_fsh", "nsrv_spp"))
  for (ii in seq_along(info$nyrs_fsh_comp)) {
    info$comp_fsh[[ii]] <- data.frame(
      "year" = split(temp[[1]],
        rep(1:length(info$nyrs_fsh_comp), info$nyrs_fsh_comp))[[ii]],
      "nsamp" = split(temp[[2]],
        rep(1:length(info$nyrs_fsh_comp), info$nyrs_fsh_comp))[[ii]],
      temp[[3]][[ii]])
  }
  names(info$comp_fsh) <- names(temp)

  # Survey index
  info$nsrv_spp <- getval("nsrv_spp")
  info$spp_srv <- getval("spp_srv")
  info$nyrs_srv <- getval("nyrs_srv")
  info$mo_srv <-  getval("mo_srv")
  info$obs_srv <- data.frame(
    "year" = getval("[^n;]yrs_srv", "mo_srv"),
    "species" = rep(info$spp_srv, info$nyrs_srv),
    "obs" = getval("obs_srv", "obs_se_srv"),
    "se" = getval("obs_se_srv", "nyrs_srv_age"))

  # Survey compositions
  info$nyrs_srv_age <- getval("nyrs_srv_age", "[^n;]yrs_srv_age")
  info$comp_srv <- list()
  temp <- list(getval("nsmpl_srv", "oc_srv"),
               getval("[^n;]yrs_srv_age", "nsmpl_srv"),
               getval("oc_srv", "wt_srv"))
  for (i in seq_along(info$nyrs_srv_age)) {
    info$comp_srv[[i]] <- data.frame(
      "year" = split(temp[[2]],
        rep(1:length(info$nyrs_srv_age), info$nyrs_srv_age))[[i]],
      "nsamp" = split(temp[[1]],
        rep(1:length(info$nyrs_srv_age), info$nyrs_srv_age))[[i]],
      temp[[3]][[i]])
  }
  names(info$comp_srv) <- names(temp[[3]])

  # Survey weight at age
  info$wt_srv <- getval("wt_srv", "wt_pop")

  # Population weight at age
  info$wt_pop <- getval("wt_pop", "maturity")

  # Maturity at age
  info$maturity <- datin[grep("maturity ", datin)[1]:
                        (grep("spawnmo", datin)[1] - 2)]

  # Spawning month
  info$spawnmo <- grep("spawnmo", datin, value = TRUE)

  # Age length key
  info$al_key <- getval("al_key", "lbinwidth")

  # Predator length bins
  info$lbinwidth <- getval("lbinwidth", "omega_vB")

  # von Bertalanffy parameters
  info$omega_vB <- getval("omega_vB", "omega_sigma")
  info$omega_sigma <- getval("omega_sigma")

  # Stomach
  info$nyrs_stomwts <- getval("nyrs_stomwts")
  info$nyrs_stomlns <- getval("nyrs_stomlns")
  info$yrs_stomwts <- getval("#yrs_stomwts|# yrs_stomwts",
                             "#yrs_stomlns|# yrs_stomlns")
  info$yrs_stomlns <- getval("#yrs_stomlns|# yrs_stomlns", "stoms_wt")
  info$stoms_wt <- getval("stoms_wt", "stoms_ln_prey")
  info$stoms_ln_prey <- getval("stoms_ln_prey", "min_SS_w")
  info$min_SS_w <- getval("min_SS_w")
  info$max_SS_w <- getval("max_SS_w")
  info$min_SS_l <- getval("min_SS_l")
  info$max_SS_l <- getval("max_SS_l")

  # Diet
  info$diet_w_dat <- getval("diet_w_dat", "diet_l_dat")
  info$diet_l_dat <- getval("diet_l_dat", "SrType")

  # Modelling options
  info$SrType <- getval("SrType")
  info$steepnessprior <- getval("# steepnessprior|#steepnessprior")
  info$cvsteepnessprior <- getval("cvsteepnessprior")
  info$phase_srec <- getval("phase_srec")
  info$sigmarprior <- getval("# sigmarprior|#sigmarprior")
  info$cvsigmarprior <- getval("cvsigmarprior")
  info$phase_sigmar <- getval("phase_sigmar")
  info$styr_rec_est <- getval("styr_rec_est")
  info$natmortprior <- getval("natmortprior")
  info$cvnatmortprior <- getval("cvnatmortprior")
  info$natmortphase2 <- getval("natmortphase2")
  info$qprior <- getval("qprior")
  info$cvqprior <- getval("cvqprior")
  info$phase_q <- getval("phase_q")
  info$q_age_min <- getval("q_age_min")
  info$q_age_max <- getval("q_age_max")
  info$cv_catchbiomass <- getval("cv_catchbiomass")
  info$sd_ration <- getval("sd_ration")

  # Phases
  info$phase_M <- getval("phase_M")
  info$phase_Rzero <- getval("phase_Rzero")
  info$phase_fmort <- getval("phase_fmort")
  info$phase_fmort1 <- getval("phase_fmort1")
  info$phase_LogRec <- getval("phase_LogRec")
  info$phase_RecDev <- getval("phase_RecDev")
  info$phase_SelFshCoff <- getval("phase_SelFshCoff")
  info$phase_SelSuvCoff <- getval("phase_SelSuvCoff")
  info$Phasepred1 <- getval("Phasepred1")
  info$PhasePred2 <- getval("PhasePred2")
  info$Phasepred3 <- getval("Phasepred3")

  # Fishery Selectivity
  info$fsh_sel_opt <- getval("fsh_sel_opt")
  info$nselages_in_fsh <- getval("nselages_in_fsh")
  info$phase_sel_fsh <- getval("phase_sel_fsh")
  info$curv_pen_fsh <- getval("curv_pen_fsh")
  info$seldec_pen_fsh <- getval("seldec_pen_fsh")
  info$sel_change_in_fsh <- getval("sel_change_in_fsh", "srv_sel_opt")

  # Survey Selectivity
  info$srv_sel_opt <- getval("srv_sel_opt")
  info$sel_change_in_srv <- getval("sel_change_in_srv", "phase_sel_srv")
  info$phase_sel_srv <- getval("phase_sel_srv")
  info$nselages_in_srv <- getval("nselages_in_srv")
  info$curv_pen_srv <- getval("curv_pen_srv")
  info$seldec_pen_srv <- getval("seldec_pen_srv")
  info$nselages_in_srv <- getval("nselages_in_srv")
  info$curv_pen_srv <- getval("curv_pen_srv")
  info$seldec_pen_srv <- getval("seldec_pen_srv")
  info$nselages_in_srv <- getval("nselages_in_srv")
  info$curv_pen_srv <- getval("curv_pen_srv")
  info$seldec_pen_srv <- getval("seldec_pen_srv")

  invisible(info)
}
