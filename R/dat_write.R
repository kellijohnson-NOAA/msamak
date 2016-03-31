#' @param info
#' @param fn_out

dat_write <- function(info, fn_out, verbose = FALSE) {

  on.exit({if(sink.number()>0) sink()})

  wl <- function(name, comment = NULL, datlist = info, decimal = FALSE){
    # simple function to clean up many repeated commands
    value <- datlist[names(datlist) == name]
    value <- unlist(value)
    if (is.null(comment)) {
      writeLines(paste0(" # ", name))
    } else {
      writeLines(paste0(" # ", name, " ", comment))
    }
    if(decimal) {
      value <- format(value, nsmall = 1)
    }
    writeLines(paste(value, collapse = " "))
  }
  wll <- function(name, comment = NULL, datlist = info){
    value <- datlist[names(datlist) == name][[1]]
    if (!is.matrix(value[[1]])) {
      for (i in 1:length(value)) {
        writeLines(paste("#", name, names(value)[i]))
        writeLines(paste(value[[i]], collapse = " "))
      }
    } else {
      for (i in 1:length(value)) {
        writeLines(paste("#", name, names(value)[i]))
        writeLines(apply(value[[i]], 1, paste, collapse = " "))
      }
    }
  }

  # Open file
  if (file.exists(fn_out)) {
    if (verbose) message(paste("Removing", fn_out))
    file.remove(fn_out)
  }
  zz <- file(fn_out, open = "at")
  sink(zz)

  # Header
  writeLines(paste(rep("#", 80), collapse = ""))
  writeLines(paste("#### Title:", info$title))
  writeLines(paste("#### Filename:", fn_out))
  writeLines(paste("#### Date:", Sys.time()))
  writeLines(paste("#### Author:", info$author))
  writeLines("#### Comment: File created using dat_write function")
  writeLines(paste(rep("#", 80), collapse = ""))

  # Commands
  wl("debug")
  wl("pin")
  wl("ResetPhasesToZero")
  wl("Disc_first_phase")
  wl("Disc_any_phases")
  wl("Initial_phase")
  wl("Terminal_phase")

  # Specs
  wl("with_pred")
  wl("resp_type")
  writeLines(c("#styr, endyr", paste(info$styr, info$endyr)))
  writeLines(c("#species names",
    paste("#", paste(info$species, collapse = "%"))))
  writeLines(paste(length(info$species)))
  wl("comp_type")
  wl("oldest_age")
  wl("l_bins")
  wl("nfsh_spp")
  wl("spp_fsh")

  # Catch
  writeLines("# catch_bio")
  for (i in 1:NCOL(info$catch)) {
    writeLines(paste("#", colnames(info$catch)[i]))
    writeLines(paste(info$catch[, i], collapse = " "))
  }

  # Fishery weight at age (kg)
  for (i in 1:length(info$wt_fsh)) {
    writeLines(paste("# wt_fsh", names(info$wt_fsh)[i]))
    writeLines(apply(info$wt_fsh[[i]], 1, paste, collapse = " "))
  }

  # Fishery compositions
  wl("nyrs_fsh_comp")
  writeLines("# yrs_fsh_comp")
  writeLines(unlist(lapply(sapply(
    info$comp_fsh, "[", "year"), paste, collapse = " ")))
  writeLines("# nsmpl_fsh")
  writeLines(unlist(lapply(sapply(
    info$comp_fsh, "[", "nsamp"), paste, collapse = " ")))
  for (i in seq_along(info$comp_fsh)) {
    writeLines(paste("# oc_fsh", names(info$comp_fsh)[i]))
    temp <- lapply(info$comp_fsh, "[", -c(1:2))[[i]]
    writeLines(apply(temp, 1, paste, collapse = " "))
  }

  # Survey index
  wl("nsrv_spp")
  wl("spp_srv")
  wl("nyrs_srv")
  writeLines("# yrs_srv")
  writeLines(sapply(split(info$obs_srv$year, info$obs_srv$species),
    paste, collapse = " "))
  wl("mo_srv")
  writeLines("# obs_srv")
  writeLines(sapply(split(info$obs_srv$obs, info$obs_srv$species),
    paste, collapse = " "))
  writeLines("# obs_se_srv")
  writeLines(sapply(split(info$obs_srv$se, info$obs_srv$species),
    paste, collapse = " "))

  # Survey compositions
  writeLines("# nyrs_srv_age")
  writeLines(paste(info$nyrs_srv_age))
  writeLines("# yrs_srv_age")
  writeLines(unlist(lapply(sapply(
    info$comp_srv, "[", "year"), paste, collapse = " ")))
  writeLines("# nsmpl_srv")
  writeLines(unlist(lapply(sapply(
    info$comp_srv, "[", "nsamp"), paste, collapse = " ")))
  for (i in seq_along(info$comp_srv)) {
    writeLines(paste("# oc_srv", names(info$comp_srv)[i]))
    temp <- lapply(info$comp_srv, "[", -c(1:2))[[i]]
    writeLines(apply(temp, 1, paste, collapse = " "))
  }

  # Survey weight at age (kg)
  for (i in 1:length(info$wt_srv)) {
    writeLines(paste("# wt_srv", names(info$wt_srv)[i]))
    writeLines(apply(info$wt_srv[[i]], 1, paste, collapse = " "))
  }

  # Population weight at age
  wll("wt_pop")

  # Maturity at age
  writeLines(info$maturity)

  # Spawning month
  writeLines(info$spawnmo)

  # Age length key
  wll("al_key")

  # Predator length bins
  wl("lbinwidth")

  # von Bertalanffy parameters
  wll("omega_vB")
  wl("omega_sigma")

  # Stomach
  wl("nyrs_stomwts")
  wl("nyrs_stomlns")
  writeLines("# yrs_stomwts")
  temp <- rep(1:length(info$nyrs_stomwts), info$nyrs_stomwts)
  writeLines(sapply(split(info$yrs_stomwts, temp),
    paste, collapse = " "))
  writeLines("# yrs_stomlns")
  temp <- rep(1:length(info$nyrs_stomlns), info$nyrs_stomlns)
  writeLines(sapply(split(info$yrs_stomlns, temp),
    paste, collapse = " "))
  wll("stoms_wt")
  wll("stoms_ln_prey")
  wl("min_SS_w")
  wl("max_SS_w")
  wl("min_SS_l")
  wl("max_SS_l")

  # Diet
  wll("diet_w_dat")
  wll("diet_l_dat")

  # Modelling options
  wl("SrType")
  wl("steepnessprior")
  wl("cvsteepnessprior")
  wl("phase_srec")
  wl("sigmarprior")
  wl("cvsigmarprior")
  wl("phase_sigmar")
  wl("styr_rec_est")
  wl("natmortprior")
  wl("cvnatmortprior")
  wl("natmortphase2")
  wl("qprior", decimal = TRUE)
  wl("cvqprior")
  wl("phase_q")
  wl("q_age_min")
  wl("q_age_max")
  wl("cv_catchbiomass")
  wl("sd_ration")

  # Phases
  wl("phase_M")
  wl("phase_Rzero")
  wl("phase_fmort")
  wl("phase_fmort1")
  wl("phase_LogRec")
  wl("phase_RecDev")
  wl("phase_SelFshCoff")
  wl("phase_SelSuvCoff")
  wl("Phasepred1")
  wl("PhasePred2")
  wl("Phasepred3")

  # Fishery Selectivity
  wl("fsh_sel_opt")
  wl("nselages_in_fsh")
  wl("phase_sel_fsh")
  wl("curv_pen_fsh")
  wl("seldec_pen_fsh")
  writeLines("# sel_change_in_fsh")
  writeLines(apply(matrix(info$sel_change_in_fsh,
    nrow = sum(info$nfsh_spp),
    byrow = TRUE), 1, paste, collapse = " "))

  # Survey selectivity
  wl("srv_sel_opt")
  writeLines("# sel_change_in_srv")
  writeLines(apply(matrix(info$sel_change_in_srv,
    nrow = length(info$phase_sel_srv),
    byrow = TRUE), 1, paste, collapse = " "))
  wl("phase_sel_srv")
  wl("nselages_in_srv")
  wl("curv_pen_srv")
  wl("seldec_pen_srv")
  wl("nselages_in_srv")
  wl("curv_pen_srv")
  wl("seldec_pen_srv")
  wl("nselages_in_srv")
  wl("curv_pen_srv")
  wl("seldec_pen_srv")

  # End of file
  writeLines("# End of file")
  writeLines("999")
  sink()
  close(zz)
}
