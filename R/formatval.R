formatval <- function(x, nages = get("nages", om), nfsh = get("nfsh_spp", om),
  species = c("pollock", "mackerel", "cod"),
  fleets = c("pollock", "mackerel", "cod.trawl", "cod.pot", "cod.longline")) {
  # determine type
  type <- unlist(lapply(x, function(y) lapply(y, class)))[1]
  iters <- grep("[[:digit:]]+", attr(x, "iteration"), value = TRUE)

  if(type == "matrix") {
    for(i in seq(length(x))) {
      if(length(x[[i]]) == length(nages) &&
         all(sapply(x[[i]], dim)[2, ] == nages)) {
        names <- species
      } else { # End if age
        if (all(sapply(x[[i]], dim)[2, ] == rep(nages, nfsh))) {
        names <- fleets
      }
      } # End if fleet x age
        temp <- list()
        for(ii in seq(length(x[[i]]))) {
          temp[[ii]] <- data.frame("val" = c(x[[i]][[ii]]),
            "age" = rep(seq(NCOL(x[[i]][[ii]])), each = NROW(x[[i]][[ii]])),
            "time" = rep(seq(NROW(x[[i]][[ii]])), NCOL(x[[i]][[ii]])),
            "species" = names[ii])
        }
      x[[i]] <- do.call("rbind", temp)
      x[[i]] <- data.frame(x[[i]],
        "iteration" = c("om", rep(iters, each = (length(x) - 1) / length(iters)))[i],
        "em" = attr(x, "em")[i])
    }
    x <- do.call("rbind", x)
  } # End if matrix
  if(type == "numeric") {
    if(all(sapply(x, length) > length(species))) names <- fleets
      else names <- species
  temp <- list()
  length <- max(unlist(lapply(x, lapply, length)))
   for(i in seq(length(x))) {
     for(ii in seq(length(x[[i]]))) {
       temp[[length(temp) + 1]] <- data.frame(
         "val" = x[[i]][[ii]],
         "time" = (length - length(x[[i]][[ii]]) + 1):length,
         "iteration" = c("om", rep(iters, each = (length(x) - 1) / length(iters)))[i],
         "em" = attr(x, "em")[i],
         "species" = names[ii])
     }
   }
   x <- do.call("rbind", temp)
 }

  return(x)
}

