formatval <- function(x) {
  # determine type
  type <- unlist(lapply(x, function(y) lapply(y, class)))[1]

  if(type == "matrix") {
    d
  }
  if(type == "numeric") {
  temp <- list()
  length <- max(unlist(lapply(x, lapply, length)))
  iters <- grep("[[:digit:]]+", attr(x, "iteration"), value = TRUE)
   for(i in seq(length(x))) {
     for(ii in seq(length(x[[i]]))) {
       temp[[length(temp) + 1]] <- data.frame(
         "val" = x[[i]][[ii]],
         "time" = (length - length(x[[i]][[ii]]) + 1):length,
         "iteration" = c("om", rep(iters, each = (length(x) - 1) / length(iters)))[i],
         "em" = attr(x, "em")[i],
         "species" = c("pollock", "mackerel", "cod")[ii])
     }
   }
   temp <- do.call("rbind", temp)
 }

  return(temp)
}
