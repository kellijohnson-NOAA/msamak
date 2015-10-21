#' @example
#' plotts(formatval(getval("Sp_Biom")), "Spawning biomass")
plotts <- function(x, ylab, plot.data = TRUE, environment = om) {

levels(x$em) <- levels(x$em)[c(0, order(as.numeric(sapply(strsplit(
  grep("_", unique(x$em), value = TRUE), "_"), "[", 1)))) + 1]

g <- ggplot(data = x,
  aes(x = time, y = val)) +
  geom_line(data = x, aes(x = time, y = val, group = iteration)) +
  facet_grid(species ~ em, scales = "free_y") +
  xlab("year") + ylab(ylab) +
  theme_bw() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black"),
        legend.key = element_rect(colour = "white"),
        legend.title = element_text(size = 7, face = "bold"),
        legend.text = element_text(size = 7, face = "bold"),
        plot.title = element_text(lineheight = 0.8, size = 10)
   )
  if(!is.null(plot.data)) {
    allyears <- min(get("styr_rec", environment)):get("endyr", environment)
    fsh <- apply(t(sapply(get("yrs_fsh_comp", environment),
      function(x) allyears %in% x)), 1, function(x) seq(allyears)[x])
    fsh <- data.frame("time" = unlist(fsh),
        "fsh" = rep(seq(sapply(fsh, length)), sapply(fsh, length)),
        "species" = rep(c("pollock", "mackerel", "cod"),
          c(sapply(fsh, length)[1:2], sum(sapply(fsh, length)[3:5]))),
        "em" = "om",
        "val" = rep(unique(sapply(ggplot_build(g)$panel$ranges, "[[", 7)[2,]),
            c(sapply(fsh, length)[1:2], sum(sapply(fsh, length)[3:5]))) *
            rep(seq(0.8, 0.95, length.out = NROW(fsh)), sapply(fsh, length)))
    srv <- apply(t(sapply(get("yrs_srv_comp", environment),
      function(x) allyears %in% x)), 1, function(x) seq(allyears)[x])
    srv <- data.frame("time" = unlist(srv),
        "srv" = rep(seq(sapply(srv, length)), sapply(srv, length)),
        "species" = rep(c("pollock", "mackerel", "cod"),
          sapply(srv, length)),
        "em" = "om",
        "val" = rep(unique(sapply(ggplot_build(g)$panel$ranges, "[[", 7)[2,]),
            sapply(srv, length)) *
            rep(seq(0.7, 0.8, length.out = NROW(srv)), sapply(srv, length)))
  g <- g +
  geom_point(data = fsh,  aes(x = time, y = val), shape = 0, size = 1) +
  geom_point(data = srv,  aes(x = time, y = val), shape = 15, size = 1) +
  scale_shape_manual(values = c(0, 15), labels = c("fishery", "survey"))

  }
  return(g)
}
