#' @example
#' plotts(formatval(getval("Sp_Biom")), "Spawning biomass")
plot_ts <- function(x, ylab, plot.data = TRUE, environment = om, save = NULL,
  textsize = 12, ...) {

  if(length(levels(x$em)) == 5) {
    x$em <- factor(x$em, levels = levels(x$em)[c(1:3, 5, 4)])
  }

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
        axis.text = element_text(size = textsize),
        strip.text.x = element_text(size = textsize),
        strip.text.y = element_text(size = textsize),
        legend.key = element_rect(colour = "white"),
        legend.title = element_text(size = 0),
        legend.text = element_text(size = textsize),
        legend.position = c(0.07, 0.90)
   )
  if(!is.null(plot.data)) {
    allyears <- min(get("styr_rec", environment)):get("endyr", environment)
    fsh <- apply(t(sapply(get("yrs_fsh_comp", environment),
      function(x) allyears %in% x)), 1, function(x) seq(allyears)[x])
    fsh <- data.frame("time" = unlist(fsh),
        "fleet" = rep(seq(sapply(fsh, length)), sapply(fsh, length)),
        "species" = rep(c("pollock", "mackerel", "cod"),
          c(sapply(fsh, length)[1:2], sum(sapply(fsh, length)[3:5]))),
        "em" = "om",
        "val" = rep(unique(sapply(ggplot_build(g)$panel$ranges, "[[", 7)[2,]),
            c(sapply(fsh, length)[1:2], sum(sapply(fsh, length)[3:5]))) *
            rep(seq(0.8, 0.95, length.out = NROW(fsh)), sapply(fsh, length)),
        "type" = "fishery")
    srv <- apply(t(sapply(get("yrs_srv_comp", environment),
      function(x) allyears %in% x)), 1, function(x) seq(allyears)[x])
    srv <- data.frame("time" = unlist(srv),
        "fleet" = rep(seq(sapply(srv, length)), sapply(srv, length)),
        "species" = rep(c("pollock", "mackerel", "cod"),
          sapply(srv, length)),
        "em" = "om",
        "val" = rep(unique(sapply(ggplot_build(g)$panel$ranges, "[[", 7)[2,]),
            sapply(srv, length)) *
            rep(seq(0.7, 0.8, length.out = NROW(srv)), sapply(srv, length)),
        "type" = "survey")
    stm <- apply(t(sapply(get("yrs_stomwts", environment),
      function(x) allyears %in% x)), 1, function(x) seq(allyears)[x])
    stm <- data.frame("time" = unlist(stm),
        "fleet" = rep(seq(sapply(stm, length)), sapply(stm, length)),
        "species" = rep(c("pollock", "mackerel", "cod"),
          sapply(stm, length)),
        "em" = "om",
        "val" = rep(unique(sapply(ggplot_build(g)$panel$ranges, "[[", 7)[2,]),
            sapply(stm, length)) *
            rep(seq(0.6, 0.7, length.out = NROW(stm)), sapply(stm, length)),
        "type" = "diet")
  points <- rbind(fsh, srv, stm)
  g <- g +
  geom_point(data = points,  aes(x = time, y = val, shape = type), size = 2)
  }
  if(!is.null(save)) {
    g
    ggsave(save, ...)
    dev.off()
  } else return(g)
}

