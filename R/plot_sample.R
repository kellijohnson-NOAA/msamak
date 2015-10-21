#' @example
#' plot_sample()

plot_sample <- function(environment = om, save = NULL, ...) {

  allyears <- min(get("styr_rec", environment)):get("endyr", environment)
    fsh <- get("yrs_fsh_comp", environment)
    fsh <- data.frame("time" = unlist(fsh),
        "fleet" = rep(seq(sapply(fsh, length)), sapply(fsh, length)),
        "species" = rep(c("pollock", "mackerel", "cod"),
          c(sapply(fsh, length)[1:2], sum(sapply(fsh, length)[3:5]))),
        "size" = rep(get("ncomps_fsh", environment), sapply(fsh, length)),
        "type" = "fishery")
    srv <- get("yrs_srv_comp", environment)
    srv <- data.frame("time" = unlist(srv),
        "fleet" = rep(seq(sapply(srv, length)), sapply(srv, length)),
        "species" = rep(c("pollock", "mackerel", "cod"),
          sapply(srv, length)),
        "size" = rep(get("ncomps_srv", environment), sapply(srv, length)),
        "type" = "survey")
    stm <- get("yrs_stomwts", environment)
    stm <- data.frame("time" = unlist(stm),
        "fleet" = rep(seq(sapply(stm, length)), sapply(stm, length)),
        "species" = rep(c("pollock", "mackerel", "cod"),
          sapply(stm, length)),
        "size" = unlist(lapply(get("stoms_w_N", environment),
          function(x) apply(x, 2, sum))),
        "type" = "diet")
  points <- rbind(fsh, srv, stm)
  points$species <- factor(points$species,
    levels = c("pollock", "mackerel", "cod"))

g <- ggplot(data = points,
  aes(x = time, y = fleet)) +
  geom_point(data = points, aes(x = time, y = fleet, size = size),
    shape = 0) +
  facet_grid(species ~ type, scales = "free_y") +
  xlab("year") + ylab("fleet") +
  theme_bw() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        axis.text = element_text(size = 12),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black"),
        legend.key = element_rect(colour = "white"),
        legend.title = element_text(size = 0),
        legend.text = element_text(size = 7),
        legend.position = c(0.08, 0.85),
        plot.title = element_text(lineheight = 0.8, size = 10)
   )

  if(!is.null(save)) {
    g
    ggsave(save, ...)
    graphics.off()
  } else return(g)
}
