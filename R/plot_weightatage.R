#' Plot weight at age for each species in the model.

#' @param fn The filename to use when saving the plot to the disk.
#'   If \code{NULL} the file is not saved and is instead printed to
#'   the screen.
#' @param envir An object specifying the \code{environment} which stores
#'   the weight-at-age information, which will be in a \code{list}.
#' @return A plot is either written to the disk in the format specified
#'   in the file extension of \code{fn} or is printed to the screen, which
#'   can then be saved using \code{ggsave}.

plot_weightatage <- function(fn = NULL, envir = om) {
  weight <- get("wt_pop", envir = envir)
  ages <- get("nages", envir = envir)
  data <- do.call("rbind", lapply(seq_along(weight), function(x) {
    data.frame("weight" = weight[[x]],
               "age" = 1:ages[x],
               "species" = c("pollock", "mackerel", "cod")[x])
  }))

  ggplot(data = data, aes(x = age, y = weight)) +
  geom_point(data = data, aes(x = age, y = weight, pch = species)) +
  xlab("age (yrs)") + ylab("weight (kg)") +
  theme_bw() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black"),
        legend.key = element_rect(colour = "white"),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.position = c(0.1, 0.75),
        plot.title = element_text(lineheight = 0.8, size = 10)
  )
  ggsave(fn)
  if(!is.null(fn)) dev.off()
}
