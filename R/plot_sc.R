plot_sc <- function(x, ylab, environment = om, save = NULL,
  textsize = 12, ...) {

  if(length(levels(x$em)) == 5) {
    x$em <- factor(x$em, levels = levels(x$em)[c(1:3, 5, 4)])
  }

g <- ggplot(data = x,
  aes(y = val)) +
  geom_boxplot(data = x, aes(x = em, y = val)) +
  facet_grid(species ~ ., scales = "free_y") +
  ylab(ylab) +
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
  if(!is.null(save)) {
    g
    ggsave(save, ...)
    dev.off()
  } else return(g)
}
