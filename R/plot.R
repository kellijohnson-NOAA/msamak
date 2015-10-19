
plotts <- function(folder = "100", value = "pred_rec",
  speciesnames = c("pollock", "mackerel", "cod")) {

equalts <- function(list, folder) {
  list <- lapply(list, function(x) {
    length <- max(sapply(list, length))
    name <- speciesnames[substitute(x)[[3]]]
    data.frame("val" = x, "pos" = name,
      "time" = (length - length(x) + 1):length,
      "folder" = folder)
    })
  do.call("rbind", list)
}
# browser()
x <- do.call("rbind", lapply(folder, function(x) {
  keep <- c(ls(), "xx")
  source(file.path(x, "ms.rep"), local = TRUE)
  xx <- equalts(eval(parse(text = value)),
    folder = folder[substitute(x)[[3]]])
  remove <- ls()[!ls() %in% keep]
  rm(list = remove)
  return(xx)
}))

ggplot(data = x,
  aes(x = time, y = val)) +
  geom_point(data = x, aes(x = time, y = val)) +
  facet_grid(pos ~ folder, scales = "free_y") +
  xlab("year") + ylab(gsub("_", " ", substitute(value))) +
  theme_bw() +
  geom_vline(lty = 2, xintercept =
    get("styr", envir = envom) - get("styr_pred", envir = envom)) +
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
  # geom_text(aes(x, y, label = lab),
  #   data = data.frame(x = 35, y = Inf,
  #   lab = round(c(get("steepness", envir = envom),
  #           get("steepness", envir = envsample),
  #           get("steepness", envir = env100)), digits = 3),
  #   pos = rep(speciesnames, 3),
  #   folder = rep(folder, each = 3)), vjust = 2.5) +
  # ggtitle("Estimated recruitment across models (column: OM, true sample sizes, 100 every year)
  #   and species (row). Dashed line indicates start of data and steepness is printed in right corner.")

}
# plotts(folder = c("model", "sample", "100"), value = "Sp_Biom")
# plotts(folder = c("model", "sample", "100"), value = "pred_rec")
