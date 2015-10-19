
plotts <- function(folder = "sample", value = "pred_rec",
  speciesnames = c("pollock", "mackerel", "cod"),
  text = NULL) {

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

x <- do.call("rbind", lapply(folder, function(x) {
  keep <- c(ls(), "xx")
  source(file.path(x, "ms.rep"), local = TRUE)
  xx <- equalts(eval(parse(text = value)),
    folder = folder[substitute(x)[[3]]])
  remove <- ls()[!ls() %in% keep]
  rm(list = remove)
  return(xx)
}))

g <- ggplot(data = x,
  aes(x = time, y = val)) +
  geom_point(data = x, aes(x = time, y = val)) +
  facet_grid(pos ~ folder, scales = "free_y") +
  xlab("year") + ylab(gsub("_", " ", substitute(value))) +
  theme_bw() +
  geom_vline(lty = 2, xintercept =
    get("styr", envir = om) - get("styr_pred", envir = om)) +
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
if (!is.null(text)) {
  g <- g +
  geom_text(aes(x, y, label = lab),
    data = data.frame(x = 35, y = Inf,
    lab = round(c(get("steepness", envir = om),
    get("steepness", envir = sample)), digits = 3),
    pos = rep(speciesnames, length(folder)),
    folder = rep(folder, each = 3)), vjust = 2.5)
}
  g
}
# plotts(folder = c("om"), value = "Sp_Biom")
# plotts(folder = c("om", "sample"), value = "pred_rec", text = TRUE)
