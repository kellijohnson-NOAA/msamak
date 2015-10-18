

equalts <- function(list) {
  list <- lapply(list, function(x) {
    length <- max(sapply(list, length))
    y <- data.frame(
      "val" = x,
      "pos" = substitute(x)[[3]],
      "time" = (length - length(x) + 1):length)
    return(y)
    })
  return(do.call("rbind", list))
}
source(file.path("sample", "ms.rep"))

x <- equalts(pred_rec)
ggplot(data = x,
  aes(x = time, y = val)) +
  geom_point(data = x, aes(x = time, y = val)) +
  facet_grid(pos ~ ., scales = "free_y")



