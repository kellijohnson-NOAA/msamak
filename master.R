#' Master script to run msamak

dirmaster <- "d:/msamak"
if (!file.exists(dirmaster)) {
  dirmaster <- "c:/msamak"
}
setwd(dirmaster)

# Label directories
dirmodel <- "model"
dirms <- "ms"
dirom <- "om"
dirsample <- "sample"

library(doParallel)
library(foreach)
library(ggplot2)

ignore <- mapply(source, list.files("R", full.names = TRUE))

#' Copy files from model to OM file
#' The folder "model" holds the parameter estimates used as true
#' values in the simulation
copyfiles <- list.files(dirmodel, full.names = TRUE)
dir.create(dirom)
ignore <- file.copy(copyfiles, dirom, recursive = TRUE)
setwd(dirom)
system(list.files(pattern = "\\.exe"), show.output.on.console = TRUE)
setwd(dirmaster)
assign(dirom, new.env())
attr(om, "name") <- dirom
sys.source(file.path(dirom, "ms.rep"), envir = eval(parse(text = dirom)))

dir.create(dirsample)
setwd(dirsample)
system(list.files(pattern = "\\.exe"), show.output.on.console = TRUE)
setwd(dirmaster)
assign(dirsample, new.env())
attr(sample, "name") <- dirsample
sys.source(file.path(dirsample, "ms.rep"), envir = eval(parse(text = dirsample)))

#' Run the simulation where a single input sample size
#' is used to generate data, but five levels of sample
#' sizes are used for EMs, two above and two below the true.
sim(iteration = 1:5,
  nom = 100, nem = list(1, 100, 200, 1000),
  dirom = "om",
  verbose = FALSE)


#' Create plots
plot_weightatage(file.path(dirms, "wegithatage.jpeg"))

#' End of file.
