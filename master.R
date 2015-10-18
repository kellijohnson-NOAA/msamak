#' Master script to run msamak

dirmaster <- "d:/msamak"
setwd(dirmaster)

library(doParallel)
library(foreach)
library(ggplot2)

ignore <- mapply(source, list.files("R", full.names = TRUE))

#' Copy files from model to OM file
#' The folder "model" holds the parameter estimates used as true
#' values in the simulation
copyfiles <- list.files("model", full.names = TRUE)
dir.create("om")
ignore <- file.copy(copyfiles, "om", recursive = TRUE)

om(fndat_out = "ms.dat")
om(fndat_out = "ms.dat", dirout = "100",
  rndm.index = 100, rndm.compfsh = 100, rndm.compsrv = 100,
  rndm.diets = 100)

setwd("sample")
system(list.files(pattern = "\\.exe"))
setwd(dirmaster)
setwd("100")
system(list.files(pattern = "\\.exe"))
setwd(dirmaster)

#' Run the simulation where a single input sample size
#' is used to generate data, but five levels of sample
#' sizes are used for EMs, two above and two below the true.
sim(iteration = 1:5,
  nom = 100, nem = list(1, 50, 100, 200, 1000),
  dirom = "om",
  verbose = FALSE)

#' End of file.
