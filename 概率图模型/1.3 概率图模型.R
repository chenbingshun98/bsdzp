#source("http://bioconductor.org/biocLite.R")
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install(version = "3.12")
biocLite()
install.packages("gRain")

library(gRain)
library(gRbase)
library(Rgraphviz)
graph <- ug(~A:B:E + E:C:D)
class(graph)
plot(graph)

