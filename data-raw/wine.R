wine <-
  read.csv(
    "https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data",
    header = FALSE
  )

names(wine) <-
  c(
    "Class",
    "Alcohol",
    "Malic acid",
    "Ash",
    "Alcalinity of ash",
    "Magnesium",
    "Total phenols",
    "Flavanoids",
    "Nonflavanoid phenols",
    "Proanthocyanins",
    "Color intensity",
    "Hue",
    "OD280/OD315 of diluted wines",
    "Proline"
  )

wine$Class <- factor(wine$Class)

#wine <- readRDS("data/wine.rds")
save(wine, file = "data/wine.Rdata")


# P <- ncol(wine) - 1
# res <- nca(wine[-1], labels=wine$Class, N_iter = 100, A_init = matrix(runif(2*P), ncol=P))
# res
#
# plot(t(tcrossprod(res$A, as.matrix(wine[-1]))), col=wine$Class)
# plot(as.data.frame(t(res$A %*% as.matrix(wine[-1]))))
