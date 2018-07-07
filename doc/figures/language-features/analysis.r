library("FactoMineR")
library("factoextra")
library("ape")


data.transpose = read.csv(file="table.csv", head = TRUE, check.names = FALSE)

data = as.data.frame(t(data.transpose))

statically.typed.languages = data[data$"static-typing"==1, ]

data.to.analyze = data # statically.typed.languages

data.active = data.to.analyze[, -(0:1)]

plot.to.png = function(ptp.filename, ptp.function, ptp.width = 540, ptp.height = 540) {
  png(filename = ptp.filename, width = ptp.width, height = ptp.height)
  plot.result = ptp.function()
  print(plot.result)
  dev.off()
}

plot.phylogram.to.png = function(file.name, title, data) {

  plot.to.png(file.name,
    function() {

      language.distances = dist(data, method = "euclidean")

      language.hierarchical.clustering = hclust(
        language.distances,
        method = "ward.D2")

      plot(
        as.phylo(language.hierarchical.clustering),
        main = title,
        type = "phylogram",
        cex = 1.2,
        label.offset = 0.2,
        font = 1, # plain text, not bold or italic
        # xlab = "Height",
        # nodePar = nodePar,
        # horiz = TRUE
      )
    }
  )
}

plot.phylogram.to.png(
  "hierarchical-clustering-of-languages.png",
  "Hierarchical Clustering of Languages by Language Features",
  data)

plot.phylogram.to.png(
  "hierarchical-clustering-of-language-features.png",
  "Hierarchical Clustering of Language Features by Language",
  data.transpose)

plot.to.png("heatmap-of-language.png",
  function() {
    language.distances = dist(data, method = "euclidean")
    heatmap(
      data.matrix(language.distances),
      col = grey(0:255 / 255),
      symm = TRUE,
    )
  }
)

plot.to.png("multidimensional-scaling-of-languages-classic.png",
  function() {

    library(MASS)

    language.distances = dist(data, method = "euclidean")
    language.mds = cmdscale(language.distances, k=2, eig=TRUE)

    x = language.mds$points[,1]
    y = language.mds$points[,2]

    plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",
      main="Classic MDS", type="n")
    text(x, y, labels = row.names(data), cex=.7)
  }
)

plot.to.png("multidimensional-scaling-of-languages-non-metric.png",
  function() {

    library(MASS)

    language.distances = dist(data, method = "euclidean")
    language.mds = isoMDS(language.distances, k=2)

    x = language.mds$points[,1]
    y = language.mds$points[,2]

    plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",
      main="Nonmetric MDS", type="n")
    text(x, y, labels = row.names(data), cex=.7)
  }
)
