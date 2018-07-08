library("FactoMineR")
library("factoextra")
library("ape")

## Util

concat.strings = function(...){ paste(..., sep = "") }

plot.to.png = function(ptp.filename, ptp.function, ptp.width = 540, ptp.height = 540) {
  png(filename = ptp.filename, width = ptp.width, height = ptp.height)
  plot.result = ptp.function()
  print(plot.result)
  dev.off()
}

## Analysis

language.features = read.csv(file="table.csv", head = TRUE, check.names = FALSE)

languages = as.data.frame(t(language.features))

statically.typed.languages = languages[data$"static-typing"==1, ]

languages.to.analyze = languages # statically.typed.languages

languages.active = languages.to.analyze[, -(0:1)]

language.features.excluding.ripl = subset(language.features, select = -c(Ripl))

hclust.method = "ward.D2"

plot.phylogram.to.png = function(file.name, title, languages) {

  plot.to.png(file.name,
    function() {

      language.distances = dist(languages, method = "euclidean")

      language.hierarchical.clustering = hclust(
        language.distances,
        method = hclust.method)

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
  languages)

plot.phylogram.to.png(
  "hierarchical-clustering-of-language-features.png",
  "Hierarchical Clustering of Language Features by Language",
  language.features)

plot.phylogram.to.png(
  "hierarchical-clustering-of-language-features-excluding-ripl.png",
  "Hierarchical Clustering of Language Features by Language Excluding Ripl",
  language.features.excluding.ripl)

plot.to.png("heatmap-of-language.png",
  function() {
    language.distances = dist(languages, method = "euclidean")
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

    language.distances = dist(languages, method = "euclidean")
    language.mds = cmdscale(language.distances, k=2, eig=TRUE)

    x = language.mds$points[,1]
    y = language.mds$points[,2]

    plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",
      main="Classic MDS", type="n")
    text(x, y, labels = row.names(languages), cex=.7)
  }
)

plot.to.png("multidimensional-scaling-of-languages-non-metric.png",
  function() {

    library(MASS)

    language.distances = dist(languages, method = "euclidean")
    language.mds = isoMDS(language.distances, k=2)

    x = language.mds$points[,1]
    y = language.mds$points[,2]

    plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",
      main="Nonmetric MDS", type="n")
    text(x, y, labels = row.names(languages), cex=.7)
  }
)
