library("FactoMineR")
library("factoextra")
library("ape")

## Util

concat.strings = function(...){ paste(..., sep = "") }

plot.to.png = function(filename, f, width = 540, height = 540) {
  png(filename = filename, width = width, height = height)
  plot.result = f()
  print(plot.result)
  dev.off()
}

## Analysis

language.features = read.csv(file="table.csv", head = TRUE, check.names = FALSE)

language.features.excluding.ripl = subset(language.features, select = -c(Ripl))

languages = as.data.frame(t(language.features))

languages.excluding.ripl = as.data.frame(t(language.features.excluding.ripl))

## statically.typed.languages = languages[data$"static-typing"==1, ]

languages.to.analyze = languages # statically.typed.languages

languages.active = languages.to.analyze[, -(0:1)]

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

    library(gplots)

    heatmap.2(
      data.matrix(language.distances),
      dendrogram = "none",
      Rowv = TRUE,
      Colv = TRUE,
      col = grey(0:255 / 255),
      symm = TRUE,
      trace = "none",
      )
  }
)

correlation.method = "pearson"
plot.to.png(
  width = 1020,
  height = 1020,
  concat.strings(
    "language-feature-correlation-heatmap-",
    correlation.method,
    ".png"),
  function() {

    language.feature.cor = cor(
      languages.excluding.ripl,
      method = correlation.method)

    ## width = width(language.feature.cor)

    library(gplots)
    ## require("gplots")

    heatmap.2(
      language.feature.cor,
      dendrogram = "none",
      Rowv = TRUE,
      Colv = TRUE,
      col = colorRampPalette(c("red", "white", "blue")),
      cexRow = 1.3,
      cexCol = 1.3,
      ## margins = c(18, 18),
        ## grey(0:255 / 255),
      symm = TRUE,
      trace = "none",
      tracecol = "black",
      colsep = 0:nrow(language.feature.cor),
      rowsep = 0:ncol(language.feature.cor),
      sepwidth = c(0.001, 0.001),
      sepcolor =  "black",

      # attempt to place the histogram/colour key above the plot
      lmat=rbind(c(5, 4, 2), c(6, 1, 3)),
      lhei=c(2.5, 5),
      lwid=c(1, 10, 1),
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
