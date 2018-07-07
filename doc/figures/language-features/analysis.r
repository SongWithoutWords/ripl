library("FactoMineR")
library("factoextra")
library("ape")


data.transpose = read.csv(file="table.csv", head = TRUE, check.names = FALSE)

data = as.data.frame(t(data.transpose))

statically.typed.languages = data[data$"static-typing"==1, ]

data.to.analyze = data # statically.typed.languages

data.active = data.to.analyze[, -(0:1)]

params.ncp = 5


res.pca <- PCA(
    data.active,
    ncp = params.ncp,
    graph = FALSE)

plot.to.png = function(ptp.filename, ptp.function, ptp.width = 540, ptp.height = 540) {
  png(filename = ptp.filename, width = ptp.width, height = ptp.height)
  plot.result = ptp.function()
  print(plot.result)
  dev.off()
}

# plot the principal component weights
plot.to.png("pca/principal-components.png",
  function(){fviz_eig(res.pca, addlabels = TRUE)})

gradient = c("#0099FF", "#FF4400")

axes.primary = c(1, 2)
axes.secondary = c(3, 2)

# plot the language features
plot.to.png("pca/language-features-primary-axes.png",
    function() {
      fviz_pca_var(
        res.pca,
        axes = axes.primary,
        geom = c("text", "point"),
        col.var = "cos2",
        gradient.cols = gradient,
        repel = TRUE)
    }
)

plot.to.png("pca/language-features-secondary-axes.png",
    function() {
      fviz_pca_var(
        res.pca,
        axes = axes.secondary,
        geom = c("text", "point"),
        col.var = "cos2",
        gradient.cols = gradient,
        repel = TRUE)
    }
)

# plot the languages
plot.to.png("pca/languages-primary-axes.png",
    function() {
      fviz_pca_ind(
        res.pca,
        axes = axes.primary,
        geom = c("text", "point"),
        col.ind = "cos2",
        gradient.cols = gradient,
        repel = TRUE)
    }
)

plot.to.png("pca/languages-secondary-axes.png",
    function() {
      fviz_pca_ind(
        res.pca,
        axes = axes.secondary,
        geom = c("text", "point"),
        col.ind = "cos2",
        gradient.cols = gradient,
        repel = TRUE)
    }
)

# plot hierarchical clustering on principal components
res.hcpc <- HCPC(res.pca, graph = FALSE)
plot.to.png("pca/language-taxonomy.png",
  function() {
    fviz_dend(res.hcpc)
  }
)

res.pca.transpose <- PCA(
      t(data.active),
      ncp = params.ncp,
      graph = FALSE)

res.hcpc.transpose <- HCPC(res.pca.transpose, graph = FALSE)
plot.to.png("pca/language-feature-taxonomy.png",
  function() {
    fviz_dend(
      cex = 0.9,
      res.hcpc.transpose,
      labels_track_height = 2.4)
  }
)

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