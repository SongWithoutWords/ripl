library("FactoMineR")
library("factoextra")

data = read.csv(file="language-features.csv", head=TRUE)

data.active <- data[, -(0:1)]

res.pca <- prcomp(data, scale = FALSE)

png(filename = "pca-eig.png", width = 720, height = 720)
fviz_eig(res.pca, addlabels = TRUE)
dev.off()
system("feh pca-eig.png", wait=FALSE)


png(filename = "pca-ind.png")
fviz_pca_ind(res.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
             )
dev.off()
system("feh pca-ind.png", wait=FALSE)

png(filename = "pca-var.png", width = 720, height = 720)
fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
             )
dev.off()
system("feh pca-var.png", wait=FALSE)

png(filename = "pca-var-and-ind.png", width = 720, height = 720)
fviz_pca_biplot(res.pca, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
                )
dev.off()
system("feh pca-var-and-ind.png", wait=FALSE)

