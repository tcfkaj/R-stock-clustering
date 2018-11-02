library(pca3d)
load('runs/run8.Rda')
load('initial.Rda')

pca.result <- prcomp(master.kkm, center = TRUE, scale. = TRUE)
summary(pca.result)$`importance`[2,1:3] # Proportion of Variance
pca.result$x[1:5,1:5]

cluster.num <- rbf.run[1:484]
names(cluster.num) <- NULL

pca3d(pca.result, group = cluster.num, show.ellipses=T,
      ellipse.ci=0.75, show.plane=F)

pca3d(pca.result, group = cluster.num, show.ellipses=F,
      ellipse.ci=0.75, show.plane=F)

# Without grouping
pca3d(pca.result, show.ellipses=F,
      ellipse.ci=0.75, show.plane=F)

# Eigenvalues and eigenvectors
# eigen.val <- pca.result$sdev^2
# eigen.vec <- pca.result$rotation
#
# eigen.val.only3 <- eigen.val[1:3]
# eigen.vec.only3 <- eigen.vec[1:3,1:3]
#
# save(eigen.val, eigen.vec, eigen.val.only3, eigen.vec.only3, file = 'pca_eigen_val_vec.Rda')




