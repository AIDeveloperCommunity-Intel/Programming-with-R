##Plotting PCA (Principal Component Analysis)

library(ggfortify)
df <- iris[c(1, 2, 3, 4)]
#autoplot(prcomp(df))

#autoplot(prcomp(df), data = iris, colour = 'Species')

#autoplot(prcomp(df), data = iris, colour = 'Species', label = TRUE, label.size = 3)

#autoplot(prcomp(df), data = iris, colour = 'Species', shape = FALSE, label.size = 3)

#autoplot(prcomp(df), data = iris, colour = 'Species', loadings = TRUE)

#autoplot(prcomp(df), data = iris, colour = 'Species', loadings = TRUE, loadings.colour = 'blue', loadings.label = TRUE, loadings.label.size = 3)

autoplot(prcomp(df), scale = 0)


## Plotting K-means

set.seed(1)
#autoplot(kmeans(USArrests, 3), data = USArrests)
autoplot(kmeans(USArrests, 3), data = USArrests, label = TRUE, label.size = 3)


## Plotting cluster package

library(cluster)
#autoplot(clara(iris[-5], 3))

#autoplot(fanny(iris[-5], 3), frame = TRUE)

autoplot(pam(iris[-5], 3), frame = TRUE, frame.type = 'norm')
