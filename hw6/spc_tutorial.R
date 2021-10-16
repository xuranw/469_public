rm(list=ls())
source("https://raw.githubusercontent.com/linnylin92/469_public/master/hw6/hw6_functions.R")

dat <- read.csv("https://raw.githubusercontent.com/linnylin92/469_public/master/hw6/baron.csv", row.names = 1)
cell_type <- as.factor(dat[,1])
dat <- dat[,-1]
dat <- scale(dat, center = T, scale = T)
dat[1:5,1:5]

###################################

# let's start with a naive K-means analysis
set.seed(10)
kmean_res <- stats::kmeans(dat, centers = length(unique(cell_type)))
table(kmean_res$cluster, cell_type) # confusion matrix
# below, this is the function you'll be using in the homework
# basically, this shuffles around the permutations in our estimated clusters to "optimally" align
#   each estimated cluster to the true cluster
compute_misclustering_rate(kmean_res$cluster, cell_type)

###################################

# let's first review the usual PCA
## this is roughly what you did in HW3
pca_res <- stats::prcomp(dat, center = T, scale. = T)
plot(pca_res$x[,1], pca_res$x[,2], asp = T, pch = 16, col = as.numeric(as.factor(cell_type)))
plot(pca_res$sdev) # scree plot

##################################

# now let's use sparse PCA
## first, let's use the cross validation to tune the parameter
set.seed(10)
## this function REQUIRES you to pass in a numeric matrix
class(dat)
spca_cv_res <- PMA::SPC.cv(dat, sumabsvs = seq(1.2, sqrt(ncol(dat))/2, length.out = 10))
## i.e., the following function will fail since it's not a numeric matrix
spca_cv_res <- PMA::SPC.cv(data.frame(dat), sumabsvs = seq(1.2, sqrt(ncol(dat))/2, length.out = 10))

spca_cv_res
plot(spca_cv_res)

## now, based on this tuning parameter, perform sparse PCA
## here, we compute the first K sparse eigenvectors, where K is simply the number of cell types here
## had this been a more well-thoughtout analysis, we would make scree plots to appropriately select the K here
spca_res <- PMA::SPC(dat, sumabsv = spca_cv_res$bestsumabsv1se, K = length(unique(cell_type)))

spca_res
## based on the sparse PCA, select the "informative" genes (i.e., genes that were not 0 across the leading sparse eigenvectors of the correlation matrix)
gene_idx <- unique(sort(unlist(lapply(1:ncol(spca_res$v), function(i){
  which(spca_res$v[,i]!=0)
}))))
length(gene_idx) # we select 72 genes of the 200 genes
dat_screened <- dat[,gene_idx]
dim(dat_screened)
dat_screened <- scale(dat_screened, center = T, scale = T)

## we can try using K-means again
set.seed(10)
kmean_res <- stats::kmeans(dat_screened, centers = length(unique(cell_type)))
table(kmean_res$cluster, cell_type)
compute_misclustering_rate(kmean_res$cluster, cell_type)
# in this example, we don't see much of a change. But that's just because this is an example


