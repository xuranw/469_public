rm(list=ls())

# recap: needed for the homework! let's generate random X's and binary Y's and fit a logistic regression,
#  and then extract the p-values. (This is a pointless exercise statistically.
#  The goal is to show you how to extract p-values and to make a QQ plot)
set.seed(10)
trials <- 500; n <- 100
p_vec <- sapply(1:trials, function(i){
  x <- stats::rnorm(n); y <- sample(c(0,1), n, replace = T)
  dat <- data.frame(x=x,y=y)
  glm_res <- stats::glm(y~., data = dat)
  p_val <- summary(glm_res)$coefficients[2,4] # if you don't know where this came from, try using " summary(glm_res)$coefficients" first
  p_val
})

# let's first make a histogram
hist(p_vec, breaks = 20, col = "gray", xlab = "P-value bin")

# let's make a QQ plot now
# As a quick review: a QQ plot shows the expected distribution on one axes, and the observed distribution on the other
#  Spend some time to look at the lecture notes, and/or think about what this actually means.
# Below, we make a QQ plot that compares the p_vec to a uniform distribution. This is NOT quite
#  what you need for the homework, but it's extremely close with some minor tweaks, so spend some time to
#  think about what is actually been shown here.
plot(sort(p_vec, decreasing = F), seq(0, 1, length.out = length(p_vec)), asp = T,
     xlab = "Observed distribution", ylab = "Expected distribution")
lines(c(0,1), c(0,1), col = "red", lty = 2, lwd = 2)
# Remember, for your homework, you need to transform the pvalues by -log_10
log_p <- -log(p_vec, base = 10)
plot(p_vec, log_p, pch = 16, xlab = "P-value", ylab = "-Log10 of p-value" )
# Note: in this example, using the qqnorm function won't help.

#################################

# now let's try using PCA
# let's use the Framingham heart data again from last time
dat <- read.csv("https://raw.githubusercontent.com/linnylin92/469_public/master/hw2/framingham.csv")
head(dat)

# let's only do a PCA of the non-categorical variables
dat2 <- dat[,which(colnames(dat) %in% c("TotChol", "Age", "SysBP", "DiaBP", "CigPDay", "BMI", "HeartRate", "Glucose"))]

# we will be using the stats::prcomp function to do principal components
# NOTE: Be sure to look at the ?stats::prcomp to see what the default arguments are
?stats::prcomp
pca_res <- stats::prcomp(dat2, center = T, scale. = T)
names(pca_res)
pca_res$sdev # this vector are the corresponding sqrt-root eigenvalues of the correlation matrix,
##            what you need to make your scree plot
# note: for your homework, you will need to normalize this vector to sum to 1 first!
plot(pca_res) # this is the scree plot plotted, plotting pca_res$sdev^2
pca_res$rotation # this matrix actually contains the eigenvectors of the correlation matrix
pca_res$x # this matrix contains the principal components (starting with the left-most (i.e., first) column
##           representing the first principal component)

plot(pca_res$x[,1], pca_res$x[,2], asp = T, xlab = "Principal component 1", ylab = "Principal component 2", pch = 16,
     col = rgb(0.5, 0.5, 0.5, 0.5))

######

# eigen-decomposition, via eigen function

eigen_res <- eigen(stats::cor(dat2))
names(eigen_res)

# we can check this is indeed an eigen-decomposition
# check 1: eigenvectors is a unitary matrix
tmp <- t(eigen_res$vectors) %*% eigen_res$vectors
tmp[abs(tmp) <= 1e-6] <- 0; tmp
# check 2: we have an exact decomposition
sum(abs(stats::cor(dat2) - eigen_res$vectors %*% diag(eigen_res$values) %*% t(eigen_res$vectors)))

# we can check the eigenvalues are the same
eigen_res$values
pca_res$sdev^2

# we can check that the eigenvectors are the same
eigen_res$vectors[,1]
pca_res$rotation[,1] # same values, up to sign

# we can check the principal components are the same
head(scale(dat2, center = T, scale = T) %*% eigen_res$vectors[,1])
head(pca_res$x[,1]) # same values, up to sign

#########

# svd decomposition, via svd function

svd_res <- svd(dat2)
names(svd_res)

# we can check this is indeed an SVD
# check 1: left singular vectors and right singular vectors are unitary matrices
tmp <- t(svd_res$u) %*% svd_res$u
tmp[abs(tmp) <= 1e-6] <- 0; tmp
tmp <- t(svd_res$v) %*% svd_res$v
tmp[abs(tmp) <= 1e-6] <- 0; tmp
# check 2: we have an exact decomposition
sum(abs(dat2 - svd_res$u %*% diag(svd_res$d) %*% t(svd_res$v)))


# I will let you figure out how to relate this to prcomp and eigen on your own for the homework

################

# NOTE 1: Had we NOT used center = T, scale. = T for stats::prcomp, we would've gotten very different results
pca_res_unscaled <- stats::prcomp(dat2, center = T, scale. = F)
head(pca_res_unscaled$x[,1])

plot(pca_res_unscaled$x[,1], pca_res_unscaled$x[,2], asp = T, xlab = "Principal component (unscaled) 1",
     ylab = "Principal (unscaled) component 2", pch = 16, col = rgb(0.5, 0.5, 0.5, 0.5))

# Note 2: We do NOT use stats::princomp (another PCA function in R) since it crashes when there are more
#  variables than samples
# let's try doing a PCA using only 5 samples. The following line of code will NOT work
res <- stats::princomp(dat2[1:5,])
# However, the next line will work, since we use prcomp instead
res <- stats::prcomp(dat2[1:5,])


################

# note: the eigenvalues of correlation matrices are non-negative, but they CAN be 0.
cor_mat <- matrix(c(1/2, 1/4, 1/4, 1/8), 2, 2)
eigen(cor_mat)
