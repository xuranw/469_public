rm(list=ls())
library(dequer)

# generate a sinusodial curve
x_val <- seq(0, 2*pi, length.out = 1000)
y_val <- 2*sin(2*x_val)

# move this to the correct space
x_val <- seq(-5, 5, length.out = 1000)
tmp <- y_val
y_val <- x_val
x_val <- tmp

# rotate this curve
mat <- cbind(x_val, y_val)
rot_mat <- matrix(c(cos(pi/4), sin(pi/4), -sin(pi/4), cos(pi/4)), 2, 2)
mat <- mat %*% rot_mat

# rescale values (since we rotated)
min_range <- min(apply(mat, 2, function(x){diff(range(x))}))
mat <- mat * 10/min_range
mat <- pmin(mat, 5)
mat <- pmax(mat, -5)

# translate these set of points onto the grid of 100 x 100
mat_grid <- matrix(0, 100, 100)
grid_val <- seq(-5,5,length.out=100)
for(i in 1:nrow(mat)){
  x_coord <- mat[i,1]; y_coord <- mat[i,2]
  idx_i <- which.min(abs(x_coord - grid_val))
  idx_j <- which.min(abs(y_coord - grid_val))
  
  mat_grid[100-idx_j+1,idx_i] <- 1
}

# set up mat_sign_grid to tell you which grid spaces belong to which cluster
mat_sign_grid <- matrix(0, ncol = 100, nrow = 100)
q <- dequer::deque()
mat_sign_grid[1,1] <- 1
dequer::push(q, c(1,1))
neigh_func <- function(vec){
  mat <- rbind(vec, c(vec[1]-1, vec[2]), c(vec[1]+1, vec[2]),
               c(vec[1], vec[2]-1), c(vec[1], vec[2]+1))
  mat <- pmin(pmax(mat, 1), 100)
}

# start growing region
while(length(q) > 0){
  # pop
  idx <- dequer::pop(q)
  
  # find neighbors
  neigh_mat <- neigh_func(idx)
  
  # elimate NAs
  bool_vec <- sapply(1:nrow(neigh_mat), function(i){
    ifelse(mat_grid[neigh_mat[i,1], neigh_mat[i,2]] == 1, F, T)
  })
  
  neigh_mat <- neigh_mat[which(bool_vec),]
  if(nrow(neigh_mat) == 0) next()
  
  # update and add to queue
  for(i in 1:nrow(neigh_mat)){
    if(mat_sign_grid[neigh_mat[i,1], neigh_mat[i,2]] == 0){
      mat_sign_grid[neigh_mat[i,1], neigh_mat[i,2]] <- 1
      dequer::push(q, neigh_mat[i,])
    }
  }
}

image(mat_sign_grid[nrow(mat_sign_grid):1,ncol(mat_sign_grid):1], asp = T)
  
# set up sampler to sample from mixture of 3 gaussians
sampler <- function(x){
  idx <- sample(1:3, 1, prob = c(0.6, 0.3, 0.1))
  if(idx == 1){
    MASS::mvrnorm(1, mu = c(-3,3), Sigma = 4*diag(2))
  } else if (idx == 2){
    MASS::mvrnorm(1, mu = c(1,-3), Sigma = matrix(c(3,2,2,2), 2, 2))
  } else {
    MASS::mvrnorm(1, mu = c(4,0), Sigma = matrix(c(1.5,-0.5,-0.5,1.5), 2, 2))
  }
}

# generate data 
n <- 150
set.seed(10)
obs_mat <- t(sapply(1:n, sampler))
idx <- lapply(1:2, function(i){which(abs(obs_mat[,i])>=5)})
obs_mat <- obs_mat[-unique(unlist(idx)),]

# match x's to y's
y_label <- sapply(1:nrow(obs_mat), function(i){
  idx_i <- which.min(abs(obs_mat[i,1] - grid_val))
  idx_j <- which.min(abs(obs_mat[i,2] - grid_val))
  mat_sign_grid[idx_i, idx_j]
})

# add random noise
y_label <- sapply(y_label, function(y){
  samp <- stats::rnbinom(1, 1, 0.1)
  ifelse(samp == 1, -y+1, y)
})

# plot data
plot(obs_mat[,1], obs_mat[,2], asp = T, col = c(1:2)[y_label+1], pch = 16)

dat <- cbind(y_label, obs_mat)
colnames(dat) <- c("y", "x1", "x2")

write.csv(dat, file = "../../public_git/hw4/synthetic_data.csv", row.names = F)
