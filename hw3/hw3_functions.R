generate_data <- function(n = 500){
  dat <- MASS::mvrnorm(n, mu = c(0,0), Sigma = matrix(c(10,-2,-2,1), 2, 2))
  scale(dat, center = T, scale = T)
}

.l2norm <- function(x){sqrt(sum(x^2))}

generate_random_direction <- function(){
  vec <- stats::rnorm(2)
  vec <- vec/.l2norm(vec)
}
