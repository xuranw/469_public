generate_data <- function(n){
  stopifnot(n > 0, n %% 1 == 0)
  
  idx_vec <- sample(1:3, n, replace = T)
  sample_vec <- rep(NA, n)

  sample_vec[idx_vec == 1] <- stats::rnorm(length(which(idx_vec == 1)), mean = 10, sd = 1)
  sample_vec[idx_vec == 2] <- stats::rgamma(length(which(idx_vec == 2)), shape = 2, scale = 2)
  sample_vec[idx_vec == 3] <- stats::rchisq(length(which(idx_vec == 3)), df = 3)

  sample_vec
}
