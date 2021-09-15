# functions for Question 1
output_coefficients <- function(fit_obj, dat, response_idx){
  stopifnot(any(c("glm", "cv.glmnet") %in% class(fit_obj)))
  stopifnot(is.data.frame(dat))
  stopifnot(response_idx %% 1 == 0, response_idx > 0, response_idx <= ncol(dat))
  
  if("glm" %in% class(fit_obj)){
    coef_vec <- coef(fit_obj)
  } else {
    coef_vec <- as.numeric(stats::coef(fit_obj, s = "lambda.1se"))[-1]
    names(coef_vec) <- colnames(dat)[-response_idx]
  }
  
  coef_vec
}

output_predictions <- function(fit_obj, dat, response_idx){
  stopifnot(any(c("glm", "cv.glmnet") %in% class(fit_obj)))
  stopifnot(is.data.frame(dat))
  stopifnot(response_idx %% 1 == 0, response_idx > 0, response_idx <= ncol(dat))
  
  if("glm" %in% class(fit_obj)){
    
    pred_vec <-  stats::predict(fit_obj, newx = dat, type = "response")
    pred_vec <- as.numeric(pred_vec >= 0.5)
      
  } else {
     
    pred_vec <- as.numeric(glmnet:::predict.cv.glmnet(fit_obj, newx = as.matrix(dat[,-response_idx]), 
                                           s = "lambda.1se", type = "class"))
  }
  
  pred_vec
}

########

# functions for Question 2

generate_data <- function(n, d, k = 3, cor_within = 0.5){
  cor_mat <- matrix(0, d, d)
  idx_vec <- round(seq(0, d, length.out = k+1))
  for(i in 1:k){
    cor_mat[(idx_vec[i]+1):(idx_vec[i+1]), (idx_vec[i]+1):(idx_vec[i+1])] <- cor_within
  }
  diag(cor_mat) <- 1
  
  x <- MASS::mvrnorm(n = n, mu = rep(0, d), Sigma = cor_mat)
  coef_truth <- rep(0,d)
  coef_truth[idx_vec[-1]] <- 5
  y <- as.numeric(x %*% coef_truth + stats::rnorm(n))
  
  list(x = x, y = y, coef_truth = coef_truth)
}

# from http://www.stat.cmu.edu/~ryantibs/statcomp/lectures/plotting.html
clockwise90 <- function(a) { t(a[nrow(a):1,]) }

# the "..." notation in the fuction allows you to input your own arguments into the image function
# for example, try plot_covariance(matrix(,5,5), main = "Test")
plot_covariance <- function(dat, ...){
  graphics::image(clockwise90(stats::cor(dat)), asp = T, ...)
}


