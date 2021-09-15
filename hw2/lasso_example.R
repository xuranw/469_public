# load in some functions that might be useful
source("https://raw.githubusercontent.com/linnylin92/469_public/master/hw2/hw2_functions.R")

# load in data
dat_org <- read.csv("https://raw.githubusercontent.com/linnylin92/469_public/master/hw2/framingham.csv")

# you need to install the glmnet package first, via install.packages("glmnet")
library(glmnet)

############

# same preprocessing as before, you don't need to worry about this for your homework
dat_org <- dat_org[,-which(colnames(dat_org) == "Educ")]
dat <- dat_org
idx <- which(colnames(dat) == "AnyCHD")
factor_idx <- which(apply(dat, 2, function(x){length(unique(x)) <= 5}))
dat[,-factor_idx] <- as.data.frame(scale(dat[,-factor_idx, drop = F]))
head(dat)

#############

# fitting Lasso (no cross-validation)
## for kicks, let us try to regress HeartRate onto TotChol, Age, SysBP, DiaBP, CigPDay, BMI and Glucose
response_idx <- which(colnames(dat) == "HeartRate")
covariate_idx <- which(colnames(dat) %in% c("TotChol", "Age", "SysBP", "DiaBP", "CigPDay", "BMI", "Glucose"))
dat2 <- dat[,c(response_idx,covariate_idx)]
## we do not include an intercept here just for demonstration
glmnet_res <- glmnet::glmnet(x = as.matrix(dat2[,-1]), y = dat2[,1], family = "gaussian",
                             intercept = F)
class(glmnet_res)
names(glmnet_res) # a0 contains the intercepts, beta contains the coefficients, lambda contains the tuning parameter
## NOTE 1: by default, the parameter alpha=1, meaning glmnet is fitting the lasso
## be sure to read the documentation for glmnet (i.e., ?glmnet::glmnet)
## NOTE 2: we had to use the "as.matrix" function to convert dat from a data.frame into a matrix
## glmnet will NOT work if you feed in a data.frame into x
## that is, the following line of code will error
glmnet_res <- glmnet::glmnet(x = dat2[,-1], y = dat2[,1], family = "gaussian",
                             intercept = F)

# now that we fitted the lasso, let's take a look at what it is
glmnet_res # not very interpretable...
graphics::plot(glmnet_res) 
## x-axis is the l1 norm of the coefficients (excluding intercept, which we don't have anyways)
## y-axis is the value of the coefficient
## one line per coefficient, so we see the coefficients all start at 0 (on the left, when we
##   penalize a LOT), and end up close to their linear regression counterpart (sometimes
##   not exactly, due to the nature of the glmnet function) on the right, where almost no penalization
##   is done
## see ?glmnet:::plot.glmnet for more documentation on how to change up this plot

# but how do we choose which model to pick specifically? This is where cv.glmnet comes in, for
#   cross-validation
# fitting Lasso (using cross-validation)
set.seed(10)
cvglmnet_res <- glmnet::cv.glmnet(x = as.matrix(dat2[,-1]), y = dat2[,1], family = "gaussian",
                             intercept = F)
class(cvglmnet_res) # it's a different class
names(cvglmnet_res) 
## lambda contains the tuning parameter, cvm contains the mean estimated mse, 
## glmnet.fit contains the fitted glmnet model, lambda.min and lambda.1se are two lambdas chosen by cross-validation
cvglmnet_res # it prints out something different from before
## remember, cross validation is random, so it's in your interest to set the seed beforehand
## by default, 10 folds are used

# let's try to plot cvglmnet_res now
plot(cvglmnet_res) 
## this is the cross validation plot. on the x-axis are different values of the penalty term (in this
##   case, on the log scale), and the y-axis is the estimated MSE. the red dots denote mean estimated MSE
##   averaged over the 10 folds, and the error bars denote the std. deviation of the estimated MSE
## these plots typically have two long vertical dotted lines. the one corresponding to the
##   the smaller value of lambda represents "lambda.min", the lambda that minimized the cross validation
##   metric (i.e., minimum mean estimated mse). The other one represents "lambda.1se", the lambda
##   that is 1 standard error above lambda.min. Both are typically used in practice. Lambda.1se
##   gives a sparser model than lambda.min, at the cost of (potentially) worse predictive power.

# we can make the fitted glmnet plot a bit more meaningful by marking lambda.1se and lambda.min
#  explicitly, with a bit more code. you won't need to do anything like this for your homework
plot(cvglmnet_res$glmnet.fit)
cv_idx <- which(names(cvglmnet_res) %in% c("lambda.min", "lambda.1se"))
for(i in cv_idx){
  lambda_idx <- which(cvglmnet_res$glmnet.fit$lambda == cvglmnet_res[i])
  l1_norm <- sum(abs(cvglmnet_res$glmnet.fit$beta[,lambda_idx]))
  lines(rep(l1_norm, 2), c(-1e3, 1e3), lwd = 2, lty = 2)
}

# note: unlike lm or glm, there is no meaningful summary (so there is no meaningful way to
#   compute standard errors for free)
summary(glmnet_res)
summary(cvglmnet_res)

########################

# let's extract the coefficients from cvglmnet_res. there are many ways to do this
## we will extract based on the lambda.1se
## method 1: what you would usually do, outside of this homework
coef_vec1 <- stats::coef(cvglmnet_res, s = "lambda.1se")
class(coef_vec1)
coef_vec1 # its in sparse matrix representation, so let's convert it to a more useable form
name_vec <- rownames(coef_vec1)
coef_vec1 <- as.numeric(coef_vec1)
names(coef_vec1) <- name_vec
coef_vec1
## observe that unlike glm and lm, the first term returned is ALWAYS the intercept, even though
##   we had asked for no intercept. so let's remove it for this example
coef_vec1 <- coef_vec1[-1]
coef_vec1

## method 2: for now, use the output_coefficients function provided for this homework
coef_vec2 <- output_coefficients(cvglmnet_res, dat2, 1)

sum(abs(coef_vec1 - coef_vec2)) <= 1e-6

## method 3: we can directly extract the coefficients from the cvglmnet_res.
## this is somewhat unadvised unless you know what you're doing, but this is what you would need to do
##   if you (for some reason) wanted the coefficients that are not associated with lambda.min or lambda.1se
lambda_idx <- which(cvglmnet_res$glmnet.fit$lambda == cvglmnet_res$lambda.1se)
coef_vec3 <- cvglmnet_res$glmnet.fit$beta[,lambda_idx]

sum(abs(coef_vec1 - coef_vec3)) <= 1e-6

#############################



# let's now compute the predictions from cvglmnet_res. there are many ways to do this
## method 1: what you would usually do, outside of this homework
pred_vec1 <-  stats::predict(cvglmnet_res, newx = as.matrix(dat2[,-1]), s = "lambda.1se")
## note: this is VERY fincky. you have to convert newx into a matrix, and the matrix has to
##   have exactly the same number of columns as the number of variables within cvglmnet_res
head(pred_vec1)
pred_vec1 <- as.numeric(pred_vec1)

## method 2: for now, you can use the output_predictions function provided for this homework
pred_vec2 <- output_predictions(cvglmnet_res, dat2, 1)
head(pred_vec2)

sum(abs(pred_vec1 - pred_vec2)) <= 1e-6

## method 3: of course, if you're hardcore, you can compute these values yourself
pred_vec3 <- as.numeric(as.matrix(dat2[,-1]) %*% coef_vec1)

sum(abs(pred_vec1 - pred_vec3)) <= 1e-6

##########################

# penalized logistic regression
## this is more-or-less the same, so we briefly do it here. the main difference is the family
## fitting:
idx <- which(colnames(dat) == "AnyCHD")
set.seed(10)
cvglmnet_res <- glmnet::cv.glmnet(x = as.matrix(dat[,-idx]), y = dat[,idx], family = "binomial",
                                  intercept = F)
## plotting:
plot(cvglmnet_res) # the y-axis is a different loss now, called binomial deviance (instead of MSE)
plot(cvglmnet_res$glmnet.fit) # similar to before
## get coefficients (one of 3 ways)
coef_vec <- output_coefficients(cvglmnet_res, dat, idx)
coef_vec
## get predictions (one of 3 ways)
pred_vec <- output_predictions(cvglmnet_res, dat, idx)
head(pred_vec)
## the other ways to extract predictions are slightly more involved. see how output_predictions is implemented

#######

# aside: observe that it /matters/ whether or not you scale the coefficients
#  this is true for both Lasso and penalized logistic regression
## unlike OLS (ordinary least squares, via the lm function), your predictions could be
##   completely different, and you might select completely different variables
## let's use our (unscaled) dat_org dataset as an example

idx <- which(colnames(dat_org) == "AnyCHD")
## we do not include an intercept here just for demonstration
set.seed(10)
cvglmnet_res_alt <- glmnet::cv.glmnet(x = as.matrix(dat_org[,-idx]), y = dat_org[,idx], family = "binomial",
                                      intercept = F)

coef_vec_alt <- output_coefficients(cvglmnet_res_alt, dat_org, idx)
coef_vec_alt
coef_vec

pred_vec_alt <- output_predictions(cvglmnet_res_alt, dat_org, idx)
table(pred_vec_alt, pred_vec)

# just to drive this point home, let's look at the predictions for 
#   logistic regression (no penalization) on both the original data and its scaled counterpart
## we'll need to include an intercept in the model for this demonstration to work
head(dat)
glm_res <- stats::glm(AnyCHD ~ . , data = dat, family = stats::binomial)
pred_vec_glm <- output_predictions(glm_res, dat, idx)

head(dat_org)
glm_res_2 <- stats::glm(AnyCHD ~ . , data = dat_org, family = stats::binomial)
pred_vec_glm_2 <- output_predictions(glm_res, dat, idx)

table(pred_vec_glm, pred_vec_glm_2)

########################3


# for fun, we can make the following plot, similar to what we did at the end of the logisitic example
## main things to note (useful for the homework):
##  - 1) you can set the color to the numerics 1 through 4 for 1=black, 2=red, 3=green, 4=blue
##  - 2) to create a line break in the title (i.e. main), use the "\n" character
pred_vec <- as.numeric(1/(1+exp(-(as.matrix(dat[,-idx])%*%coef_vec))))
ord <- order(pred_vec)
par(mar = c(4,6,4,0.5))
graphics::plot(pred_vec[ord], 
               col = c(1,2)[dat$AnyCHD+1][ord], pch = 16,
               xlab = "Subject index (Ordered)", 
               ylab = "Predicted probability of CHD",
               main = "Predicted probability of subjects for CHD\nPenalized logisitc regression")
graphics::lines(x = c(-1e4,1e4), y = rep(0.5, 2), 
                col = "black", lty = 2, lwd = 3)
legend("topleft", c("Had CHD", "Did not have CHD"), 
       bty="n", fill=c(1, 2))
