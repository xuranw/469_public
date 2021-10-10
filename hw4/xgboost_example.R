rm(list=ls())

# we'll be using the same framingham data that we've always been using for this example
source("https://raw.githubusercontent.com/linnylin92/469_public/master/hw4/hw4_functions.R")
dat_org <- read.csv("https://raw.githubusercontent.com/linnylin92/469_public/master/hw2/framingham.csv")
## same preprocessing as before
dat_org <- dat_org[,-which(colnames(dat_org) == "Educ")]
dat <- dat_org
idx <- which(colnames(dat) == "AnyCHD")
factor_idx <- which(apply(dat, 2, function(x){length(unique(x)) <= 5}))
dat[,-factor_idx] <- as.data.frame(scale(dat[,-factor_idx, drop = F]))
head(dat)
response_idx <- which(colnames(dat) == "AnyCHD")
covariate_idx <- which(colnames(dat) %in% c("TotChol", "Age", "SysBP", "DiaBP", "CigPDay", "BMI", "Glucose", "HeartRate"))
label <- as.numeric(dat[,response_idx])
dat <- as.matrix(dat[,covariate_idx])

dim(dat)

# let's split into training and testing
set.seed(10); n <- length(label)
idx <- sample(1:n, round(.2*n))
train_dat <- dat[-idx,]; train_label <- label[-idx]
test_dat <- dat[idx,]; test_label <- label[idx]

# let's fit a XGBoost model
max_depth = 10
xgb_fit <- xgboost::xgboost(data = train_dat, label = train_label, max.depth = max_depth,
                            nround = 20, objective = "binary:logistic")
## what are fitting? We are fitting a forest of 20 decision trees (given by nround) where each
##  tree's maximum depth is 10. notice we told xgboost to minimize the logistic objective function
## observe that it prints out the training error by default, and it (of course) decreases as the rounds 
##  go on

# let's compute the misclassification error for training data
## to do this, we need to first extract the predictions
train_pred <- as.numeric(stats::predict(xgb_fit, newdata = train_dat) > 0.5)
tab <- table(train_label, train_pred)
1-max(c(sum(diag(tab)), tab[1,2]+tab[2,1]))/sum(tab)

# now let's compute the testing data
test_pred <- as.numeric(stats::predict(xgb_fit, newdata = test_dat) > 0.5)
tab <- table(test_label, test_pred)
1-max(c(sum(diag(tab)), tab[1,2]+tab[2,1]))/sum(tab)
## yikes! quite a bit higher

# the easiest parameter to tune is the number of trees in this forest (i.e., nround)
## towards this end, we will use the xgboost::xgb.cv function, which tunes the number of trees ONLY.
## if you wanted to tune /any/ of the other parameters, you need to either write your own
##   dedicated cross-validation function or use another package (see the challenge question!)
set.seed(10)
xgb_cv <- xgboost::xgb.cv(data = train_dat, label = train_label,
                          nrounds = 20, nfold = 5, metrics = list("error"),
                          max_depth = max_depth, objective = "binary:logistic", early_stopping_rounds = 5)
## note the parameters: nfold (number of folds for cross validation), metrics (what metric
##   to monitor to pick the best number of trees?), early_stoppping_rounds (when to stop if the
##   monitored value stops decreasing on the held-out sample in cross validation?)
## we see that the best iteration is 13
xgb_cv$best_iteration
xgb_fit <- xgboost::xgboost(data = train_dat, label = train_label, max.depth = max_depth,
                            nround = xgb_cv$best_iteration, objective = "binary:logistic")
## using this newly-fitted model (with the appropriate depth), let's measure the training and testing
##  error again
train_pred <- as.numeric(stats::predict(xgb_fit, newdata = train_dat) > 0.5)
tab <- table(train_label, train_pred)
1-max(c(sum(diag(tab)), tab[1,2]+tab[2,1]))/sum(tab)
test_pred <- as.numeric(stats::predict(xgb_fit, newdata = test_dat) > 0.5)
tab <- table(test_label, test_pred)
1-max(c(sum(diag(tab)), tab[1,2]+tab[2,1]))/sum(tab)
## a /bit/ better on test data, but we would need to do more extensive analysis/tuning to really do better.
## for this example, we'll stop here for now, since we're just demo-ing how the code works

# last thing: the feature importances
importance_mat <- xgboost::xgb.importance(model = xgb_fit)
head(importance_mat)
xgboost::xgb.plot.importance(importance_mat)

# I highly encourage you to read the documentation or additional links for XGBoost, as it will
#   reveal more about the nuiances of using XGBoost that this HW4 does not cover

##############################

# let's just observe xgboost outputs further 
## you won't need this for the homework, but it's good to know what is going on under the hood
## let's fit a simpler forest to make this exploration easier
xgb_fit <- xgboost::xgboost(data = train_dat, label = train_label, max.depth = 2,
                            nround = 5, objective = "binary:logistic")
class(xgb_fit)
names(xgb_fit)

xgb_fit$params # the parameters we used to fit
xgb_fit$niter #the number of trees
xgb_fit$evaluation_log # the training error

# how might we look at the /actual/ fitted model?
## it turns out this is not as easy as one might hope... 
xgb_fit$handle 
## this is actually a POINTER to somewhere in your memory that actually stores the fitted model
## in other words, you can't access the fitted model yourself

# Option #1: XGBoost allows you to do a "dump" 
#  (i.e., literally printing out the fitted tree in a pretty user-unfriendly way)
xgboost::xgb.dump(xgb_fit) 
## how the heck do you read this?
## booster[0] denotes this is the first tree we're looking at
## 0 through 6 denotes the different "decisions" we need to make as we traverse the tree
## f1<-0.119 for example, denotes what threshold we are using (i.e., if the value at feature 1
##   is less than 0.119, do something, else do something else)
## yes=1 means: go to line "1:" if yes. similarly, no=2 means: go to line "2:" otherwise
## missing=1 states what happens if there is a missing value
## the leaf values denote the log-odd ratios at the leaves (I think... don't quote me on this.)
xgboost::xgb.dump(xgb_fit, with_stats = TRUE) 
## you can see the cover and gains, what is tracked when we plot the variable importance

# Option #2: plotting the trees
## unfortunately, this requires you to install other packages such as "rlang" and "DiagrammeR",
##   which can be a pain, which is why we don't require you to do this in the homework
xgboost::xgb.plot.tree(model = xgb_fit)
xgboost::xgb.plot.tree(model = xgb_fit, trees = 1) # if you just wanted to print a specific tree, like the first one


# one thing to note is that xgboost is designed so its stored matrices are actually neither
#   matrices nor data.frames. They are data.tables, which is a newer type of data structure
#   added into R post-hoc. 
## read more at https://cran.r-project.org/web/packages/data.table/vignettes/datatable-intro.html
tmp <- xgb_fit$evaluation_log
tmp
class(tmp)
## there's some subtleties with what makes this different from normal, but we'll just highlight some
##   "new" things you can do
tmp[,"train_error"] # the "old" way you're used to, since it's inheriting some data.frame characteristics
tmp[,train_error] # the "new" way, specific to only data.tables. 
## notice that the former way gives you ANOTHER data.table, NOT the vector you would've expected
class(tmp[,"train_error"])
class(tmp[,train_error])
## contrast this is we cast tmp into a data.frame
tmp2 <- as.data.frame(tmp)
tmp2
tmp2[,"train_error"] 
class(tmp2[,"train_error"])
tmp2[,train_error] # this will error, since tmp2 is not a data.table

# remember that xgb_cv we fit above? Same logic applies
xgb_cv$evaluation_log
class(xgb_cv$evaluation_log)
# so if we wanted plot, we just need to remember data.table operations
plot(xgb_cv$evaluation_log[,iter], xgb_cv$evaluation_log[,test_error_mean],
     xlab = "Iteration", ylab = "Mean test errror (from CV)")

##############################

# HW4 code (from HW4 pdf) put here for your convenience
snp_data <- as.matrix(read.csv("https://raw.githubusercontent.com/linnylin92/469_public/master/hw4/synthetic_famuss.csv"))
heart_disease <- snp_data[,1]; snp_data <- snp_data[,-1]

set.seed(10); n <- length(heart_disease)
idx <- sample(1:n, round(.2*n))
train_dat <- snp_data[-idx,]; train_label <- heart_disease[-idx]
test_dat <- snp_data[idx,]; test_label <- heart_disease[idx]

###

source("https://raw.githubusercontent.com/linnylin92/469_public/master/hw4/hw4_functions.R")
dat <- as.matrix(read.csv("https://raw.githubusercontent.com/linnylin92/469_public/master/hw4/synthetic_data.csv"))
y <- dat[,1]; x <- dat[,2:3]

grid_val <- seq(-5, 5, length.out = 100)
test_grid <- as.matrix(expand.grid(grid_val, grid_val))
colnames(test_grid) <- c("x1", "x2")
head(test_grid)

example_classifier <- function(vec){
  ifelse(vec[2] >= 2, 0, 1)
}
pred_vec <- apply(test_grid, 1, example_classifier)

plot_prediction_region(x, y, pred_vec, test_grid, xlab = "Dimension 1",
                       ylab = "Dimension 2", main = "Example decision boundary",
                       pch = 16, asp = T)
