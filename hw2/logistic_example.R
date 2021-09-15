# load in some functions that might be useful
source("https://raw.githubusercontent.com/linnylin92/469_public/master/hw2/hw2_functions.R")

# load in data, basic exploration
dat <- read.csv("https://raw.githubusercontent.com/linnylin92/469_public/master/hw2/framingham.csv")
dim(dat)
colnames(dat)
head(dat)
summary(dat)
str(dat)

############

# for datasets like these (unlike SNP data), it's in your best interest to rescale the data
#   you don't need to do this step in your homework
## remove education since it's categorical with more than 2 levels
dat <- dat[,-which(colnames(dat) == "Educ")]
idx <- which(colnames(dat) == "AnyCHD")
factor_idx <- which(apply(dat, 2, function(x){length(unique(x)) <= 5}))
dat[,-factor_idx] <- as.data.frame(scale(dat[,-factor_idx, drop = F]))
head(dat)
summary(dat)

## in general, outside of this course, this preprocessing/cleaning step is typically
##   the one that will give you the most headache. forunately, for homeworks in this course
##   we'll clean the data for you so you can jump (more-or-less) immediately to the "statistics"
##   part of the analyses
## outside of this course, you might find the stats::model.matrix useful

############

# fitting logistic regression
## the "-1" in the formula omits the intercept
glm_res <- stats::glm(AnyCHD ~ . - 1 , data = dat, family = stats::binomial)
class(dat)
glm_res
## note: the following line of code will NOT work
## this demonstrates that the input dataset into glm MUST be a data frame
glm_res <- stats::glm(AnyCHD ~ . - 1 , data = as.matrix(dat), family = stats::binomial)
## note: the "stats::" prefix I put before "glm" is simply for explicitly. This is R-lingo
##   for telling R which package to specifically grab the "glm" function from. In most cases,
##   you can omit "stats::" (or any package name prefix), and nothing will change. However,
##   I will be doing this (whenever I remember) to explicitly remind students which
##   package I'm using, which will become more useful as we start using functions for 
##   many different packages

######

# extract estimated coefficients
## method 1: what you would usually do, outside of this homework
coef_vec1 <- stats::coef(glm_res)
coef_vec1
length(coef_vec1) == (ncol(dat)-1)

## method 2: for now, use the output_coefficients function provided for this homework
coef_vec2 <- output_coefficients(glm_res, dat, idx)

## method 3: as it turns out, the glm_res obj contains the coefficients inside it, so we
##  can just navigate and extract it directly
coef_vec3 <- glm_res$coefficients

## check to see we got the same thing
length(coef_vec1) == length(coef_vec2)
sum(abs(coef_vec1 - coef_vec2)) <= 1e-6

############

# summarize the fit (and see which ones are significant)
summary(glm_res)

############

# form predictions
## method 1: what you would usually do, outside of this homework
## read the documentation for stats::predict.glm for more information
pred_vec1 <-  stats::predict(glm_res, newx = dat, type = "response")
head(pred_vec1) # these are the probabilities to be in one of the classes
pred_vec1 <- as.numeric(pred_vec1 >= 0.5) # converting it to a 0-1
head(pred_vec1) 
## note: if you're familiar with ROC curves, you'll realize that this threshold of 0.5 is
##  yet another parameter you can tune. We might revisit this in Chapter 4 later in the course

## method 2: for now, you can use the output_predictions function provided for this homework
pred_vec2 <- output_predictions(glm_res, dat, idx)

## method 3: if you're really hardcore, you can compute the predictions yourself
## this is primarily useful only as a learning exercise, to make sure you know what is going
##  behind the scenes with the stats::predict function
pred_vec3 <- as.numeric(1/(1+exp(-(as.matrix(dat[,-idx])%*%coef_vec1))))
pred_vec3 <- as.numeric(pred_vec3 >= 0.5) 

## check to see we got the same thing
length(pred_vec1) == length(pred_vec2)
sum(abs(pred_vec1 - pred_vec2)) <= 1e-6
length(pred_vec1) == length(pred_vec3)
sum(abs(pred_vec1 - pred_vec3)) <= 1e-6

############

# after extracting predictions, we can compute the misclassification rate
obs_response <- dat$AnyCHD
tab <- table(pred_vec1, obs_response) 
tab
## the different rows reflect different estimated responses
## the different columns reflect different observed responses

## to compute the misclassification rate, simply sum the diagonal terms, divided by the grand sum
1 - sum(diag(tab))/sum(tab)
## note: in general, the labeling of the estimated responses is arbitrary, so outside of
##   this homework, you need to try permuting the class labels, and seeing which permutation
##   yields a lower misclassification rate
1 - sum(diag(tab[c(2,1),]))/sum(tab)
## for this homework, you won't need to worry about this permutation

############

# we can make some plots to better visualize the results
# plotting the coefficients
## this is beyond the homework, but it's good to see how to make these types of plots
## you might be familiar with ggplot, but in our class, we typically give our examples
##   in base R, since we don't assume familiarity with ggplot or any tidyverse-related package

## let's refit the model with an intercept
glm_res <- stats::glm(AnyCHD ~ . , data = dat, family = stats::binomial)
coef_vec <- stats::coef(glm_res)

## extract standard errors
name_vec <- names(coef_vec)
names(coef_vec) <- NULL
sum_mat <- summary(glm_res)
sd_vec <- sum_mat$coefficients[,2]

orange <- grDevices::rgb(232, 125, 49, max = 255)
blue <- grDevices::rgb(162, 215, 216, max = 255)
res <- graphics::barplot(coef_vec, col = orange, ylim = range(c(coef_vec - sd_vec,coef_vec + sd_vec)),
                         main = "Coefficients for logisitic model")
graphics::abline(0, 0, lwd = 2)
graphics::text(as.numeric(res), min(coef_vec - sd_vec)-0.05, srt = -45, adj = 0,
               xpd = T, labels = name_vec, cex = 1)
graphics::arrows(res, coef_vec + sd_vec, res, coef_vec - sd_vec, angle=90, code=3, 
                 length=0.1)

# plotting the log-odds
## we need to jitter the response (the "0.05*stats::rnorm..." part) so we can see the different values
## notice that this is plotting the logit function
pred_vec <- stats::predict(glm_res, type = "response", newdata = dat)
ord <- order(pred_vec)

par(mar = c(4,6,4,0.5))
graphics::plot(pred_vec[ord] + 0.05*stats::rnorm(1:length(ord)), 
               col = c(orange, blue)[dat$AnyCHD+1][ord], pch = 16,
               xlab = "Subject index (Ordered)", 
               ylab = "Predicted probability of CHD\n(With Jitter)",
               main = "Predicted probability of subjects for CHD")
graphics::lines(x = c(-1e4,1e4), y = rep(0.5, 2), 
                col = "black", lty = 2, lwd = 3)
legend("topleft", c("Had CHD", "Did not have CHD"), 
       bty="n", fill=c(blue, orange))


########################

# additional documentation about this data
#' Framingham study data
#'
#' @name framingham
#' @docType data
#' @format A data frame with 2142 rows and 14 variables
#' \describe{
#'   \item{Sex}{participant sex, 0 for female, 1 for male}
#'   \item{TotChol}{serum total cholesterol, in mg/dL}
#'   \item{Age}{age at exam, in years}
#'   \item{SysBP}{systolic blood pressure, in mmHg}
#'   \item{DiaBP}{diastolic blood pressure, in mmHg}
#'   \item{CurSmoke}{current cigarette smoking at exam, 0 for not currently, 
#'   1 for currently}
#'   \item{CigPDay}{number of cigarettes smoked each day}
#'   \item{BMI}{body mass index, weight in kilograms/height meters squared}
#'   \item{Diabetes}{diabetic according to criteria of first exam treated or 
#'   first exam with casual glucose of 200 mg/dL or more, 0 for not considered
#'   to be diabetic, 1 for considered diabetic}
#'   \item{BPMeds}{use of anti-hypertensive medication at exam,
#'   0 for not currently in use, 1 for currently in use}
#'   \item{HeartRate}{heart rate (ventricular rate), in beats/min}
#'   \item{Glucose}{casual serum glucose, in mg/dL}
#'   \item{Educ}{attained education, 1 for 0 to 11 years of schooling,
#'   2 for high school diploma, 3 for some college, vocational school,
#'   4 for college degree or more}
#'   \item{AnyCHD}{participant developed angina pectoris, 
#'   myocardial infarction (hospitalized and silent or unrecognized), 
#'   coronary insufficiency (unstable angina), or fatal coronary heart disease
#'   in first check-up or in one of the future follow-ups}
#' }
#' @author Kevin Lin \email{kevinl1@andrew.cmu.edu}
#' @source Data released by BioLINCC request for teaching uses. Data 
#' ("Framingham Data") accessed on October 26, 2016. See more 
#' information at \url{https://biolincc.nhlbi.nih.gov/teaching/}. The
#' documentation is taken from \url{https://biolincc.nhlbi.nih.gov/static/studies/teaching/framdoc.pdf}.
#' @keywords data
NULL
