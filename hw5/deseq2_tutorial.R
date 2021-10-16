# WARNING: Through testing with myself and the TA, we have realized that this
#   tutorial (and homework) efffectively require you to have the latest version of R.
sessionInfo()
# This above line should read "R version 3.6.3 (2020-02-29)"
# If it doesn't, it /might/ not matter, as long as you get the following 6 packages to install and
#   you can load in the data

install.packages("pheatmap") # for visualization in Question 1E
install.packages("hexbin") # for visualization in Question 1D

# These are the bioconductor packages. Think of this as a separate R repository (not run by CRAN)
#   that contains packages dedicated to analyzing biological data.
# You can read more about this at: https://www.bioconductor.org/about/
# Each package has its own page. For example, for the DESeq2 package, you can visit
#   the following url: https://www.bioconductor.org/packages/release/bioc/html/DESeq2.html
#   A google search of "bioconductor DESeq2" will point you directly to this page. This page is useful
#   for possible vignettes, the reference manual, how to install the package,
#   verifying you have the latest version of the package, etc.
# Let's try first installing DESeq2
install.packages("BiocManager")
BiocManager::install("DESeq2") # the main package you will be relying on throughout Question 1
BiocManager::install("SummarizedExperiment") # the package that DESeq2 depends on, and not having the latest version of this package will cause erros
BiocManager::install("vsn") # package for Question 1D
BiocManager::install("genefilter") # package for Question 1E

# Now let's load all the packages and the dataset
library(DESeq2); library(hexbin)
library(vsn); library(SummarizedExperiment)
library(genefilter); library(pheatmap)
load(url("https://github.com/linnylin92/469_public/raw/master/hw5/airway.RData"))
dds
# an R object called dds will appear in your workspace, with class DESeqDataSet

# To be clear, importantly, you want to make sure your version of SummarizedExperiment is 1.16.1
# You can verify that this using sessionInfo(), and see that 1.16.1 is the latest version at https://bioconductor.org/packages/release/bioc/html/SummarizedExperiment.html
sessionInfo()
# If you get the error message "Error in if (len > 1) paste(len, " elements, ", sep = "") else "" :  argument is of length zero",
#   it /probably/ means you  do not have the latest version of sessionInfo()
# If this is the case, you have two options: 1) (the painful route) reinstall the latest version of R from the CRAN website
#   for instance, at https://cran.r-project.org/. You want to install R version 3.6.3.
#   Note: This will require you to re-install /all/ the packages you have used before, which can be a headache
#   especially if you use R for other classes. Realistically, it's at most an hour of tedious re-installation for everything (R and packages).
#   2) (the less painful but more tedious route) Email Kevin, and he will give you more specific directions on an alternative Question 1 to do.

########################

# Let's start our investigation of dds. This follows exactly what you'll be doing in Question 1
# Relevant code for Q1.A:
class(dds)
# DESeqDataSet is a "S4" R object. It is /not/ a glorified list, unlike most of the other things you are used to in R
getSlots(class(dds))
dds@assays # This is how you access things in S4 classes, with the "@" sign, as opposed to the "$" sign you are used to
# Essentially, an S4 class is a more formal way to design object-oriented things in R. You can read more about it in http://adv-r.had.co.nz/S4.html
#   We won't focus too much about it in this class
SummarizedExperiment::colData(dds) # Observe that in Bioconductor, the convention is to have the rows represent the genes, and the columns represent the subjects.
#  This is OPPOSITE (or more exactly, the transpose) of what we are used to in statistics.
count_dds <- SummarizedExperiment::assay(dds) # a handy function to extract the count matrix for you
count_dds[1:5, 1:5]
dim(count_dds)
colnames(count_dds)
head(rownames(count_dds))
head(count_dds)

# Relevant code for Q1.B:
dds <- DESeq2::DESeq(dds) # observe that a bunch of preprocessing is being done
res <- DESeq2::results(dds)
res # we can peak at the results stored in res, but we will not look at it too closely for HW5

# Relevant code for Q1.C:
DESeq2::plotMA(res, ylim = c(-5, 5), main = "MA plot") # you will be asked how to interpret this MA plot in HW5

# Relevant code for Q1.D:
vsd <- DESeq2::vst(dds, blind = FALSE)
count_vsd <- SummarizedExperiment::assay(vsd)
p1 <- vsn::meanSdPlot(count_dds, plot = F)$gg + ggplot2::labs(title = "Not variance stabilized")
p2 <- vsn::meanSdPlot(count_vsd, plot = F)$gg + ggplot2::labs(title = "Variance stabilized")
gridExtra::grid.arrange(p1, p2, ncol=2)
# the code provided in the HW5 is simplier than above, but this is the code used to generate the plot in
#   the figure shown in HW5 if you're curious about how I did it. It relies on some basic ggplot manipulation

# Relevant code for Q1.E:
topVarGenes <- head(order(genefilter::rowVars(count_vsd), decreasing = TRUE), 40) # select some genes
dat <- count_vsd[topVarGenes,]
dat <- dat - rowMeans(dat) # remove the mean
anno <- as.data.frame(SummarizedExperiment::colData(vsd)[, c("cell","dex")])
pheatmap::pheatmap(dat, annotation_col = anno) # plot
pheatmap::pheatmap(dat, annotation_col = anno, fontsize_row = 5) # smaller font size for the genes if it bothers you




