# Reproducibility supplement for "A Kernel Measure of Dissimilarity between M Distribution"
This repository contains codes to reproduce the results in the paper "A Kernel Measure of Dissimilarity between M Distribution".

Most codes can be run on a local laptop (MacBook Pro with 2.4 GHz Intel Core i5, 16GB memory).
For some of the codes below, we use saved intermediate results (whose computations may take several days on a server) to expedite the reproduction procedure. One can also uncomment the corresponding codes in the `.R` file to reproduce all the intermediate results.


## The power curves

`Power_curves.R` reproduces the power curves (Figure 2 in the main text) of different methods.

One can adjust the number of replications (the variable `replic`) and the number of cores used in parallel computing (the variable `num_cores`). With `replic=100` and `num_cores=7`, it takes around 10 hours to run on a personal laptop, and produces less smooth curves (but the relative performances are still the same).

The R packages needed: `KMD`, `gridExtra`, `nbpMatching`, `energy`, `ggplot2`.

## The Arabic data set
`Arabic_digits.R` reproduces the results concerning the Arabic data set, including Figure 1 in the Introduction. 

The computation for this example may be too large to run on a personal computer. We recommend first computing `pairwise_dist_full.RData` and `feature13.RData` on the server (by uncommenting the related codes in `Arabic_digits.R`), and then the code can be run on a personal laptop.
Before running the code, make sure the files `ArabicDigits.rda`, `pairwise_dist_full.RData`, and `feature13.RData` are placed in the correct directory (and modify the directory in `Arabic_digits.R` accordingly).

The R packages needed: `KMD`, `dtw`, `ggplot2`.

## Simulations on data sets from the UCI Machine Learning Repository
`uci_data.R` reproduces the results in Table 1 concerning data sets from the UCI Machine Learning Repository (see our paper for more details).

Before running the code, make sure the files `Amazon.csv`, `semeion.txt`, `isolet1+2+3+4.txt`, `isolet5.txt`, and `lrs.txt` are placed in the correct directory.

The R package needed: `KMD`.

## Sentiment analysis
`sentiment_analysis.R` analyzes the movie reviews mentioned in the simulation section.

The R packages needed: `KMD`, `stringr`, `text2vec`, `e1071`.

## CLT under the null
`CLT_null.R` checks the validity of Theorem 3 in the main text and reproduces Figure 3 in Appendix D.1.

One can adjust the number of cores used in parallel computing (the variable `num_cores`). With `num_cores=7`, it takes about 20 minutes to run on a personal laptop.

The R package needed: `KMD`.

## Detection threshold
`detection_threshold.R` provides the empirical validation for the detection threshold theorem, and reproduces Figure 4 in Appendix D.2.

Before running the code, make sure the files `Powerd20_1_2.RData`, `Powerd20_2_1.RData`, `Powerd101.RData`, `Powerd102.RData`, `Powerd51.RData`, and `Powerd52.RData` are placed in the correct directory.

The R packages needed: `KMD`, `ggplot2`, `plyr`, `latex2exp`.

## Choice of k for the k-NN graph
`diff_K.R` reproduces the power plot (Figure 5) with different k in Appendix D.3.

Before running the code, make sure the related files are placed in the correct directory. One can also adjust the number of replications `rep` and the number of cores `num_cores` used for parallel computing. With `rep=100` and `num_cores=7`, it takes around 10 hours to run on a local latop and also produces a similar figure.

The R packages needed: `KMD`, `ggplot2`, `MASS`, `ggpubr`.

`bias_mse.R` reproduces the two figures (Figures 6-7) in Appendix D.3 on the bias and MSE of the empirical KMD. With the number of cores `num_cores=7` used in parallel, it takes approximately 1.5 hours to run on a local laptop.

The R packages needed: `KMD`, `ggplot2`, `ggpubr`.

## Validation of the asymptotic distribution-free property
`tilde_g3.R` produces Figure 8 in Appendix D.4 that shows the converges of \tilde{g}_3. `asymp_var.R` reproduces the histograms of normalized empirical KMD in Appendix D.4 (Figure 9).

The R package needed: `KMD`.

## Berkeley growth data
`growth.R` reproduces the results concerning the Berkeley Growth Study.

The R packages needed: `KMD`, `fda`.





## Data dictionary
`ArabicDigits.rda`: The data set is from the R package `mfds` (http://ls.home.amu.edu.pl/mfds.pdf).

`Amazon.csv`: https://archive.ics.uci.edu/ml/datasets/Amazon+Commerce+reviews+set

`semeion.txt`: https://archive.ics.uci.edu/ml/datasets/semeion+handwritten+digit

`isolet1+2+3+4.txt`, `isolet5.txt`: https://archive.ics.uci.edu/ml/datasets/isolet

`lrs.txt`: https://archive.ics.uci.edu/ml/datasets/Low+Resolution+Spectrometer

For other data sets that are called from an R package, please see the corresponding help pages for the data set information.


