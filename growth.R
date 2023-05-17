library(fda)
attach(growth)
# order 6 spline basis with knots at ages of observations
# number of basis functions 35
# monotonic smoothing model: y = beta_0 + beta_1 \int exp(w) + epsilon
wbasis = create.bspline.basis(c(1,18), 35, 6, age)
# roughness penalty on the third derivatives; smoothing parameter 1/sqrt(10) 
growfdPar = fdPar(wbasis, 3, 10^(-0.5))
growth_female = list()
for (i in 1:54) {
  growth_female[[i]] = smooth.monotone(age, hgtf[,i], growfdPar)
}
growth_male = list()
for (i in 1:39) {
  growth_male[[i]] = smooth.monotone(age, hgtm[,i], growfdPar)
}
growth_all = c(growth_female, growth_male)
Dist_mat = matrix(0, nrow = 93, ncol = 93)
for (i in 1:93) {
  for (j in 1:i) {
    if (j < i) {
      sq_diff = function(x) {
        func_val1 = growth_all[[i]]$beta[1] + growth_all[[i]]$beta[2]*eval.monfd(x,growth_all[[i]]$Wfd)
        func_val2 = growth_all[[j]]$beta[1] + growth_all[[j]]$beta[2]*eval.monfd(x,growth_all[[j]]$Wfd)
        return((func_val1 - func_val2)^2)
      }
      Dist_mat[i,j] = sqrt(integrate(sq_diff, lower = 1, upper = 18)$value)
      Dist_mat[j,i] = Dist_mat[i,j]
    } 
  }
}
set.seed(1)
KMD::KMD(Dist_mat,c(rep(1,54),rep(2,39)),M=2,Knn=1)
# 0.8471035
KMD::KMD_test(Dist_mat,c(rep(1,54),rep(2,39)),M=2,Knn=1)
# 0.001996008

Dist_mat2 = matrix(0, nrow = 93, ncol = 93)
for (i in 1:93) {
  for (j in 1:i) {
    if (j < i) {
      sq_diff = function(x) {
        func_val1 = growth_all[[i]]$beta[1] + growth_all[[i]]$beta[2]*eval.monfd(x,growth_all[[i]]$Wfd)
        func_val2 = growth_all[[j]]$beta[1] + growth_all[[j]]$beta[2]*eval.monfd(x,growth_all[[j]]$Wfd)
        return((func_val1 - func_val2)^2)
      }
      Dist_mat2[i,j] = sqrt(integrate(sq_diff, lower = 5, upper = 12)$value)
      Dist_mat2[j,i] = Dist_mat2[i,j]
    } 
  }
}
set.seed(1)
KMD::KMD(Dist_mat2,c(rep(1,54),rep(2,39)),M=2,Knn=1)
# 0.3665717
KMD::KMD_test(Dist_mat2,c(rep(1,54),rep(2,39)),M=2,Knn=1)
# 0.005988024

# This tests the difference between boys and girls heights in the
# Berkeley growth data.
# First set up a basis system to hold the smooths
knots  <- growth$age
norder <- 6
nbasis <- length(knots) + norder - 2
hgtbasis <- create.bspline.basis(range(knots), nbasis, norder, knots)
# Now smooth with a fourth-derivative penalty and a very small smoothing
# parameter
Lfdobj <- 4
lambda <- 1e-2
growfdPar <- fdPar(hgtbasis, Lfdobj, lambda)
hgtmfd <- smooth.basis(growth$age, growth$hgtm, growfdPar)$fd
hgtffd <- smooth.basis(growth$age, growth$hgtf, growfdPar)$fd
# Call tperm.fd
tres <- tperm.fd(hgtmfd,hgtffd)

indices = (growth$age >= 5) & (growth$age <= 12)
knots  <- growth$age[indices]
norder <- 6
nbasis <- length(knots) + norder - 2
hgtbasis <- create.bspline.basis(range(knots), nbasis, norder, knots)
Lfdobj <- 4
lambda <- 1e-2
growfdPar <- fdPar(hgtbasis, Lfdobj, lambda)
hgtmfd <- smooth.basis(growth$age[indices], growth$hgtm[indices,], growfdPar)$fd
hgtffd <- smooth.basis(growth$age[indices], growth$hgtf[indices,], growfdPar)$fd
tres <- tperm.fd(hgtmfd,hgtffd)


