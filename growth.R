# Berkeley growth data
# The script reproduces the results concerning the Berkeley Growth Study.

library(fda)
attach(growth)
# Order 6 spline basis with knots at ages of observations
# Number of basis functions: 35
# Monotonic smoothing model: y = beta_0 + beta_1 \int exp(w) + epsilon
wbasis = create.bspline.basis(c(1,18), 35, 6, age)
# Roughness penalty on the third derivatives; smoothing parameter 1/sqrt(10) 
growfdPar = fdPar(wbasis, 3, 10^(-0.5))

# Obtain the smoothed functions
growth_female = list()
for (i in 1:54) {
  growth_female[[i]] = smooth.monotone(age, hgtf[,i], growfdPar)
}
growth_male = list()
for (i in 1:39) {
  growth_male[[i]] = smooth.monotone(age, hgtm[,i], growfdPar)
}
growth_all = c(growth_female, growth_male)

# Compute the distance matrix
Dist_mat = matrix(0, nrow = 93, ncol = 93)
for (i in 1:93) {
  for (j in 1:i) {
    if (j < i) {
      # The function to be integrated (squared difference)
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
# Compute the KMD
KMD::KMD(Dist_mat,c(rep(1,54),rep(2,39)),M=2,Knn=1)
# 0.8471035
# Compute the p-value for equal distributions
KMD::KMD_test(Dist_mat,c(rep(1,54),rep(2,39)),M=2,Knn=1)
# 0.001996008

# Only use heights between ages 5 to 12
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
# Compute the KMD
KMD::KMD(Dist_mat2,c(rep(1,54),rep(2,39)),M=2,Knn=1)
# 0.3665717
# Compute the p-value for equal distributions
KMD::KMD_test(Dist_mat2,c(rep(1,54),rep(2,39)),M=2,Knn=1)
# 0.005988024
