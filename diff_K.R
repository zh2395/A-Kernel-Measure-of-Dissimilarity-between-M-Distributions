# The script generates the power plot (Figure 5) with different k in Appendix D.3.

# k: number of nearest neighbors
# Consider total number of observations n = 150 or 300, and different dimension d
# Consider three cases: location differences, scale differences, and a t-distribution example
# (Only for the scale problem, consider another kernel K2)

# Number of replications to compute the empirical power
rep = 1000
num_cores = 24
# Different k to be considered
ks = c(1,5,10,15,20,30,40,50,75,100,125,150,175,200)
# Dimension
dims = 2^(1:6)


# Computation: a few hours on server (24 cores in parallel)
# We use saved results; one may un-comment the codes below to reproduce them

load("/Users/huangzhen/Downloads/power_n150_location.Rdata")
load("/Users/huangzhen/Downloads/power_n150_location5.Rdata")
load("/Users/huangzhen/Downloads/power_n300_location.Rdata")
load("/Users/huangzhen/Downloads/power_n300_location5.Rdata")
load("/Users/huangzhen/Downloads/power_n150_scale.Rdata")
load("/Users/huangzhen/Downloads/power_n150_scale5.Rdata")
load("/Users/huangzhen/Downloads/power_n150_scaleK2.Rdata")
load("/Users/huangzhen/Downloads/power_n150_scaleK25.Rdata")
load("/Users/huangzhen/Downloads/power_n300_scale.Rdata")
load("/Users/huangzhen/Downloads/power_n300_scale5.Rdata")
load("/Users/huangzhen/Downloads/power_n300_scaleK2.Rdata")
load("/Users/huangzhen/Downloads/power_n300_scaleK25.Rdata")
load("/Users/huangzhen/Downloads/power_n150_t.Rdata")
load("/Users/huangzhen/Downloads/power_n150_t5.Rdata")
load("/Users/huangzhen/Downloads/power_n300_t.Rdata")
load("/Users/huangzhen/Downloads/power_n300_t5.Rdata")

# # 4-5 hours (24 cores in parallel)
# power_n300_location = matrix(NA, nrow = length(dims), ncol = length(ks))
# for (i in 1:length(dims)) {
#  for (j in 1:length(ks)) {
#    getPval = function(id) {
#      set.seed(id)
#      ni = 100
#      X1 = MASS::mvrnorm(ni,rep(0,dims[i]),diag(dims[i]))
#      X2 = MASS::mvrnorm(ni,rep(0.1,dims[i]),diag(dims[i]))
#      X3 = MASS::mvrnorm(ni,rep(0.2,dims[i]),diag(dims[i]))
#      Y = c(rep(1,ni),rep(2,ni),rep(3,ni))
#      Pvals = KMD::KMD_test(rbind(X1,X2,X3), Y, M = 3, Knn = ks[j])
#      return(Pvals)
#    }
#    power_n300_location[i,j] = mean(unlist(parallel::mclapply(seq(1,rep), getPval, mc.cores = num_cores)) <= 0.05)
#  }
# }
# 
# 
# ks = c(1,5,10,15,20,30,40,50,75,100,125)
# power_n150_location = matrix(NA, nrow = length(dims), ncol = length(ks))
# for (i in 1:length(dims)) {
#  for (j in 1:length(ks)) {
#    getPval = function(id) {
#      set.seed(id)
#      ni = 50
#      X1 = MASS::mvrnorm(ni,rep(0,dims[i]),diag(dims[i]))
#      X2 = MASS::mvrnorm(ni,rep(0.1,dims[i]),diag(dims[i]))
#      X3 = MASS::mvrnorm(ni,rep(0.2,dims[i]),diag(dims[i]))
#      Y = c(rep(1,ni),rep(2,ni),rep(3,ni))
#      Pvals = KMD::KMD_test(rbind(X1,X2,X3), Y, M = 3, Knn = ks[j])
#      return(Pvals)
#    }
#    power_n150_location[i,j] = mean(unlist(parallel::mclapply(seq(1,rep), getPval, mc.cores = num_cores)) <= 0.05)
#  }
# }
# 
# ks = c(1,5,10,15,20,30,40,50,75,100,125,150,175,200)
# power_n300_location5 = matrix(NA, nrow = length(dims), ncol = length(ks))
# 
# for (i in 1:length(dims)) {
#  for (j in 1:length(ks)) {
#    getPval = function(id) {
#      set.seed(id)
#      ni = 60
#      X1 = MASS::mvrnorm(ni,rep(0,dims[i]),diag(dims[i]))
#      X2 = MASS::mvrnorm(ni,rep(0.05,dims[i]),diag(dims[i]))
#      X3 = MASS::mvrnorm(ni,rep(0.1,dims[i]),diag(dims[i]))
#      X4 = MASS::mvrnorm(ni,rep(0.15,dims[i]),diag(dims[i]))
#      X5 = MASS::mvrnorm(ni,rep(0.2,dims[i]),diag(dims[i]))
#      Y = c(rep(1,ni),rep(2,ni),rep(3,ni),rep(4,ni),rep(5,ni))
#      Pvals = KMD::KMD_test(rbind(X1,X2,X3,X4,X5), Y, M = 5, Knn = ks[j])
#      return(Pvals)
#    }
#    power_n300_location5[i,j] = mean(unlist(parallel::mclapply(seq(1,rep), getPval, mc.cores = num_cores)) <= 0.05)
#  }
# }
# 
# ks = c(1,5,10,15,20,30,40,50,75,100,125)
# power_n150_location5 = matrix(NA, nrow = length(dims), ncol = length(ks))
# 
# for (i in 1:length(dims)) {
#  for (j in 1:length(ks)) {
#    getPval = function(id) {
#      set.seed(id)
#      ni = 30
#      X1 = MASS::mvrnorm(ni,rep(0,dims[i]),diag(dims[i]))
#      X2 = MASS::mvrnorm(ni,rep(0.05,dims[i]),diag(dims[i]))
#      X3 = MASS::mvrnorm(ni,rep(0.1,dims[i]),diag(dims[i]))
#      X4 = MASS::mvrnorm(ni,rep(0.15,dims[i]),diag(dims[i]))
#      X5 = MASS::mvrnorm(ni,rep(0.2,dims[i]),diag(dims[i]))
#      Y = c(rep(1,ni),rep(2,ni),rep(3,ni),rep(4,ni),rep(5,ni))
#      Pvals = KMD::KMD_test(rbind(X1,X2,X3,X4,X5), Y, M = 5, Knn = ks[j])
#      return(Pvals)
#    }
#    power_n150_location5[i,j] = mean(unlist(parallel::mclapply(seq(1,rep), getPval, mc.cores = num_cores)) <= 0.05)
#  }
# }
# 
# ks = c(1,5,10,15,20,30,40,50,75,100,125)
# power_n150_scale = matrix(NA, nrow = length(dims), ncol = length(ks))
# 
# for (i in 1:length(dims)) {
#  for (j in 1:length(ks)) {
#    getPval = function(id) {
#      set.seed(id)
#      ni = 50
#      X1 = MASS::mvrnorm(ni,rep(0,dims[i]),diag(dims[i]))
#      X2 = MASS::mvrnorm(ni,rep(0,dims[i]),diag(rep(1.5,dims[i])))
#      X3 = MASS::mvrnorm(ni,rep(0,dims[i]),diag(rep(2,dims[i])))
#      Y = c(rep(1,ni),rep(2,ni),rep(3,ni))
#      Pvals = KMD::KMD_test(rbind(X1,X2,X3), Y, M = 3, Knn = ks[j])
#      return(Pvals)
#    }
#    power_n150_scale[i,j] = mean(unlist(parallel::mclapply(seq(1,rep), getPval, mc.cores = num_cores)) <= 0.05)
#  }
# }
# 
# 
# ks = c(1,5,10,15,20,30,40,50,75,100,125)
# power_n150_scaleK2 = matrix(NA, nrow = length(dims), ncol = length(ks))
# 
# for (i in 1:length(dims)) {
#  for (j in 1:length(ks)) {
#    getPval = function(id) {
#      set.seed(id)
#      ni = 50
#      X1 = MASS::mvrnorm(ni,rep(0,dims[i]),diag(dims[i]))
#      X2 = MASS::mvrnorm(ni,rep(0,dims[i]),diag(rep(1.5,dims[i])))
#      X3 = MASS::mvrnorm(ni,rep(0,dims[i]),diag(rep(2,dims[i])))
#      Y = c(rep(1,ni),rep(2,ni),rep(3,ni))
#      Pvals = KMD::KMD_test(rbind(X1,X2,X3), Y, M = 3, Knn = ks[j], Kernel = diag(c(10,1,1)))
#      return(Pvals)
#    }
#    power_n150_scaleK2[i,j] = mean(unlist(parallel::mclapply(seq(1,rep), getPval, mc.cores = num_cores)) <= 0.05)
#  }
# }
# 
# 
# ks = c(1,5,10,15,20,30,40,50,75,100,125,150,175,200)
# power_n300_scale = matrix(NA, nrow = length(dims), ncol = length(ks))
# 
# for (i in 1:length(dims)) {
#  for (j in 1:length(ks)) {
#    getPval = function(id) {
#      set.seed(id)
#      ni = 100
#      X1 = MASS::mvrnorm(ni,rep(0,dims[i]),diag(dims[i]))
#      X2 = MASS::mvrnorm(ni,rep(0,dims[i]),diag(rep(1.5,dims[i])))
#      X3 = MASS::mvrnorm(ni,rep(0,dims[i]),diag(rep(2,dims[i])))
#      Y = c(rep(1,ni),rep(2,ni),rep(3,ni))
#      Pvals = KMD::KMD_test(rbind(X1,X2,X3), Y, M = 3, Knn = ks[j])
#      return(Pvals)
#    }
#    power_n300_scale[i,j] = mean(unlist(parallel::mclapply(seq(1,rep), getPval, mc.cores = num_cores)) <= 0.05)
#  }
# }
# 
# 
# ks = c(1,5,10,15,20,30,40,50,75,100,125,150,175,200)
# power_n300_scaleK2 = matrix(NA, nrow = length(dims), ncol = length(ks))
# 
# for (i in 1:length(dims)) {
#  for (j in 1:length(ks)) {
#    getPval = function(id) {
#      set.seed(id)
#      ni = 100
#      X1 = MASS::mvrnorm(ni,rep(0,dims[i]),diag(dims[i]))
#      X2 = MASS::mvrnorm(ni,rep(0,dims[i]),diag(rep(1.5,dims[i])))
#      X3 = MASS::mvrnorm(ni,rep(0,dims[i]),diag(rep(2,dims[i])))
#      Y = c(rep(1,ni),rep(2,ni),rep(3,ni))
#      Pvals = KMD::KMD_test(rbind(X1,X2,X3), Y, M = 3, Knn = ks[j], Kernel = diag(c(10,1,1)))
#      return(Pvals)
#    }
#    power_n300_scaleK2[i,j] = mean(unlist(parallel::mclapply(seq(1,rep), getPval, mc.cores = num_cores)) <= 0.05)
#  }
# }
# 
# 
# delta = c(0,0.1,0.2,0.3,0.4)
# ks = c(1,5,10,15,20,30,40,50,75,100,125)
# power_n150_t = matrix(NA, nrow = length(delta), ncol = length(ks))
# 
# for (i in 1:length(dims)) {
#  for (j in 1:length(ks)) {
#    getPval = function(id) {
#      set.seed(id)
#      ni = 50
#      X1 = matrix(rt(ni*16,1,delta[i]),ncol = 16)
#      X2 = matrix(rt(ni*16,1),ncol = 16)
#      X3 = matrix(rt(ni*16,1),ncol = 16)
#      Y = c(rep(1,ni),rep(2,ni),rep(3,ni))
#      Pvals = KMD::KMD_test(rbind(X1,X2,X3), Y, M = 3, Knn = ks[j])
#      return(Pvals)
#    }
#    power_n150_t[i,j] = mean(unlist(parallel::mclapply(seq(1,rep), getPval, mc.cores = num_cores)) <= 0.05)
#  }
# }
# 
# 
# ks = c(1,5,10,15,20,30,40,50,75,100,125,150,175,200)
# power_n300_t = matrix(NA, nrow = length(delta), ncol = length(ks))
# 
# for (i in 1:length(dims)) {
#  for (j in 1:length(ks)) {
#    getPval = function(id) {
#      set.seed(id)
#      ni = 100
#      X1 = matrix(rt(ni*16,1,delta[i]),ncol = 16)
#      X2 = matrix(rt(ni*16,1),ncol = 16)
#      X3 = matrix(rt(ni*16,1),ncol = 16)
#      Y = c(rep(1,ni),rep(2,ni),rep(3,ni))
#      Pvals = KMD::KMD_test(rbind(X1,X2,X3), Y, M = 3, Knn = ks[j])
#      return(Pvals)
#    }
#    power_n300_t[i,j] = mean(unlist(parallel::mclapply(seq(1,rep), getPval, mc.cores = num_cores)) <= 0.05)
#  }
# }
# 
# 
# ks = c(1,5,10,15,20,30,40,50,75,100,125)
# power_n150_scale5 = matrix(NA, nrow = length(dims), ncol = length(ks))
# 
# for (i in 1:length(dims)) {
#  for (j in 1:length(ks)) {
#    getPval = function(id) {
#      set.seed(id)
#      ni = 30
#      X1 = MASS::mvrnorm(ni,rep(0,dims[i]),diag(dims[i]))
#      X2 = MASS::mvrnorm(ni,rep(0,dims[i]),diag(rep(1.25,dims[i])))
#      X3 = MASS::mvrnorm(ni,rep(0,dims[i]),diag(rep(1.5,dims[i])))
#      X4 = MASS::mvrnorm(ni,rep(0,dims[i]),diag(rep(1.75,dims[i])))
#      X5 = MASS::mvrnorm(ni,rep(0,dims[i]),diag(rep(2,dims[i])))
#      Y = c(rep(1,ni),rep(2,ni),rep(3,ni),rep(4,ni),rep(5,ni))
#      Pvals = KMD::KMD_test(rbind(X1,X2,X3,X4,X5), Y, M = 5, Knn = ks[j])
#      return(Pvals)
#    }
#    power_n150_scale5[i,j] = mean(unlist(parallel::mclapply(seq(1,rep), getPval, mc.cores = num_cores)) <= 0.05)
#  }
# }
# 
# 
# ks = c(1,5,10,15,20,30,40,50,75,100,125)
# power_n150_scaleK25 = matrix(NA, nrow = length(dims), ncol = length(ks))
# 
# for (i in 1:length(dims)) {
#  for (j in 1:length(ks)) {
#    getPval = function(id) {
#      set.seed(id)
#      ni = 30
#      X1 = MASS::mvrnorm(ni,rep(0,dims[i]),diag(dims[i]))
#      X2 = MASS::mvrnorm(ni,rep(0,dims[i]),diag(rep(1.25,dims[i])))
#      X3 = MASS::mvrnorm(ni,rep(0,dims[i]),diag(rep(1.5,dims[i])))
#      X4 = MASS::mvrnorm(ni,rep(0,dims[i]),diag(rep(1.75,dims[i])))
#      X5 = MASS::mvrnorm(ni,rep(0,dims[i]),diag(rep(2,dims[i])))
#      Y = c(rep(1,ni),rep(2,ni),rep(3,ni),rep(4,ni),rep(5,ni))
#      Pvals = KMD::KMD_test(rbind(X1,X2,X3,X4,X5), Y, M = 5, Knn = ks[j], Kernel = diag(c(10,1,1,1,1)))
#      return(Pvals)
#    }
#    power_n150_scaleK25[i,j] = mean(unlist(parallel::mclapply(seq(1,rep), getPval, mc.cores = num_cores)) <= 0.05)
#  }
# }
# 
# 
# ks = c(1,5,10,15,20,30,40,50,75,100,125,150,175,200)
# power_n300_scale5 = matrix(NA, nrow = length(dims), ncol = length(ks))
# 
# for (i in 1:length(dims)) {
#  for (j in 1:length(ks)) {
#    getPval = function(id) {
#      set.seed(id)
#      ni = 60
#      X1 = MASS::mvrnorm(ni,rep(0,dims[i]),diag(dims[i]))
#      X2 = MASS::mvrnorm(ni,rep(0,dims[i]),diag(rep(1.25,dims[i])))
#      X3 = MASS::mvrnorm(ni,rep(0,dims[i]),diag(rep(1.5,dims[i])))
#      X4 = MASS::mvrnorm(ni,rep(0,dims[i]),diag(rep(1.75,dims[i])))
#      X5 = MASS::mvrnorm(ni,rep(0,dims[i]),diag(rep(2,dims[i])))
#      Y = c(rep(1,ni),rep(2,ni),rep(3,ni),rep(4,ni),rep(5,ni))
#      Pvals = KMD::KMD_test(rbind(X1,X2,X3,X4,X5), Y, M = 5, Knn = ks[j])
#      return(Pvals)
#    }
#    power_n300_scale5[i,j] = mean(unlist(parallel::mclapply(seq(1,rep), getPval, mc.cores = num_cores)) <= 0.05)
#  }
# }
# 
# 
# power_n300_scaleK25 = matrix(NA, nrow = length(dims), ncol = length(ks))
# 
# for (i in 1:length(dims)) {
#  for (j in 1:length(ks)) {
#    getPval = function(id) {
#      set.seed(id)
#      ni = 60
#      X1 = MASS::mvrnorm(ni,rep(0,dims[i]),diag(dims[i]))
#      X2 = MASS::mvrnorm(ni,rep(0,dims[i]),diag(rep(1.25,dims[i])))
#      X3 = MASS::mvrnorm(ni,rep(0,dims[i]),diag(rep(1.5,dims[i])))
#      X4 = MASS::mvrnorm(ni,rep(0,dims[i]),diag(rep(1.75,dims[i])))
#      X5 = MASS::mvrnorm(ni,rep(0,dims[i]),diag(rep(2,dims[i])))
#      Y = c(rep(1,ni),rep(2,ni),rep(3,ni),rep(4,ni),rep(5,ni))
#      Pvals = KMD::KMD_test(rbind(X1,X2,X3,X4,X5), Y, M = 5, Knn = ks[j], Kernel = diag(c(10,1,1,1,1)))
#      return(Pvals)
#    }
#    power_n300_scaleK25[i,j] = mean(unlist(parallel::mclapply(seq(1,rep), getPval, mc.cores = num_cores)) <= 0.05)
#  }
# }
# 
# 
# ks = c(1,5,10,15,20,30,40,50,75,100,125)
# power_n150_t5 = matrix(NA, nrow = length(delta), ncol = length(ks))
# 
# for (i in 1:length(dims)) {
#  for (j in 1:length(ks)) {
#    getPval = function(id) {
#      set.seed(id)
#      ni = 30
#      X1 = matrix(rt(ni*16,1,delta[i]),ncol = 16)
#      X2 = matrix(rt(ni*16,1),ncol = 16)
#      X3 = matrix(rt(ni*16,1),ncol = 16)
#      X4 = matrix(rt(ni*16,1),ncol = 16)
#      X5 = matrix(rt(ni*16,1),ncol = 16)
#      Y = c(rep(1,ni),rep(2,ni),rep(3,ni),rep(4,ni),rep(5,ni))
#      Pvals = KMD::KMD_test(rbind(X1,X2,X3,X4,X5), Y, M = 5, Knn = ks[j])
#      return(Pvals)
#    }
#    power_n150_t5[i,j] = mean(unlist(parallel::mclapply(seq(1,rep), getPval, mc.cores = num_cores)) <= 0.05)
#  }
# }
# 
# 
# ks = c(1,5,10,15,20,30,40,50,75,100,125,150,175,200)
# power_n300_t5 = matrix(NA, nrow = length(delta), ncol = length(ks))
# 
# for (i in 1:length(dims)) {
#  for (j in 1:length(ks)) {
#    getPval = function(id) {
#      set.seed(id)
#      ni = 60
#      X1 = matrix(rt(ni*16,1,delta[i]),ncol = 16)
#      X2 = matrix(rt(ni*16,1),ncol = 16)
#      X3 = matrix(rt(ni*16,1),ncol = 16)
#      X4 = matrix(rt(ni*16,1),ncol = 16)
#      X5 = matrix(rt(ni*16,1),ncol = 16)
#      Y = c(rep(1,ni),rep(2,ni),rep(3,ni),rep(4,ni),rep(5,ni))
#      Pvals = KMD::KMD_test(rbind(X1,X2,X3,X4,X5), Y, M = 5, Knn = ks[j])
#      return(Pvals)
#    }
#    power_n300_t5[i,j] = mean(unlist(parallel::mclapply(seq(1,rep), getPval, mc.cores = num_cores)) <= 0.05)
#  }
# }

# Generate different sub-plots of the figure
library(ggplot2)
# A color blind friendly palette
cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#0072B2", "#F0E442", "#D55E00", "#CC79A7")
scale_n150_M3 = data.frame(k = rep(c(1,5,10,15,20,30,40,50,75,100,125),6),
                           setting = c(rep("d=2",11),rep("d=2(K2)",11),rep("d=4",11),rep("d=4(K2)",11),rep("d=8",11),rep("d=8(K2)",11)),
                           Power = c(power_n150_scale[1,],power_n150_scaleK2[1,],power_n150_scale[2,],power_n150_scaleK2[2,],power_n150_scale[3,],power_n150_scaleK2[3,]))
p_scale_n150_M3 = ggplot(scale_n150_M3, aes(x = k, y = Power)) +
  geom_line(aes(color = setting, linetype = setting)) +
  scale_colour_manual(values=cbp1) +
  ylab(expression(atop("Scale","Power"))) +
  geom_point(aes(color = setting,shape=setting)) +
  theme(legend.position = "none")+
  ylim(c(0,1))

scale_n150_M5 = data.frame(k = rep(c(1,5,10,15,20,30,40,50,75,100,125),6),
                           setting = c(rep("d=2",11),rep("d=2(K2)",11),rep("d=4",11),rep("d=4(K2)",11),rep("d=8",11),rep("d=8(K2)",11)),
                           Power = c(power_n150_scale5[1,],power_n150_scaleK25[1,],power_n150_scale5[2,],power_n150_scaleK25[2,],power_n150_scale5[3,],power_n150_scaleK25[3,]))
p_scale_n150_M5 = ggplot(scale_n150_M5, aes(x = k, y = Power)) +
  geom_line(aes(color = setting, linetype = setting)) +
  scale_colour_manual(values=cbp1) +
  geom_point(aes(color = setting,shape=setting)) + 
  theme(legend.position = "none")+
  ylim(c(0,1))

scale_n300_M3 = data.frame(k = rep(c(1,5,10,15,20,30,40,50,75,100,125,150,175,200),6),
                           setting = c(rep("d=2",14),rep("d=2(K2)",14),rep("d=4",14),rep("d=4(K2)",14),rep("d=8",14),rep("d=8(K2)",14)),
                           Power = c(power_n300_scale[1,],power_n300_scaleK2[1,],power_n300_scale[2,],power_n300_scaleK2[2,],power_n300_scale[3,],power_n300_scaleK2[3,]))
p_scale_n300_M3 = ggplot(scale_n300_M3, aes(x = k, y = Power)) +
  geom_line(aes(color = setting, linetype = setting)) +
  scale_colour_manual(values=cbp1) +
  geom_point(aes(color = setting,shape=setting)) + 
  theme(legend.position = "none")+
  ylim(c(0,1))

scale_n300_M5 = data.frame(k = rep(c(1,5,10,15,20,30,40,50,75,100,125,150,175,200),6),
                           d = c(rep("2",14),rep("2(K2)",14),rep("4",14),rep("4(K2)",14),rep("8",14),rep("8(K2)",14)),
                           Power = c(power_n300_scale5[1,],power_n300_scaleK25[1,],power_n300_scale5[2,],power_n300_scaleK25[2,],power_n300_scale5[3,],power_n300_scaleK25[3,]))
p_scale_n300_M5 = ggplot(scale_n300_M5, aes(x = k, y = Power)) +
  geom_line(aes(color = d, linetype = d)) +
  scale_colour_manual(values=cbp1) +
  geom_point(aes(color = d,shape=d))+
  ylim(c(0,1)) + 
  theme(legend.box.margin = margin(0, -8, 0, 0))




location_n150_M3 = data.frame(k = rep(c(1,5,10,15,20,30,40,50,75,100,125),6),
                              d = c(rep("2^1",11),rep("2^2",11),rep("2^3",11),rep("2^4",11),rep("2^5",11),rep("2^6",11)),
                              Power = c(power_n150_location[1,],power_n150_location[2,],power_n150_location[3,],power_n150_location[4,],power_n150_location[5,],power_n150_location[6,]))
p_location_n150_M3 = ggplot(location_n150_M3, aes(x = k, y = Power)) +
  geom_line(aes(color = d, linetype = d)) +
  scale_colour_manual(values=cbp1) +
  geom_point(aes(color = d,shape = d)) +
  ylab(expression(atop("Location","Power"))) +
  theme(legend.position = "none",plot.title = element_text(hjust = 0.5)) +
  ggtitle("n = 150, M = 3") +
  ylim(c(0,1))

location_n150_M5 = data.frame(k = rep(c(1,5,10,15,20,30,40,50,75,100,125),6),
                              d = c(rep("2^1",11),rep("2^2",11),rep("2^3",11),rep("2^4",11),rep("2^5",11),rep("2^6",11)),
                              Power = c(power_n150_location5[1,],power_n150_location5[2,],power_n150_location5[3,],power_n150_location5[4,],power_n150_location5[5,],power_n150_location5[6,]))
p_location_n150_M5 = ggplot(location_n150_M5, aes(x = k, y = Power)) +
  geom_line(aes(color = d, linetype = d)) +
  scale_colour_manual(values=cbp1) +
  geom_point(aes(color = d,shape=d)) + 
  theme(legend.position = "none",plot.title = element_text(hjust = 0.5)) +
  ggtitle("n = 150, M = 5") +
  ylim(c(0,1))

location_n300_M3 = data.frame(k = rep(c(1,5,10,15,20,30,40,50,75,100,125,150,175,200),6),
                              d = c(rep("2^1",14),rep("2^2",14),rep("2^3",14),rep("2^4",14),rep("2^5",14),rep("2^6",14)),
                              Power = c(power_n300_location[1,],power_n300_location[2,],power_n300_location[3,],power_n300_location[4,],power_n300_location[5,],power_n300_location[6,]))
p_location_n300_M3 = ggplot(location_n300_M3, aes(x = k, y = Power)) +
  geom_line(aes(color = d, linetype = d)) +
  scale_colour_manual(values=cbp1) +
  geom_point(aes(color = d,shape=d)) + 
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  ggtitle("n = 300, M = 3") +
  ylim(c(0,1))

location_n300_M5 = data.frame(k = rep(c(1,5,10,15,20,30,40,50,75,100,125,150,175,200),6),
                              d = c(rep("2^1",14),rep("2^2",14),rep("2^3",14),rep("2^4",14),rep("2^5",14),rep("2^6",14)),
                              Power = c(power_n300_location5[1,],power_n300_location5[2,],power_n300_location5[3,],power_n300_location5[4,],power_n300_location5[5,],power_n300_location5[6,]))
p_location_n300_M5 = ggplot(location_n300_M5, aes(x = k, y = Power)) +
  geom_line(aes(color = d, linetype = d)) +
  scale_colour_manual(values=cbp1) +
  geom_point(aes(color = d,shape=d)) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("n = 300, M = 5") +
  ylim(c(0,1))


t_n150_M3 = data.frame(k = rep(c(1,5,10,15,20,30,40,50,75,100,125),5),
                       delta = c(rep("0",11),rep("0.1",11),rep("0.2",11),rep("0.3",11),rep("0.4",11)),
                       Power = c(power_n150_t[1,],power_n150_t[2,],power_n150_t[3,],power_n150_t[4,],power_n150_t[5,]))
p_t_n150_M3 = ggplot(t_n150_M3, aes(x = k, y = Power)) +
  geom_line(aes(color = delta, linetype = delta)) +
  scale_colour_manual(values=cbp1) +
  geom_point(aes(color = delta,shape = delta)) + 
  theme(legend.position = "none")+
  ylab(expression(atop("t-distribution","Power"))) +
  ylim(c(0,1))

t_n150_M5 = data.frame(k = rep(c(1,5,10,15,20,30,40,50,75,100,125),5),
                       delta = c(rep("0",11),rep("0.1",11),rep("0.2",11),rep("0.3",11),rep("0.4",11)),
                       Power = c(power_n150_t5[1,],power_n150_t5[2,],power_n150_t5[3,],power_n150_t5[4,],power_n150_t5[5,]))
p_t_n150_M5 = ggplot(t_n150_M5, aes(x = k, y = Power)) +
  geom_line(aes(color = delta, linetype = delta)) +
  scale_colour_manual(values=cbp1) +
  geom_point(aes(color = delta,shape=delta)) + 
  theme(legend.position = "none")+
  ylim(c(0,1))

t_n300_M3 = data.frame(k = rep(c(1,5,10,15,20,30,40,50,75,100,125,150,175,200),5),
                       delta = c(rep("0",14),rep("0.1",14),rep("0.2",14),rep("0.3",14),rep("0.4",14)),
                       Power = c(power_n300_t[1,],power_n300_t[2,],power_n300_t[3,],power_n300_t[4,],power_n300_t[5,]))
p_t_n300_M3 = ggplot(t_n300_M3, aes(x = k, y = Power)) +
  geom_line(aes(color = delta, linetype = delta)) +
  scale_colour_manual(values=cbp1) +
  geom_point(aes(color = delta,shape=delta)) + 
  theme(legend.position = "none")+
  ylim(c(0,1))

t_n300_M5 = data.frame(k = rep(c(1,5,10,15,20,30,40,50,75,100,125,150,175,200),5),
                       delta = c(rep("0",14),rep("0.1",14),rep("0.2",14),rep("0.3",14),rep("0.4",14)),
                       Power = c(power_n300_t5[1,],power_n300_t5[2,],power_n300_t5[3,],power_n300_t5[4,],power_n300_t5[5,]))
p_t_n300_M5 = ggplot(t_n300_M5, aes(x = k, y = Power)) +
  geom_line(aes(color = delta, linetype = delta)) +
  scale_colour_manual(values=cbp1) +
  geom_point(aes(color = delta,shape=delta)) + 
  ylim(c(0,1))

# Combine all sub-plots
ggpubr::ggarrange(p_location_n150_M3, p_location_n150_M5, p_location_n300_M3, p_location_n300_M5,
                  p_scale_n150_M3, p_scale_n150_M5, p_scale_n300_M3, p_scale_n300_M5,
                  p_t_n150_M3, p_t_n150_M5, p_t_n300_M3, p_t_n300_M5,
                  ncol = 4, nrow = 3, widths = c(1.07, 0.96,0.98,1.27),heights = c(1.1,1,1))



