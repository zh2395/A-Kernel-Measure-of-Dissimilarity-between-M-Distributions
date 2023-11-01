# The script reproduces the two figures (Figures 6-7) in Appendix D.3 
# on the bias and MSE of the empirical KMD

# The number of replications used to estimate the mean and MSE
B = 10000
num_cores = 7
library(ggplot2)

# eta = 1 M = 2
# Consider dimension = 2, n = 300
d = 2
n = 300
# Store the mean for k = 1,..,30 (number of nearest neighbors)
mean_eta_n300d2 = rep(NA,30)
# Store the mse for k = 1,..,30
mse_eta_n300d2 = rep(NA,30)
for (k in 1:30) {
  compute_eta = function(i) {
    set.seed(i)
    X1 = matrix(runif(n*d/2),ncol = d)
    X2 = matrix(runif(n*d/2),ncol = d)
    X2[,1] = X2[,1] + 1
    return(KMD::KMD(X = rbind(X1,X2), Y = c(rep(1,n/2),rep(2,n/2)), M = 2, Knn = k))
  }
  tmp = unlist(parallel::mclapply(seq(1,B), compute_eta, mc.cores = num_cores))
  mean_eta_n300d2[k] = mean(tmp)
  mse_eta_n300d2[k] = mean((tmp - 1)^2)
}

# Consider dimension = 2, n = 60
n = 60
mean_eta_n60d2 = rep(NA,30)
mse_eta_n60d2 = rep(NA,30)
for (k in 1:30) {
  compute_eta = function(i) {
    set.seed(i)
    X1 = matrix(runif(n*d/2),ncol = d)
    X2 = matrix(runif(n*d/2),ncol = d)
    X2[,1] = X2[,1] + 1
    return(KMD::KMD(X = rbind(X1,X2), Y = c(rep(1,n/2),rep(2,n/2)), M = 2, Knn = k))
  }
  tmp = unlist(parallel::mclapply(seq(1,B), compute_eta, mc.cores = num_cores))
  mean_eta_n60d2[k] = mean(tmp)
  mse_eta_n60d2[k] = mean((tmp - 1)^2)
}

# Consider dimension = 20, n = 60
d = 20
n = 60
mean_eta_n60d20 = rep(NA,30)
mse_eta_n60d20 = rep(NA,30)
for (k in 1:30) {
  compute_eta = function(i) {
    set.seed(i)
    X1 = matrix(runif(n*d/2),ncol = d)
    X2 = matrix(runif(n*d/2),ncol = d)
    X2[,1] = X2[,1] + 1
    return(KMD::KMD(X = rbind(X1,X2), Y = c(rep(1,n/2),rep(2,n/2)), M = 2, Knn = k))
  }
  tmp = unlist(parallel::mclapply(seq(1,B), compute_eta, mc.cores = num_cores))
  mean_eta_n60d20[k] = mean(tmp)
  mse_eta_n60d20[k] = mean((tmp - 1)^2)
}

# Consider dimension = 20, n = 300
n = 300
mean_eta_n300d20 = rep(NA,30)
mse_eta_n300d20 = rep(NA,30)
for (k in 1:30) {
  compute_eta = function(i) {
    set.seed(i)
    X1 = matrix(runif(n*d/2),ncol = d)
    X2 = matrix(runif(n*d/2),ncol = d)
    X2[,1] = X2[,1] + 1
    return(KMD::KMD(X = rbind(X1,X2), Y = c(rep(1,n/2),rep(2,n/2)), M = 2, Knn = k))
  }
  tmp = unlist(parallel::mclapply(seq(1,B), compute_eta, mc.cores = num_cores))
  mean_eta_n300d20[k] = mean(tmp)
  mse_eta_n300d20[k] = mean((tmp - 1)^2)
}


# eta = 1 M = 3
# Repeat the same procedure as above
d = 2
n = 300
mean_eta_n300d2M3 = rep(NA,30)
mse_eta_n300d2M3 = rep(NA,30)
for (k in 1:30) {
  compute_eta = function(i) {
    set.seed(i)
    X1 = matrix(runif(100*d),ncol = d)
    X2 = matrix(runif(100*d),ncol = d)
    X2[,1] = X2[,1] + 1
    X3 = matrix(runif(100*d),ncol = d)
    X3[,2] = X3[,2] + 1
    return(KMD::KMD(X = rbind(X1,X2,X3), Y = c(rep(1,100),rep(2,100),rep(3,100)), M = 3, Knn = k))
  }
  tmp = unlist(parallel::mclapply(seq(1,B), compute_eta, mc.cores = num_cores))
  mean_eta_n300d2M3[k] = mean(tmp)
  mse_eta_n300d2M3[k] = mean((tmp - 1)^2)
}

d = 20
mean_eta_n300d20M3 = rep(NA,30)
mse_eta_n300d20M3 = rep(NA,30)
for (k in 1:30) {
  compute_eta = function(i) {
    set.seed(i)
    X1 = matrix(runif(100*d),ncol = d)
    X2 = matrix(runif(100*d),ncol = d)
    X2[,1] = X2[,1] + 1
    X3 = matrix(runif(100*d),ncol = d)
    X3[,2] = X3[,2] + 1
    return(KMD::KMD(X = rbind(X1,X2,X3), Y = c(rep(1,100),rep(2,100),rep(3,100)), M = 3, Knn = k))
  }
  tmp = unlist(parallel::mclapply(seq(1,B), compute_eta, mc.cores = num_cores))
  mean_eta_n300d20M3[k] = mean(tmp)
  mse_eta_n300d20M3[k] = mean((tmp - 1)^2)
}

n = 60
d = 2
mean_eta_n60d2M3 = rep(NA,30)
mse_eta_n60d2M3 = rep(NA,30)
for (k in 1:30) {
  compute_eta = function(i) {
    set.seed(i)
    X1 = matrix(runif(20*d),ncol = d)
    X2 = matrix(runif(20*d),ncol = d)
    X3 = matrix(runif(20*d),ncol = d)
    X2[,1] = X2[,1] + 1
    X3[,2] = X3[,2] + 1
    return(KMD::KMD(X = rbind(X1,X2,X3), Y = c(rep(1,20),rep(2,20),rep(3,20)), M = 3, Knn = k))
  }
  tmp = unlist(parallel::mclapply(seq(1,B), compute_eta, mc.cores = num_cores))
  mean_eta_n60d2M3[k] = mean(tmp)
  mse_eta_n60d2M3[k] = mean((tmp - 1)^2)
}

d = 20
mean_eta_n60d20M3 = rep(NA,30)
mse_eta_n60d20M3 = rep(NA,30)
for (k in 1:30) {
  compute_eta = function(i) {
    set.seed(i)
    X1 = matrix(runif(20*d),ncol = d)
    X2 = matrix(runif(20*d),ncol = d)
    X3 = matrix(runif(20*d),ncol = d)
    X2[,1] = X2[,1] + 1
    X3[,2] = X3[,2] + 1
    return(KMD::KMD(X = rbind(X1,X2,X3), Y = c(rep(1,20),rep(2,20),rep(3,20)), M = 3, Knn = k))
  }
  tmp = unlist(parallel::mclapply(seq(1,B), compute_eta, mc.cores = num_cores))
  mean_eta_n60d20M3[k] = mean(tmp)
  mse_eta_n60d20M3[k] = mean((tmp - 1)^2)
}


# eta = 0.5 M = 2
# Repeat the same procedure as above
d = 2
n = 300

mean_eta0.5_n300d2 = rep(NA,30)
mse_eta0.5_n300d2 = rep(NA,30)
for (k in 1:30) {
  compute_eta = function(i) {
    set.seed(i)
    X1 = matrix(runif(n*d/2),ncol = d)
    X2 = matrix(runif(n*d/2),ncol = d)
    X2[,1] = X2[,1] + 1/2
    return(KMD::KMD(X = rbind(X1,X2), Y = c(rep(1,n/2),rep(2,n/2)), M = 2, Knn = k))
  }
  tmp = unlist(parallel::mclapply(seq(1,B), compute_eta, mc.cores = num_cores))
  mean_eta0.5_n300d2[k] = mean(tmp)
  mse_eta0.5_n300d2[k] = mean((tmp - 0.5)^2)
}

d = 20
mean_eta0.5_n300d20 = rep(NA,30)
mse_eta0.5_n300d20 = rep(NA,30)
for (k in 1:30) {
  compute_eta = function(i) {
    set.seed(i)
    X1 = matrix(runif(n*d/2),ncol = d)
    X2 = matrix(runif(n*d/2),ncol = d)
    X2[,1] = X2[,1] + 1/2
    return(KMD::KMD(X = rbind(X1,X2), Y = c(rep(1,n/2),rep(2,n/2)), M = 2, Knn = k))
  }
  tmp = unlist(parallel::mclapply(seq(1,B), compute_eta, mc.cores = num_cores))
  mean_eta0.5_n300d20[k] = mean(tmp)
  mse_eta0.5_n300d20[k] = mean((tmp - 0.5)^2)
}

n = 60
mean_eta0.5_n60d20 = rep(NA,30)
mse_eta0.5_n60d20 = rep(NA,30)
for (k in 1:30) {
  compute_eta = function(i) {
    set.seed(i)
    X1 = matrix(runif(n*d/2),ncol = d)
    X2 = matrix(runif(n*d/2),ncol = d)
    X2[,1] = X2[,1] + 1/2
    return(KMD::KMD(X = rbind(X1,X2), Y = c(rep(1,n/2),rep(2,n/2)), M = 2, Knn = k))
  }
  tmp = unlist(parallel::mclapply(seq(1,B), compute_eta, mc.cores = num_cores))
  mean_eta0.5_n60d20[k] = mean(tmp)
  mse_eta0.5_n60d20[k] = mean((tmp - 0.5)^2)
}

d = 2
mean_eta0.5_n60d2 = rep(NA,30)
mse_eta0.5_n60d2 = rep(NA,30)
for (k in 1:30) {
  compute_eta = function(i) {
    set.seed(i)
    X1 = matrix(runif(n*d/2),ncol = d)
    X2 = matrix(runif(n*d/2),ncol = d)
    X2[,1] = X2[,1] + 1/2
    return(KMD::KMD(X = rbind(X1,X2), Y = c(rep(1,n/2),rep(2,n/2)), M = 2, Knn = k))
  }
  tmp = unlist(parallel::mclapply(seq(1,B), compute_eta, mc.cores = num_cores))
  mean_eta0.5_n60d2[k] = mean(tmp)
  mse_eta0.5_n60d2[k] = mean((tmp - 0.5)^2)
}


# eta = 0.5 M = 3
# Repeat the same procedure as above
d = 2
n = 300
mean_eta0.5_n300d2M3 = rep(NA,30)
mse_eta0.5_n300d2M3 = rep(NA,30)
for (k in 1:30) {
  compute_eta = function(i) {
    set.seed(i)
    X1 = matrix(runif(n*d/3),ncol = d)
    X2 = matrix(runif(n*d/3),ncol = d)
    X3 = matrix(runif(n*d/3),ncol = d)
    X3[,1] = X3[,1] + 1
    return(KMD::KMD(X = rbind(X1,X2,X3), Y = c(rep(1,n/3),rep(2,n/3),rep(3,n/3)), M = 3, Knn = k))
  }
  tmp = unlist(parallel::mclapply(seq(1,B), compute_eta, mc.cores = num_cores))
  mean_eta0.5_n300d2M3[k] = mean(tmp)
  mse_eta0.5_n300d2M3[k] = mean((tmp - 0.5)^2)
}

d = 20
mean_eta0.5_n300d20M3 = rep(NA,30)
mse_eta0.5_n300d20M3 = rep(NA,30)
for (k in 1:30) {
  compute_eta = function(i) {
    set.seed(i)
    X1 = matrix(runif(n*d/3),ncol = d)
    X2 = matrix(runif(n*d/3),ncol = d)
    X3 = matrix(runif(n*d/3),ncol = d)
    X3[,1] = X3[,1] + 1
    return(KMD::KMD(X = rbind(X1,X2,X3), Y = c(rep(1,n/3),rep(2,n/3),rep(3,n/3)), M = 3, Knn = k))
  }
  tmp = unlist(parallel::mclapply(seq(1,B), compute_eta, mc.cores = num_cores))
  mean_eta0.5_n300d20M3[k] = mean(tmp)
  mse_eta0.5_n300d20M3[k] = mean((tmp - 0.5)^2)
}

d = 20
n = 60
mean_eta0.5_n60d20M3 = rep(NA,30)
mse_eta0.5_n60d20M3 = rep(NA,30)
for (k in 1:30) {
  compute_eta = function(i) {
    set.seed(i)
    X1 = matrix(runif(n*d/3),ncol = d)
    X2 = matrix(runif(n*d/3),ncol = d)
    X3 = matrix(runif(n*d/3),ncol = d)
    X3[,1] = X3[,1] + 1
    return(KMD::KMD(X = rbind(X1,X2,X3), Y = c(rep(1,n/3),rep(2,n/3),rep(3,n/3)), M = 3, Knn = k))
  }
  tmp = unlist(parallel::mclapply(seq(1,B), compute_eta, mc.cores = num_cores))
  mean_eta0.5_n60d20M3[k] = mean(tmp)
  mse_eta0.5_n60d20M3[k] = mean((tmp - 0.5)^2)
}

d = 2
mean_eta0.5_n60d2M3 = rep(NA,30)
mse_eta0.5_n60d2M3 = rep(NA,30)
for (k in 1:30) {
  compute_eta = function(i) {
    set.seed(i)
    X1 = matrix(runif(n*d/3),ncol = d)
    X2 = matrix(runif(n*d/3),ncol = d)
    X3 = matrix(runif(n*d/3),ncol = d)
    X3[,1] = X3[,1] + 1
    return(KMD::KMD(X = rbind(X1,X2,X3), Y = c(rep(1,n/3),rep(2,n/3),rep(3,n/3)), M = 3, Knn = k))
  }
  tmp = unlist(parallel::mclapply(seq(1,B), compute_eta, mc.cores = num_cores))
  mean_eta0.5_n60d2M3[k] = mean(tmp)
  mse_eta0.5_n60d2M3[k] = mean((tmp - 0.5)^2)
}

# eta = 0.1 M = 2
# Repeat the same procedure as above
d = 2
n = 300
mean_eta0.1_n300d2 = rep(NA,30)
mse_eta0.1_n300d2 = rep(NA,30)
for (k in 1:30) {
  compute_eta = function(i) {
    set.seed(i)
    X1 = matrix(runif(n*d/2),ncol = d)
    X2 = matrix(runif(n*d/2),ncol = d)
    X2[,1] = X2[,1] + 0.1
    return(KMD::KMD(X = rbind(X1,X2), Y = c(rep(1,n/2),rep(2,n/2)), M = 2, Knn = k))
  }
  tmp = unlist(parallel::mclapply(seq(1,B), compute_eta, mc.cores = num_cores))
  mean_eta0.1_n300d2[k] = mean(tmp)
  mse_eta0.1_n300d2[k] = mean((tmp - 0.1)^2)
}
d = 20
mean_eta0.1_n300d20 = rep(NA,30)
mse_eta0.1_n300d20 = rep(NA,30)
for (k in 1:30) {
  compute_eta = function(i) {
    set.seed(i)
    X1 = matrix(runif(n*d/2),ncol = d)
    X2 = matrix(runif(n*d/2),ncol = d)
    X2[,1] = X2[,1] + 0.1
    return(KMD::KMD(X = rbind(X1,X2), Y = c(rep(1,n/2),rep(2,n/2)), M = 2, Knn = k))
  }
  tmp = unlist(parallel::mclapply(seq(1,B), compute_eta, mc.cores = num_cores))
  mean_eta0.1_n300d20[k] = mean(tmp)
  mse_eta0.1_n300d20[k] = mean((tmp - 0.1)^2)
}
n = 60
mean_eta0.1_n60d20 = rep(NA,30)
mse_eta0.1_n60d20 = rep(NA,30)
for (k in 1:30) {
  compute_eta = function(i) {
    set.seed(i)
    X1 = matrix(runif(n*d/2),ncol = d)
    X2 = matrix(runif(n*d/2),ncol = d)
    X2[,1] = X2[,1] + 0.1
    return(KMD::KMD(X = rbind(X1,X2), Y = c(rep(1,n/2),rep(2,n/2)), M = 2, Knn = k))
  }
  tmp = unlist(parallel::mclapply(seq(1,B), compute_eta, mc.cores = num_cores))
  mean_eta0.1_n60d20[k] = mean(tmp)
  mse_eta0.1_n60d20[k] = mean((tmp - 0.1)^2)
}
d = 2
mean_eta0.1_n60d2 = rep(NA,30)
mse_eta0.1_n60d2 = rep(NA,30)
for (k in 1:30) {
  compute_eta = function(i) {
    set.seed(i)
    X1 = matrix(runif(n*d/2),ncol = d)
    X2 = matrix(runif(n*d/2),ncol = d)
    X2[,1] = X2[,1] + 0.1
    return(KMD::KMD(X = rbind(X1,X2), Y = c(rep(1,n/2),rep(2,n/2)), M = 2, Knn = k))
  }
  tmp = unlist(parallel::mclapply(seq(1,B), compute_eta, mc.cores = num_cores))
  mean_eta0.1_n60d2[k] = mean(tmp)
  mse_eta0.1_n60d2[k] = mean((tmp - 0.1)^2)
}


# eta = 0.1 M = 3
# Repeat the same procedure as above
d = 2
n = 300
mean_eta0.1_n300d2M3 = rep(NA,30)
mse_eta0.1_n300d2M3 = rep(NA,30)
for (k in 1:30) {
  compute_eta = function(i) {
    set.seed(i)
    X1 = matrix(runif(n*d/3),ncol = d)
    X2 = matrix(runif(n*d/3),ncol = d)
    X3 = matrix(runif(n*d/3),ncol = d)
    X3[,1] = X3[,1] + 0.2
    return(KMD::KMD(X = rbind(X1,X2,X3), Y = c(rep(1,n/3),rep(2,n/3),rep(3,n/3)), M = 3, Knn = k))
  }
  tmp = unlist(parallel::mclapply(seq(1,B), compute_eta, mc.cores = num_cores))
  mean_eta0.1_n300d2M3[k] = mean(tmp)
  mse_eta0.1_n300d2M3[k] = mean((tmp - 0.1)^2)
}
d = 20
mean_eta0.1_n300d20M3 = rep(NA,30)
mse_eta0.1_n300d20M3 = rep(NA,30)
for (k in 1:30) {
  compute_eta = function(i) {
    set.seed(i)
    X1 = matrix(runif(n*d/3),ncol = d)
    X2 = matrix(runif(n*d/3),ncol = d)
    X3 = matrix(runif(n*d/3),ncol = d)
    X3[,1] = X3[,1] + 0.2
    return(KMD::KMD(X = rbind(X1,X2,X3), Y = c(rep(1,n/3),rep(2,n/3),rep(3,n/3)), M = 3, Knn = k))
  }
  tmp = unlist(parallel::mclapply(seq(1,B), compute_eta, mc.cores = num_cores))
  mean_eta0.1_n300d20M3[k] = mean(tmp)
  mse_eta0.1_n300d20M3[k] = mean((tmp - 0.1)^2)
}
n = 60
mean_eta0.1_n60d20M3 = rep(NA,30)
mse_eta0.1_n60d20M3 = rep(NA,30)
for (k in 1:30) {
  compute_eta = function(i) {
    set.seed(i)
    X1 = matrix(runif(n*d/3),ncol = d)
    X2 = matrix(runif(n*d/3),ncol = d)
    X3 = matrix(runif(n*d/3),ncol = d)
    X3[,1] = X3[,1] + 0.2
    return(KMD::KMD(X = rbind(X1,X2,X3), Y = c(rep(1,n/3),rep(2,n/3),rep(3,n/3)), M = 3, Knn = k))
  }
  tmp = unlist(parallel::mclapply(seq(1,B), compute_eta, mc.cores = num_cores))
  mean_eta0.1_n60d20M3[k] = mean(tmp)
  mse_eta0.1_n60d20M3[k] = mean((tmp - 0.1)^2)
}
d = 2
mean_eta0.1_n60d2M3 = rep(NA,30)
mse_eta0.1_n60d2M3 = rep(NA,30)
for (k in 1:30) {
  compute_eta = function(i) {
    set.seed(i)
    X1 = matrix(runif(n*d/3),ncol = d)
    X2 = matrix(runif(n*d/3),ncol = d)
    X3 = matrix(runif(n*d/3),ncol = d)
    X3[,1] = X3[,1] + 0.2
    return(KMD::KMD(X = rbind(X1,X2,X3), Y = c(rep(1,n/3),rep(2,n/3),rep(3,n/3)), M = 3, Knn = k))
  }
  tmp = unlist(parallel::mclapply(seq(1,B), compute_eta, mc.cores = num_cores))
  mean_eta0.1_n60d2M3[k] = mean(tmp)
  mse_eta0.1_n60d2M3[k] = mean((tmp - 0.1)^2)
}


# A color blind friendly palette
cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#0072B2", "#F0E442", "#D55E00", "#CC79A7")
# Generate the six sub-plots and combine them together
# Mean of the estimators
p_eta1M2 = ggplot(data.frame(k = rep(1:30,4),
                             d.n = c(rep("(2,60)",30),rep("(2,300)",30),rep("(20,60)",30),rep("(20,300)",30)),
                             Mean = c(mean_eta_n60d2,mean_eta_n300d2,mean_eta_n60d20,mean_eta_n300d20)),
                  aes(x = k, y = Mean, color = d.n)) +
  geom_line(aes(color = d.n, linetype = d.n)) +
  ylab(expression(atop("M = 2","Mean of the estimator"))) +
  geom_point(aes(color = d.n,shape=d.n)) +
  scale_colour_manual(values=cbp1) +
  theme(legend.position = "none",plot.title = element_text(hjust = 0.5)) +
  ggtitle("eta = 1") +
  ylim(c(0,1.04))
p_eta0.5M2 = ggplot(data.frame(k = rep(1:30,4),
                               d.n = c(rep("(2,60)",30),rep("(2,300)",30),rep("(20,60)",30),rep("(20,300)",30)),
                               Mean = c(mean_eta0.5_n60d2,mean_eta0.5_n300d2,mean_eta0.5_n60d20,mean_eta0.5_n300d20)),
                    aes(x = k, y = Mean)) +
  geom_line(aes(color = d.n, linetype = d.n)) +
  ylab("Mean of the estimator") +
  scale_colour_manual(values=cbp1) +
  geom_point(aes(color = d.n,shape=d.n)) +
  theme(legend.position = "none",plot.title = element_text(hjust = 0.5)) +
  ggtitle("eta = 0.5") +
  ylim(c(0,0.5))

p_eta0.1M2 = ggplot(data.frame(k = rep(1:30,4),
                               d.n = c(rep("(2,60)",30),rep("(2,300)",30),rep("(20,60)",30),rep("(20,300)",30)),
                               Mean = c(mean_eta0.1_n60d2,mean_eta0.1_n300d2,mean_eta0.1_n60d20,mean_eta0.1_n300d20)),
                    aes(x = k, y = Mean)) +
  geom_line(aes(color = d.n, linetype = d.n)) +
  ylab("Mean of the estimator") +
  labs(col = "(d,n)", linetype ="(d,n)", shape="(d,n)") +
  scale_colour_manual(values=cbp1) +
  geom_point(aes(color = d.n,shape=d.n)) +
  theme(plot.title = element_text(hjust = 0.5),legend.box.margin = margin(0, -8, 0, 0)) +
  ggtitle("eta = 0.1") +
  ylim(c(0,0.1))

p_eta1M3 = ggplot(data.frame(k = rep(1:30,4),
                             d.n = c(rep("(2,60)",30),rep("(2,300)",30),rep("(20,60)",30),rep("(20,300)",30)),
                             Mean = c(mean_eta_n60d2M3,mean_eta_n300d2M3,mean_eta_n60d20M3,mean_eta_n300d20M3)),
                  aes(x = k, y = Mean)) +
  geom_line(aes(color = d.n, linetype = d.n)) +
  ylab(expression(atop("M = 3","Mean of the estimator"))) +
  scale_colour_manual(values=cbp1) +
  geom_point(aes(color = d.n,shape=d.n)) +
  theme(legend.position = "none",plot.title = element_text(hjust = 0.5)) +
  ylim(c(0,1))
p_eta0.5M3 = ggplot(data.frame(k = rep(1:30,4),
                               d.n = c(rep("(2,60)",30),rep("(2,300)",30),rep("(20,60)",30),rep("(20,300)",30)),
                               Mean = c(mean_eta0.5_n60d2M3,mean_eta0.5_n300d2M3,mean_eta0.5_n60d20M3,mean_eta0.5_n300d20M3)),
                    aes(x = k, y = Mean)) +
  geom_line(aes(color = d.n, linetype = d.n)) +
  ylab("Mean of the estimator") +
  scale_colour_manual(values=cbp1) +
  geom_point(aes(color = d.n,shape=d.n)) +
  theme(legend.position = "none",plot.title = element_text(hjust = 0.5)) +
  ylim(c(0,0.5))

p_eta0.1M3 = ggplot(data.frame(k = rep(1:30,4),
                               d.n = c(rep("(2,60)",30),rep("(2,300)",30),rep("(20,60)",30),rep("(20,300)",30)),
                               Mean = c(mean_eta0.1_n60d2M3,mean_eta0.1_n300d2M3,mean_eta0.1_n60d20M3,mean_eta0.1_n300d20M3)),
                    aes(x = k, y = Mean)) +
  geom_line(aes(color = d.n, linetype = d.n)) +
  ylab("Mean of the estimator") +
  labs(col = "(d,n)", linetype ="(d,n)", shape="(d,n)") +
  scale_colour_manual(values=cbp1) +
  geom_point(aes(color = d.n,shape=d.n)) +
  theme(plot.title = element_text(hjust = 0.5),legend.box.margin = margin(0, -8, 0, 0)) +
  ylim(c(0,0.1))

# Combine the plots together
ggpubr::ggarrange(p_eta1M2, p_eta0.5M2, p_eta0.1M2,
                  p_eta1M3, p_eta0.5M3, p_eta0.1M3,
                  ncol = 3, nrow = 2, widths = c(1,0.9,1.25) )


# MSE of the estimators
p_eta1M2_MSE = ggplot(data.frame(k = rep(1:30,4),
                             d.n = c(rep("(2,60)",30),rep("(2,300)",30),rep("(20,60)",30),rep("(20,300)",30)),
                             MSE = c(mse_eta_n60d2,mse_eta_n300d2,mse_eta_n60d20,mse_eta_n300d20)),
                  aes(x = k, y = MSE, color = d.n)) +
  geom_line(aes(color = d.n, linetype = d.n)) +
  ylab(expression(atop("M = 2","MSE"))) +
  geom_point(aes(color = d.n,shape=d.n)) + 
  scale_colour_manual(values=cbp1) +
  theme(legend.position = "none",plot.title = element_text(hjust = 0.5)) +
  ggtitle("eta = 1") #+ylim(c(0,1))

p_eta0.5M2_MSE = ggplot(data.frame(k = rep(1:30,4),
                               d.n = c(rep("(2,60)",30),rep("(2,300)",30),rep("(20,60)",30),rep("(20,300)",30)),
                               MSE = c(mse_eta0.5_n60d2,mse_eta0.5_n300d2,mse_eta0.5_n60d20,mse_eta0.5_n300d20)),
                    aes(x = k, y = MSE)) +
  geom_line(aes(color = d.n, linetype = d.n)) +
  ylab("MSE") +
  scale_colour_manual(values=cbp1) +
  geom_point(aes(color = d.n,shape=d.n)) + 
  theme(legend.position = "none",plot.title = element_text(hjust = 0.5)) +
  ggtitle("eta = 0.5")

p_eta0.1M2_MSE = ggplot(data.frame(k = rep(1:30,4),
                               d.n = c(rep("(2,60)",30),rep("(2,300)",30),rep("(20,60)",30),rep("(20,300)",30)),
                               MSE = c(mse_eta0.1_n60d2,mse_eta0.1_n300d2,mse_eta0.1_n60d20,mse_eta0.1_n300d20)),
                    aes(x = k, y = MSE)) +
  geom_line(aes(color = d.n, linetype = d.n)) +
  ylab("MSE") +
  labs(col = "(d,n)", linetype ="(d,n)", shape="(d,n)") + 
  scale_colour_manual(values=cbp1) +
  geom_point(aes(color = d.n,shape=d.n)) + 
  theme(plot.title = element_text(hjust = 0.5),legend.box.margin = margin(0, -8, 0, 0)) +
  ggtitle("eta = 0.1") 

p_eta1M3_MSE = ggplot(data.frame(k = rep(1:30,4),
                             d.n = c(rep("(2,60)",30),rep("(2,300)",30),rep("(20,60)",30),rep("(20,300)",30)),
                             MSE = c(mse_eta_n60d2M3,mse_eta_n300d2M3,mse_eta_n60d20M3,mse_eta_n300d20M3)),
                  aes(x = k, y = MSE)) +
  geom_line(aes(color = d.n, linetype = d.n)) +
  ylab(expression(atop("M = 3","MSE"))) +
  scale_colour_manual(values=cbp1) +
  geom_point(aes(color = d.n,shape=d.n)) + 
  theme(legend.position = "none",plot.title = element_text(hjust = 0.5)) 
p_eta0.5M3_MSE = ggplot(data.frame(k = rep(1:30,4),
                               d.n = c(rep("(2,60)",30),rep("(2,300)",30),rep("(20,60)",30),rep("(20,300)",30)),
                               MSE = c(mse_eta0.5_n60d2M3,mse_eta0.5_n300d2M3,mse_eta0.5_n60d20M3,mse_eta0.5_n300d20M3)),
                    aes(x = k, y = MSE)) +
  geom_line(aes(color = d.n, linetype = d.n)) +
  ylab("MSE") +
  scale_colour_manual(values=cbp1) +
  geom_point(aes(color = d.n,shape=d.n)) + 
  theme(legend.position = "none",plot.title = element_text(hjust = 0.5)) 

p_eta0.1M3_MSE = ggplot(data.frame(k = rep(1:30,4),
                               d.n = c(rep("(2,60)",30),rep("(2,300)",30),rep("(20,60)",30),rep("(20,300)",30)),
                               MSE = c(mse_eta0.1_n60d2M3,mse_eta0.1_n300d2M3,mse_eta0.1_n60d20M3,mse_eta0.1_n300d20M3)),
                    aes(x = k, y = MSE)) +
  geom_line(aes(color = d.n, linetype = d.n)) +
  ylab("MSE") +
  labs(col = "(d,n)", linetype ="(d,n)", shape="(d,n)") + 
  scale_colour_manual(values=cbp1) +
  geom_point(aes(color = d.n,shape=d.n)) + 
  theme(plot.title = element_text(hjust = 0.5),legend.box.margin = margin(0, -8, 0, 0))
# Combine the plots together
ggpubr::ggarrange(p_eta1M2_MSE, p_eta0.5M2_MSE, p_eta0.1M2_MSE,
                  p_eta1M3_MSE, p_eta0.5M3_MSE, p_eta0.1M3_MSE,
                  ncol = 3, nrow = 2, widths = c(1,0.9,1.25) )
