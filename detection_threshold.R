# Detection threshold
library(KMD)
library(ggplot2)
library(plyr)
require('latex2exp')
d = 20
N1 = 20000
N2 = 10000
N = N1 + N2
h = 2
bs = (100:0)/100
rep = 200
num_cores = 24
# The following computation takes nearly 4 days to run on a server with 24 cores in parallel
Powerd20_2_1 = matrix(0,3,101)

#for (i in 1:101) {
#  b = bs[i]
#  for (j in 1:3) {
#    getPval = function(id) {
#      set.seed(id)
#      X1 = MASS::mvrnorm(N1,rep(0,d),diag(d)*9)
#      X2 = MASS::mvrnorm(N2,rep(0,d),diag(d)*(3+h/N^b)^2)
#      Pvals = KMD_test(rbind(X1,X2), c(rep(1,N1),rep(2,N2)), M = 2, Knn = j, Kernel = "discrete", Permutation = FALSE)[2]
#      return(Pvals)
#    }
#    Powerd20_2_1[j,i] = mean(unlist(parallel::mclapply(seq(1,rep), getPval, mc.cores = num_cores)) <= 0.05)
#  }
#}

# We use the saved computation result instead
load("~/Downloads/Powerd20_2_1.RData")
d_20_pi_2_1 = data.frame(b = rep(-bs,3),
                         Method = c(rep("1NN",101),rep("2NN",101),rep("3NN",101)),
                         Power = c(Powerd20_2_1[1,],Powerd20_2_1[2,],Powerd20_2_1[3,]),
                         threshold = -(1/2 - 2/20),
                         class = "C")


# The following computation takes nearly 4 days to run on a server with 24 cores in parallel
Powerd20_1_2 = matrix(0,3,101)
N1 = 10000
N2 = 20000
#for (i in 1:101) {
#  b = bs[i]
#  for (j in 1:3) {
#    getPval = function(id) {
#      set.seed(id)
#      X1 = MASS::mvrnorm(N1,rep(0,d),diag(d)*9)
#      X2 = MASS::mvrnorm(N2,rep(0,d),diag(d)*(3+h/N^b)^2)
#      Pvals = KMD_test(rbind(X1,X2), c(rep(1,N1),rep(2,N2)), M = 2, Knn = j, Kernel = "discrete", Permutation = FALSE)[2]
#      return(Pvals)
#    }
#    Powerd20_1_2[j,i] = mean(unlist(parallel::mclapply(seq(1,rep), getPval, mc.cores = num_cores)) <= 0.05)
#  }
#}

# We use the saved computation result instead
load("~/Downloads/Powerd20_1_2.RData")
d_20_pi_1_2 = data.frame(b = rep(-bs,3),
                         Method = c(rep("1NN",101),rep("2NN",101),rep("3NN",101)),
                         Power = c(Powerd20_1_2[1,],Powerd20_1_2[2,],Powerd20_1_2[3,]),
                         threshold = - 2/20,
                         class = "F")

d = 10
N1 = 20000
N2 = 10000
Powerd101 = matrix(0,3,101)
# The following code takes approximately 7.5 hours on a server (24 cores in parallel)

#for (i in 1:101) {
#  b = bs[i]
#  for (j in 1:3) {
#    getPval = function(id) {
#      set.seed(id)
#      X1 = MASS::mvrnorm(N1,rep(0,d),diag(d)*9)
#      X2 = MASS::mvrnorm(N2,rep(0,d),diag(d)*(3+h/N^b)^2)
#      Pvals = KMD_test(rbind(X1,X2), c(rep(1,N1),rep(2,N2)), M = 2, Knn = j, Kernel = "discrete", Permutation = FALSE)[2]
#      return(Pvals)
#    }
#    Powerd101[j,i] = mean(unlist(parallel::mclapply(seq(1,rep), getPval, mc.cores = num_cores)) <= 0.05)
#  }
#}

# We use the saved computation result instead
load("~/Downloads/Powerd101.RData")

N1 = 10000
N2 = 20000
Powerd102 = matrix(0,3,101)
# The following code takes approximately 7.5 hours on a server (24 cores in parallel)

#for (i in 1:101) {
#  b = bs[i]
#  for (j in 1:3) {
#    getPval = function(id) {
#      set.seed(id)
#      X1 = MASS::mvrnorm(N1,rep(0,d),diag(d)*9)
#      X2 = MASS::mvrnorm(N2,rep(0,d),diag(d)*(3+h/N^b)^2)
#      Pvals = KMD_test(rbind(X1,X2), c(rep(1,N1),rep(2,N2)), M = 2, Knn = j, Kernel = "discrete", Permutation = FALSE)[2]
#      return(Pvals)
#    }
#    Powerd102[j,i] = mean(unlist(parallel::mclapply(seq(1,rep), getPval, mc.cores = num_cores)) <= 0.05)
#  }
#}

# We use the saved computation result instead
load("~/Downloads/Powerd102.RData")

d_10_pi_2_1 = data.frame(b = rep(-bs,3),
                         Method = c(rep("1NN",101),rep("2NN",101),rep("3NN",101)),
                         Power = c(Powerd101[1,],Powerd101[2,],Powerd101[3,]),
                         threshold = -(1/2 - 2/10),
                         class = "B")
d_10_pi_1_2 = data.frame(b = rep(-bs,3),
                         Method = c(rep("1NN",101),rep("2NN",101),rep("3NN",101)),
                         Power = c(Powerd102[1,],Powerd102[2,],Powerd102[3,]),
                         threshold = - 2/10,
                         class = "E")


d = 5
N1 = 20000
N2 = 10000
Powerd51 = matrix(0,3,101)
# The following code takes approximately 35 minutes on a server (24 cores in parallel)

#for (i in 1:101) {
#  b = bs[i]
#  for (j in 1:3) {
#    getPval = function(id) {
#      set.seed(id)
#      X1 = MASS::mvrnorm(N1,rep(0,d),diag(d)*9)
#      X2 = MASS::mvrnorm(N2,rep(0,d),diag(d)*(3+h/N^b)^2)
#      Pvals = KMD_test(rbind(X1,X2), c(rep(1,N1),rep(2,N2)), M = 2, Knn = j, Kernel = "discrete", Permutation = FALSE)[2]
#      return(Pvals)
#    }
#    Powerd51[j,i] = mean(unlist(parallel::mclapply(seq(1,rep), getPval, mc.cores = num_cores)) <= 0.05)
#  }
#}

# We use the saved computation result instead
load("~/Downloads/Powerd51.RData")

N1 = 10000
N2 = 20000
Powerd52 = matrix(0,3,101)
# The following code takes approximately 35 minutes on a server (24 cores in parallel)

#for (i in 1:101) {
#  b = bs[i]
#  for (j in 1:3) {
#    getPval = function(id) {
#      set.seed(id)
#      X1 = MASS::mvrnorm(N1,rep(0,d),diag(d)*9)
#      X2 = MASS::mvrnorm(N2,rep(0,d),diag(d)*(3+h/N^b)^2)
#      Pvals = KMD_test(rbind(X1,X2), c(rep(1,N1),rep(2,N2)), M = 2, Knn = j, Kernel = "discrete", Permutation = FALSE)[2]
#      return(Pvals)
#    }
#    Powerd52[j,i] = mean(unlist(parallel::mclapply(seq(1,rep), getPval, mc.cores = num_cores)) <= 0.05)
#  }
#}

# We use the saved computation result instead
load("~/Downloads/Powerd52.RData")

d_5_pi_2_1 = data.frame(b = rep(-bs,3),
                        Method = c(rep("1NN",101),rep("2NN",101),rep("3NN",101)),
                        Power = c(Powerd51[1,],Powerd51[2,],Powerd51[3,]),
                        threshold = -1/4,
                        class = "A")
d_5_pi_1_2 = data.frame(b = rep(-bs,3),
                        Method = c(rep("1NN",101),rep("2NN",101),rep("3NN",101)),
                        Power = c(Powerd52[1,],Powerd52[2,],Powerd52[3,]),
                        threshold = -1/4,
                        class = "D")

appender <- function(string) {
  print(string)
  c(TeX("$d=5, \\pi_1 = 2/3,\\pi_2 = 1/3$"),
    TeX("$d=10, \\pi_1 = 2/3,\\pi_2 = 1/3$"),
    TeX("$d=20, \\pi_1 = 2/3,\\pi_2 = 1/3$"),
    TeX("$d=5, \\pi_1 = 1/3,\\pi_2 = 2/3$"),
    TeX("$d=10, \\pi_1 = 1/3,\\pi_2 = 2/3$"),
    TeX("$d=20, \\pi_1 = 1/3,\\pi_2 = 2/3$"))
}
df_all = rbind(d_5_pi_1_2,d_5_pi_2_1,d_10_pi_1_2,d_10_pi_2_1,d_20_pi_1_2,d_20_pi_2_1)
cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#0072B2", "#F0E442", "#D55E00", "#CC79A7")
f_all  = ggplot(df_all, aes(x=b, y=Power, group=Method,colour=Method)) +
  geom_line() + scale_colour_manual(values=cbp1) +
  facet_wrap(~class, labeller = as_labeller(appender,default = label_parsed), scales = "free_x") +
  geom_hline(yintercept=0.05, linetype="dashed", color = "gray") +
  geom_vline(data = ddply(df_all, "class", summarize, thresh = mean(threshold)), aes(xintercept=thresh),linetype="dashed",color="red")
f_all


