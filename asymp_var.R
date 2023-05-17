B = 100000
num_cores = 7

mutual_prob = function(j) {
  if (j %% 2 == 0) {
    j = j/2
    b = 1
    if (j >= 2) {
      for (i in 1:(j-1)) {
        b = b + prod(2*(1:i))/prod((1:i)*2+1)*(3/4)^i
      }
    }
    return(1/(4/3+sqrt(3)/2/pi*b))
  }
  if (j %% 2 == 1) {
    j = (j-1)/2
    b = 3
    if (j == 0) {
      return(2/3)
    }
    for (i in 1:j) {
      b = b + prod((1:i)*2-1)/prod(2*(1:i))*(3/4)^i
    }
    return(2/b)
  }
}

sig = rep(NA,3)
for (i in 1:3) {
  a = 1/3; b = 1/9; c = 1/9; d = c(1,5,10)[i]
  g1 = 1; g2 = 1; g3 = mutual_prob(d)
  sig[i] = (a*(g1+g3)+b*(g2-2*g1-2*g3-1)+c*(g1-g2+g3+1))/((1-1/3)^2)
}


ni = 150
n = 3*ni
d = 1
Null_KMD = function(id){
  set.seed(id)
  X = matrix(rnorm(n*d), nrow = n, ncol = d)
  Y = c(rep(1,ni),rep(2,ni),rep(3,ni))
  return(KMD::KMD(X, Y, M = 3, Knn = 1, Kernel = "discrete")*sqrt(n)/sig[1])
}
Thm4_d1 = unlist(parallel::mclapply(seq(1,B), Null_KMD, mc.cores = num_cores))
Null_KMD = function(id){
  set.seed(id)
  X = matrix(rnorm(n*d), nrow = n, ncol = d)
  Y = c(rep(1,ni),rep(2,ni),rep(3,ni))
  return(KMD::KMD_test(X, Y, M = 3, Knn = 1, Kernel = "discrete", Permutation = FALSE)[1,1])
}
Thm3_d1 = unlist(parallel::mclapply(seq(1,B), Null_KMD, mc.cores = num_cores))

d = 5
Null_KMD = function(id){
  set.seed(id)
  X = matrix(rnorm(n*d), nrow = n, ncol = d)
  Y = c(rep(1,ni),rep(2,ni),rep(3,ni))
  return(KMD::KMD(X, Y, M = 3, Knn = 1, Kernel = "discrete")*sqrt(n)/sig[2])
}
Thm4_d5 = unlist(parallel::mclapply(seq(1,B), Null_KMD, mc.cores = num_cores))
Null_KMD = function(id){
  set.seed(id)
  X = matrix(rnorm(n*d), nrow = n, ncol = d)
  Y = c(rep(1,ni),rep(2,ni),rep(3,ni))
  return(KMD::KMD_test(X, Y, M = 3, Knn = 1, Kernel = "discrete", Permutation = FALSE)[1,1])
}
Thm3_d5 = unlist(parallel::mclapply(seq(1,B), Null_KMD, mc.cores = num_cores))

d = 10
Null_KMD = function(id){
  set.seed(id)
  X = matrix(rnorm(n*d), nrow = n, ncol = d)
  Y = c(rep(1,ni),rep(2,ni),rep(3,ni))
  return(KMD::KMD(X, Y, M = 3, Knn = 1, Kernel = "discrete")*sqrt(n)/sig[3])
}
Thm4_d10 = unlist(parallel::mclapply(seq(1,B), Null_KMD, mc.cores = num_cores))
Null_KMD = function(id){
  set.seed(id)
  X = matrix(rnorm(n*d), nrow = n, ncol = d)
  Y = c(rep(1,ni),rep(2,ni),rep(3,ni))
  return(KMD::KMD_test(X, Y, M = 3, Knn = 1, Kernel = "discrete", Permutation = FALSE)[1,1])
}
Thm3_d10 = unlist(parallel::mclapply(seq(1,B), Null_KMD, mc.cores = num_cores))

par(mfrow = c(2, 3))
hist(Thm3_d1, breaks = c(-Inf,seq(-5,5,length=40),Inf), freq = FALSE,
     xlim = c(-4,4), ylim = c(0,0.5), main = "d = 1",
     xlab = expression(paste(hat(eta)," normalized by permutation variance")))
lines(seq(-5,5,length=1000),dnorm(seq(-5,5,length=1000)),col="red")
hist(Thm3_d5, breaks = c(-Inf,seq(-5,5,length=40),Inf), freq = FALSE,
     xlim = c(-4,4), ylim = c(0,0.5), main =  "d = 5",
     xlab = expression(paste(hat(eta)," normalized by permutation variance")))
lines(seq(-5,5,length=1000),dnorm(seq(-5,5,length=1000)),col="red")
hist(Thm3_d10, breaks = c(-Inf,seq(-5,5,length=40),Inf), freq = FALSE,
     xlim = c(-4,4), ylim = c(0,0.5), main =  "d = 10",
     xlab = expression(paste(hat(eta)," normalized by permutation variance")))
lines(seq(-5,5,length=1000),dnorm(seq(-5,5,length=1000)),col="red")
hist(Thm4_d1, breaks = c(-Inf,seq(-5,5,length=40),Inf), freq = FALSE,
     xlim = c(-4,4), ylim = c(0,0.5), main =  "d = 1",
     xlab = expression(paste(hat(eta)," normalized by limiting variance")))
lines(seq(-5,5,length=1000),dnorm(seq(-5,5,length=1000)),col="red")
hist(Thm4_d10, breaks = c(-Inf,seq(-5,5,length=40),Inf), freq = FALSE,
     xlim = c(-4,4), ylim = c(0,0.5), main =  "d = 10",
     xlab = expression(paste(hat(eta)," normalized by limiting variance")))
lines(seq(-5,5,length=1000),dnorm(seq(-5,5,length=1000)),col="red")
hist(Thm4_d5, breaks = c(-Inf,seq(-5,5,length=40),Inf), freq = FALSE,
     xlim = c(-4,4), ylim = c(0,0.5), main =  "d = 5",
     xlab = expression(paste(hat(eta)," normalized by limiting variance")))
lines(seq(-5,5,length=1000),dnorm(seq(-5,5,length=1000)),col="red")