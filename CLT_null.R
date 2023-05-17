# CLT under null
num_cores = 7
par(mfrow = c(1, 3))
for (k in 1:3) {
  ni = 10^(k+1)
  n = 3*ni
  d = 2
  Null_KMD = function(id){
    set.seed(id)
    X = matrix(rnorm(n*d), nrow = n, ncol = d)
    Y = c(rep(1,ni),rep(2,ni),rep(3,ni))
    return(KMD::KMD_test(X, Y, M = 3, Knn = 1, Kernel = "discrete", Permutation = FALSE)[1,1])
  }
  normalize_eta = unlist(parallel::mclapply(seq(1,20000), Null_KMD, mc.cores = num_cores))
  hist(normalize_eta, breaks = c(-Inf,seq(-5,5,length=50),Inf), freq = FALSE,
       xlim = c(-4,4), ylim = c(0,0.5), main = bquote(n[i] ~ "=" ~ .(ni)),
       xlab = expression(paste("normalized ",hat(eta))))
  lines(seq(-5,5,length=1000),dnorm(seq(-5,5,length=1000)),col="red")
}