# Power curves
# Packages needed: KMD, gridExtra
library(nbpMatching)
library(energy)
library(ggplot2)
replic = 1000
num_cores = 7

# Creates the null covariance matrix for mmcm, corresponding to the scenario when all K distributions are the same
mhcccreate<-function(nvec)
{
  k<-length(nvec)
  n<-sum(nvec)
  mu1<-rep(0,k)
  sig1<-matrix(0,k,k)
  for(i in 1:k)
  {
    mu1[i]<-(nvec[i]*(nvec[i]-1))/(2*(n-1))
    sig1[i,i]<-(((nvec[i]*(nvec[i]-1))/(2*(n-1)))*(1-((nvec[i]*(nvec[i]-1))/(2*(n-1)))))+((nvec[i]*(nvec[i]-1)*(nvec[i]-2)*(nvec[i]-3))/(4*(n-1)*(n-3)))
  }
  for(i in 1:k)
  {
    for(j in setdiff(1:k,i))
    {
      sig1[i,j]<-((nvec[i])*(nvec[j])*(nvec[i]-1)*(nvec[j]-1))/(2*((n-1)^2)*(n-3))
    }
  }
  mu<-matrix(0,k,k)
  for(i in 1:k)
  {
    for(j in i:k)
    {
      mu[i,j]<-(((nvec[i])*(nvec[j]))/(n-1))
    }
  }
  muv<-t(mu)[lower.tri(t(mu))]
  bigsig<-matrix(0,(((k^2)-k)/2),(((k^2)-k)/2))
  for(i in 1:(k-1))
  {
    for(j in (i+1):k)
    {
      bigsig[(((i-1)*k)-((i*(i-1))/2)+j-i),(((i-1)*k)-((i*(i-1))/2)+j-i)]<- ((nvec[i]*nvec[j]*(nvec[i]-1)*(nvec[j]-1))/((n-1)*(n-3)))+(((nvec[i]*nvec[j])/(n-1))*(1-((nvec[i]*nvec[j])/(n-1))))
    }
  }
  if(k>=3)
  {
    for(i in 1:(k-2))
    {
      for(j in (i+1):(k-1))
      {
        for(l in (j+1):k)
        {
          bigsig[(((i-1)*k)-((i*(i-1))/2)+j-i),(((i-1)*k)-((i*(i-1))/2)+l-i)]<-((nvec[i]*(nvec[i]-1)*nvec[j]*nvec[l])/((n-1)*(n-3))) - ((((nvec[i])^2)*nvec[j]*nvec[l])/((n-1)^2))
          bigsig[(((i-1)*k)-((i*(i-1))/2)+l-i),(((i-1)*k)-((i*(i-1))/2)+j-i)]<-((nvec[i]*(nvec[i]-1)*nvec[j]*nvec[l])/((n-1)*(n-3))) - ((((nvec[i])^2)*nvec[j]*nvec[l])/((n-1)^2))
          bigsig[(((i-1)*k)-((i*(i-1))/2)+j-i),(((j-1)*k)-((j*(j-1))/2)+l-j)]<-((nvec[j]*(nvec[j]-1)*nvec[i]*nvec[l])/((n-1)*(n-3))) - ((((nvec[j])^2)*nvec[i]*nvec[l])/((n-1)^2))
          bigsig[(((j-1)*k)-((j*(j-1))/2)+l-j),(((i-1)*k)-((i*(i-1))/2)+j-i)]<-((nvec[j]*(nvec[j]-1)*nvec[i]*nvec[l])/((n-1)*(n-3))) - ((((nvec[j])^2)*nvec[i]*nvec[l])/((n-1)^2))
          bigsig[(((j-1)*k)-((j*(j-1))/2)+l-j),(((i-1)*k)-((i*(i-1))/2)+l-i)]<-((nvec[l]*(nvec[l]-1)*nvec[j]*nvec[i])/((n-1)*(n-3))) - ((((nvec[l])^2)*nvec[j]*nvec[i])/((n-1)^2))
          bigsig[(((i-1)*k)-((i*(i-1))/2)+l-i),(((j-1)*k)-((j*(j-1))/2)+l-j)]<-((nvec[l]*(nvec[l]-1)*nvec[j]*nvec[i])/((n-1)*(n-3))) - ((((nvec[l])^2)*nvec[j]*nvec[i])/((n-1)^2))
        }
      }
    }
  }
  if(k>=4)
  {
    for(i in 1:(k-3))
    {
      for(j in (i+1):(k-2))
      {
        for(l in (j+1):(k-1))
        {
          for(m in (l+1):k)
          {
            bigsig[(((i-1)*k)-((i*(i-1))/2)+j-i),(((l-1)*k)-((l*(l-1))/2)+m-l)]<-(2*nvec[i]*nvec[j]*nvec[l]*nvec[m])/(((n-1)^2)*(n-3))
            bigsig[(((i-1)*k)-((i*(i-1))/2)+l-i),(((j-1)*k)-((j*(j-1))/2)+m-j)]<-(2*nvec[i]*nvec[j]*nvec[l]*nvec[m])/(((n-1)^2)*(n-3))
            bigsig[(((i-1)*k)-((i*(i-1))/2)+m-i),(((j-1)*k)-((j*(j-1))/2)+l-j)]<-(2*nvec[i]*nvec[j]*nvec[l]*nvec[m])/(((n-1)^2)*(n-3))
            bigsig[(((j-1)*k)-((j*(j-1))/2)+l-j),(((i-1)*k)-((i*(i-1))/2)+m-i)]<-(2*nvec[i]*nvec[j]*nvec[l]*nvec[m])/(((n-1)^2)*(n-3))
            bigsig[(((j-1)*k)-((j*(j-1))/2)+m-j),(((i-1)*k)-((i*(i-1))/2)+l-i)]<-(2*nvec[i]*nvec[j]*nvec[l]*nvec[m])/(((n-1)^2)*(n-3))
            bigsig[(((l-1)*k)-((l*(l-1))/2)+m-l),(((i-1)*k)-((i*(i-1))/2)+j-i)]<-(2*nvec[i]*nvec[j]*nvec[l]*nvec[m])/(((n-1)^2)*(n-3))
          }
        }
      }
    }
  }
  return(list(as.matrix(mu1),sig1,as.matrix(muv),bigsig))
}
# Calculates the pairwise crosscounts for the K classes being examined
mhccexecutelong<-function(nvec, apmat)
{
  k<-length(nvec)
  n<-sum(nvec)
  smatch <- as.matrix(nonbimatch(distancematrix(as.matrix(stats::dist(apmat, method = "euclidean", diag = TRUE, upper = TRUE, p = 2))))$matches)
  multcm<-rep(0,k)
  cs<-c(0,cumsum(nvec))
  for(i in 1:k)
  {
    for(j in (cs[i]+1):(cs[i+1]))
    {
      multcm[i]<-multcm[i]+((as.numeric(smatch[j,4])>cs[i])&&(as.numeric(smatch[j,4])<=cs[i+1]))
    }
  }
  multcm<-multcm/2
  A<-matrix(0,k,k)
  for(i in 1:k)
  {
    for(j in i:k)
    {
      for(l in (cs[i]+1):(cs[i+1]))
      {
        A[i,j]<-A[i,j]+((as.numeric(smatch[l,4])>cs[j])&&(as.numeric(smatch[l,4])<=cs[j+1]))
      }
    }
  }
  av<-t(A)[lower.tri(t(A))]
  return(list(as.matrix(multcm),as.matrix(av)))
}

# Compute the statistics of MCM/MMCM for permutation test
# nvec = (n_1,...,n_M)
CMPermTest = function(data_list,B) {
  nvec<-rep(0,length(data_list))
  apmat<-c()
  for(i in (1:length(data_list))) 
  {
    nvec[i]<-nrow(data_list[[i]])
    apmat<-rbind(apmat,data_list[[i]])
  }
  
  n = sum(nvec)
  ll<-mhcccreate(nvec)
  muv<-ll[[3]]
  inv_bigsig<-solve(ll[[4]])
  
  lll<-mhccexecutelong(nvec,apmat)
  multcm<-lll[[1]]
  av<-lll[[2]]
  MMCMstat<-t(as.matrix(av)-as.matrix(muv))%*%inv_bigsig%*%(as.matrix(av)-as.matrix(muv))
  MCMstat = sum(av)
  PermLarge = function(j) {
    perm_apmat = apmat[sample(1:n,size=n,replace=FALSE),]
    lll<-mhccexecutelong(nvec,perm_apmat)
    multcm<-lll[[1]]
    av<-lll[[2]]
    permMMCMstat<-t(as.matrix(av)-as.matrix(muv))%*%inv_bigsig%*%(as.matrix(av)-as.matrix(muv))
    permMCMstat = sum(av)
    return(c(permMMCMstat, MCMstat) >= c(MMCMstat, permMCMstat))
  }
  
  pval = base::apply(cbind(c(1,1),sapply(1:B, PermLarge)), 1, base::mean)
  return(pval)
}


# normal location
dims = 2^(1:9)
getPval = function(id) {
  Pvals = rep(NA,length(dims))
  for (i in 1:length(dims)) {
    d = dims[i]
    set.seed(id)
    X1 = MASS::mvrnorm(100,rep(0,d),diag(d))
    X2 = MASS::mvrnorm(100,rep(0.1,d),diag(d))
    X3 = MASS::mvrnorm(100,rep(0.2,d),diag(d))
    Pvals[i] = KMD::KMD_test(X=rbind(X1,X2,X3), Y=rep(1:3,each=100), M = 3, Knn = 30)
  }
  return(Pvals)
}
PermPval30NN = matrix(unlist(parallel::mclapply(seq(1,replic), getPval, mc.cores = num_cores)),ncol = length(dims),byrow = TRUE)


getPval = function(id) {
  Pvals = rep(NA,length(dims)*2)
  for (i in 1:length(dims)) {
    d = dims[i]
    set.seed(id)
    X1 = MASS::mvrnorm(100,rep(0,d),diag(d))
    X2 = MASS::mvrnorm(100,rep(0.1,d),diag(d))
    X3 = MASS::mvrnorm(100,rep(0.2,d),diag(d))
    MMCM_MCM = CMPermTest(list(X1,X2,X3),500)
    Pvals[i+length(dims)] = MMCM_MCM[1]
    Pvals[i] = MMCM_MCM[2]
  }
  return(Pvals)
}
PermPvalCM = matrix(unlist(parallel::mclapply(seq(1,replic), getPval, mc.cores = num_cores)),ncol = length(dims)*2,byrow = TRUE)

getPval = function(id) {
  Pvals = rep(NA,length(dims))
  for (i in 1:length(dims)) {
    d = dims[i]
    set.seed(id)
    X1 = MASS::mvrnorm(100,rep(0,d),diag(d))
    X2 = MASS::mvrnorm(100,rep(0.1,d),diag(d))
    X3 = MASS::mvrnorm(100,rep(0.2,d),diag(d))
    Pvals[i] = disco(rbind(X1,X2,X3), gl(3,100), distance=FALSE,R=500)$p.value
  }
  return(Pvals)
}
PermPvaldisco = matrix(unlist(parallel::mclapply(seq(1,replic), getPval, mc.cores = num_cores)),ncol = length(dims),byrow = TRUE)

getPval = function(id) {
  Pvals = rep(NA,length(dims))
  for (i in 1:8) {
    d = dims[i]
    set.seed(id)
    X1 = MASS::mvrnorm(100,rep(0,d),diag(d))
    X2 = MASS::mvrnorm(100,rep(0.1,d),diag(d))
    X3 = MASS::mvrnorm(100,rep(0.2,d),diag(d))
    Pvals[i] = summary(manova(rbind(X1,X2,X3)~as.factor(c(rep(1,100),rep(2,100),rep(3,100)))),test="Pillai")$stats[1,"Pr(>F)"]
  }
  return(Pvals)
}
ParaPvalPillai = matrix(unlist(parallel::mclapply(seq(1,replic), getPval, mc.cores = num_cores)),ncol = length(dims),byrow = TRUE)

getPval = function(id) {
  Pvals = rep(NA,length(dims))
  for (i in 1:8) {
    d = dims[i]
    set.seed(id)
    X1 = MASS::mvrnorm(100,rep(0,d),diag(d))
    X2 = MASS::mvrnorm(100,rep(0.1,d),diag(d))
    X3 = MASS::mvrnorm(100,rep(0.2,d),diag(d))
    Pvals[i] = summary(manova(rbind(X1,X2,X3)~as.factor(c(rep(1,100),rep(2,100),rep(3,100)))),test="Roy")$stats[1,"Pr(>F)"]
  }
  return(Pvals)
}
ParaPvalRoy = matrix(unlist(parallel::mclapply(seq(1,replic), getPval, mc.cores = num_cores)),ncol = length(dims),byrow = TRUE)

df1 = data.frame(Dimension = rep(dims,6),
                 Method = c(rep("KMD",length(dims)),rep("Pillai",length(dims)),rep("Roy",length(dims)),rep("MCM",length(dims)),rep("MMCM",length(dims)),rep("DISCO",length(dims))),
                 Power = c(apply(PermPval30NN<=0.05,2,mean),apply(ParaPvalPillai<=0.05,2,mean),apply(ParaPvalRoy<=0.05,2,mean),apply(PermPvalCM[,1:length(dims)]<=0.05,2,mean),apply(PermPvalCM[,(length(dims)+1):(2*length(dims))]<=0.05,2,mean),apply(PermPvaldisco<=0.05,2,mean)))
cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#0072B2", "#F0E442", "#D55E00", "#CC79A7")

p1 = ggplot(df1, aes(x=Dimension, y=Power, group=Method,colour=Method)) +
  geom_point() + geom_line() + scale_colour_manual(values=cbp1[-3]) +
  scale_x_continuous(trans='log2',n.breaks = length(dims)) + geom_vline(xintercept = 300, linetype="dashed", colour = "grey") +
  theme(legend.position="bottom") + guides(color=guide_legend(nrow=2,byrow=TRUE))+ ggtitle("Normal location")



# normal scale problem
dims = round(exp(seq(log(2), log(2^6), length.out = 11)))
getPval = function(id) {
  Pvals = rep(NA,length(dims))
  for (i in 1:length(dims)) {
    d = dims[i]
    set.seed(id)
    X1 = MASS::mvrnorm(100,rep(0,d),diag(d))
    X2 = MASS::mvrnorm(100,rep(0,d),diag(rep(1.5,d)))
    X3 = MASS::mvrnorm(100,rep(0,d),diag(rep(2,d)))
    Pvals[i] = KMD::KMD_test(X=rbind(X1,X2,X3), Y=rep(1:3,each=100), M = 3, Knn = 30)
  }
  return(Pvals)
}
PermPval30NNS = matrix(unlist(parallel::mclapply(seq(1,replic), getPval, mc.cores = num_cores)),ncol = length(dims),byrow = TRUE)

getPval = function(id) {
  Pvals = rep(NA,length(dims))
  for (i in 1:length(dims)) {
    d = dims[i]
    set.seed(id)
    X1 = MASS::mvrnorm(100,rep(0,d),diag(d))
    X2 = MASS::mvrnorm(100,rep(0,d),diag(rep(1.5,d)))
    X3 = MASS::mvrnorm(100,rep(0,d),diag(rep(2,d)))
    Pvals[i] = KMD::KMD_test(X=rbind(X1,X2,X3), Y=rep(1:3,each=100), M = 3, Knn = 30, Kernel = diag(c(10,1,1)))
  }
  return(Pvals)
}
PermPval30NNSK2 = matrix(unlist(parallel::mclapply(seq(1,replic), getPval, mc.cores = num_cores)),ncol = length(dims),byrow = TRUE)

getPval = function(id) {
  Pvals = rep(NA,length(dims)*2)
  for (i in 1:length(dims)) {
    d = dims[i]
    set.seed(id)
    X1 = MASS::mvrnorm(100,rep(0,d),diag(d))
    X2 = MASS::mvrnorm(100,rep(0,d),diag(rep(1.5,d)))
    X3 = MASS::mvrnorm(100,rep(0,d),diag(rep(2,d)))
    MMCM_MCM = CMPermTest(list(X1,X2,X3),500)
    Pvals[i+length(dims)] = MMCM_MCM[1]
    Pvals[i] = MMCM_MCM[2]
  }
  return(Pvals)
}
PermPvalCMS = matrix(unlist(parallel::mclapply(seq(1,replic), getPval, mc.cores = num_cores)),ncol = length(dims)*2,byrow = TRUE)

getPval = function(id) {
  Pvals = rep(NA,length(dims))
  for (i in 1:length(dims)) {
    d = dims[i]
    set.seed(id)
    X1 = MASS::mvrnorm(100,rep(0,d),diag(d))
    X2 = MASS::mvrnorm(100,rep(0,d),diag(rep(1.5,d)))
    X3 = MASS::mvrnorm(100,rep(0,d),diag(rep(2,d)))
    Pvals[i] = disco(rbind(X1,X2,X3), gl(3,100), distance=FALSE,R=500)$p.value
  }
  return(Pvals)
}
PermPvaldiscoS = matrix(unlist(parallel::mclapply(seq(1,replic), getPval, mc.cores = num_cores)),ncol = length(dims),byrow = TRUE)

getPval = function(id) {
  Pvals = rep(NA,length(dims))
  for (i in 1:length(dims)) {
    d = dims[i]
    set.seed(id)
    X1 = MASS::mvrnorm(100,rep(0,d),diag(d))
    X2 = MASS::mvrnorm(100,rep(0,d),diag(rep(1.5,d)))
    X3 = MASS::mvrnorm(100,rep(0,d),diag(rep(2,d)))
    Pvals[i] = summary(manova(rbind(X1,X2,X3)~as.factor(c(rep(1,100),rep(2,100),rep(3,100)))),test="Roy")$stats[1,"Pr(>F)"]
  }
  return(Pvals)
}
PermPvalRoyS = matrix(unlist(parallel::mclapply(seq(1,replic), getPval, mc.cores = num_cores)),ncol = length(dims),byrow = TRUE)

df2 = data.frame(Dimension = rep(dims,6),
                 Method = c(rep("KMD",length(dims)),rep("KMD(K2)",length(dims)),rep("MCM",length(dims)),rep("MMCM",length(dims)),rep("DISCO",length(dims)),rep("Roy",length(dims))),
                 Power = c(apply(PermPval30NNS<=0.05,2,mean),apply(PermPval30NNSK2<=0.05,2,mean),apply(PermPvalCMS[,1:length(dims)]<=0.05,2,mean),apply(PermPvalCMS[,(length(dims)+1):(2*length(dims))]<=0.05,2,mean),apply(PermPvaldiscoS<=0.05,2,mean),apply(PermPvalRoyS<=0.05,2,mean)),
                 task = "Permutation Test")
p2 = ggplot(df2, aes(x=Dimension, y=Power, group=Method,colour=Method)) +
  geom_point() + geom_line() + scale_colour_manual(values=cbp1[-6]) +
  scale_x_continuous(trans='log2')  +
  theme(legend.position="bottom",legend.text = element_text(size=7),legend.key.size = unit(0.6, 'cm'))+guides(color=guide_legend(nrow=2,byrow=TRUE)) + 
  ggtitle("Normal scale")


# t-distribution
delta = (0:7)/10
d = 16
getPval = function(id) {
  Pvals = rep(NA,length(delta))
  for (i in 1:length(delta)) {
    set.seed(id)
    X1 = matrix(rt(100*d,1,delta[i]),ncol = d)
    X2 = matrix(rt(100*d,1),ncol = d)
    X3 = matrix(rt(100*d,1),ncol = d)
    Pvals[i] = KMD::KMD_test(X=rbind(X1,X2,X3), Y=rep(1:3,each=100), M = 3, Knn = 30)
  }
  return(Pvals)
}
PermPval30NNt = matrix(unlist(parallel::mclapply(seq(1,replic), getPval, mc.cores = num_cores)),ncol = length(delta),byrow = TRUE)

getPval = function(id) {
  Pvals = rep(NA,2*length(delta))
  for (i in 1:length(delta)) {
    set.seed(id)
    X1 = matrix(rt(30*d,1,delta[i]),ncol = d)
    X2 = matrix(rt(30*d,1),ncol = d)
    X3 = matrix(rt(30*d,1),ncol = d)
    MMCM_MCM = CMPermTest(list(X1,X2,X3),500)
    Pvals[i+length(delta)] = MMCM_MCM[1]
    Pvals[i] = MMCM_MCM[2]
  }
  return(Pvals)
}
PermPvalCMt = matrix(unlist(parallel::mclapply(seq(1,replic), getPval, mc.cores = num_cores)),ncol = 2*length(delta),byrow = TRUE)

getPval = function(id) {
  Pvals = rep(NA,length(delta))
  for (i in 1:length(delta)) {
    set.seed(id)
    X1 = matrix(rt(100*d,1,delta[i]),ncol = d)
    X2 = matrix(rt(100*d,1),ncol = d)
    X3 = matrix(rt(100*d,1),ncol = d)
    Pvals[i] = disco(rbind(X1,X2,X3), gl(3,100), distance=FALSE,R=500)$p.value
  }
  return(Pvals)
}
PermPvaldiscot = matrix(unlist(parallel::mclapply(seq(1,replic), getPval, mc.cores = num_cores)),ncol = length(delta),byrow = TRUE)

getPval = function(id) {
  Pvals = rep(NA,length(delta))
  for (i in 1:length(delta)) {
    set.seed(id)
    X1 = matrix(rt(100*d,1,delta[i]),ncol = d)
    X2 = matrix(rt(100*d,1),ncol = d)
    X3 = matrix(rt(100*d,1),ncol = d)
    Pvals[i] = summary(manova(rbind(X1,X2,X3)~as.factor(c(rep(1,100),rep(2,100),rep(3,100)))),test="Pillai")$stats[1,"Pr(>F)"]
  }
  return(Pvals)
}
PermPvalPillait = matrix(unlist(parallel::mclapply(seq(1,replic), getPval, mc.cores = num_cores)),ncol = length(delta),byrow = TRUE)

getPval = function(id) {
  Pvals = rep(NA,length(delta))
  for (i in 1:length(delta)) {
    set.seed(id)
    X1 = matrix(rt(100*d,1,delta[i]),ncol = d)
    X2 = matrix(rt(100*d,1),ncol = d)
    X3 = matrix(rt(100*d,1),ncol = d)
    Pvals[i] = summary(manova(rbind(X1,X2,X3)~as.factor(c(rep(1,100),rep(2,100),rep(3,100)))),test="Roy")$stats[1,"Pr(>F)"]
  }
  return(Pvals)
}
PermPvalRoyt = matrix(unlist(parallel::mclapply(seq(1,replic), getPval, mc.cores = num_cores)),ncol = length(delta),byrow = TRUE)

df3 = data.frame(noncentrality_parameter = rep(delta,6),
                 Method = c(rep("KMD",length(delta)),rep("MCM",length(delta)),rep("MMCM",length(delta)),rep("DISCO",length(delta)),rep("Pillai",length(delta)),rep("Roy",length(delta))),
                 Power = c(apply(PermPval30NNt<=0.05,2,mean),apply(PermPvalCMt[,1:length(delta)]<=0.05,2,mean),apply(PermPvalCMt[,(length(delta)+1):(2*length(delta))]<=0.05,2,mean),apply(PermPvaldiscot<=0.05,2,mean),apply(PermPvalPillait<=0.05,2,mean),apply(PermPvalRoyt<=0.05,2,mean)),
                 task = "Student t(1) distribution")

p3 = ggplot(df3, aes(x=noncentrality_parameter, y=Power, group=Method,colour=Method)) +
  geom_point() + geom_line() + scale_colour_manual(values=cbp1[-3])  +
  theme(legend.position="bottom") +guides(color=guide_legend(nrow=2,byrow=TRUE))+ 
  ggtitle("t distribution location")

# U-shaped
delta = 1+0.05*(0:8)
U_shaped_generator = function(n) {
  sample_identity = base::sample(1:3,size = n, replace = TRUE, prob = c(0.5,0.25,0.25))
  X = matrix(NA, nrow = n, ncol = 2)
  if (sum(sample_identity == 1) > 0) {
    X[sample_identity == 1,] = MASS::mvrnorm(sum(sample_identity == 1),c(0,0),diag(c(2,1/8)))
  }
  if (sum(sample_identity == 2) > 0) {
    X[sample_identity == 2,] = MASS::mvrnorm(sum(sample_identity == 2),c(-3,1),matrix(c(1/2,-1/3,-1/3,1/2),2,2))
  }
  if (sum(sample_identity == 3) > 0) {
    X[sample_identity == 3,] = MASS::mvrnorm(sum(sample_identity == 3),c(3,1),matrix(c(1/2,1/3,1/3,1/2),2,2))
  }
  return(X)
}

getPval = function(id) {
  Pvals = rep(NA,length(delta))
  for (i in 1:length(delta)) {
    set.seed(id)
    X1 = U_shaped_generator(100)
    X2 = U_shaped_generator(100)
    X3 = U_shaped_generator(100)
    X3 = X3*delta[i]
    Pvals[i] = KMD::KMD_test(X=rbind(X1,X2,X3), Y=rep(1:3,each=100), M = 3, Knn = 30)
  }
  return(Pvals)
}
PermPval30NNU = matrix(unlist(parallel::mclapply(seq(1,replic), getPval, mc.cores = num_cores)),ncol = length(delta),byrow = TRUE)

getPval = function(id) {
  Pvals = rep(NA,2*length(delta))
  for (i in 1:length(delta)) {
    set.seed(id)
    X1 = U_shaped_generator(100)
    X2 = U_shaped_generator(100)
    X3 = U_shaped_generator(100)
    X3 = X3*delta[i]
    MMCM_MCM = CMPermTest(list(X1,X2,X3),500)
    Pvals[i+length(delta)] = MMCM_MCM[1]
    Pvals[i] = MMCM_MCM[2]
  }
  return(Pvals)
}
PermPvalCMU = matrix(unlist(parallel::mclapply(seq(1,replic), getPval, mc.cores = num_cores)),ncol = 2*length(delta),byrow = TRUE)

getPval = function(id) {
  Pvals = rep(NA,length(delta))
  for (i in 1:length(delta)) {
    set.seed(id)
    X1 = U_shaped_generator(100)
    X2 = U_shaped_generator(100)
    X3 = U_shaped_generator(100)
    X3 = X3*delta[i]
    Pvals[i] = disco(rbind(X1,X2,X3), gl(3,100), distance=FALSE,R=500)$p.value
  }
  return(Pvals)
}
PermPvaldiscoU = matrix(unlist(parallel::mclapply(seq(1,replic), getPval, mc.cores = num_cores)),ncol = length(delta),byrow = TRUE)

getPval = function(id) {
  Pvals = rep(NA,length(delta))
  for (i in 1:length(delta)) {
    set.seed(id)
    X1 = U_shaped_generator(100)
    X2 = U_shaped_generator(100)
    X3 = U_shaped_generator(100)
    X3 = X3*delta[i]
    Pvals[i] = summary(manova(rbind(X1,X2,X3)~as.factor(c(rep(1,100),rep(2,100),rep(3,100)))),test="Pillai")$stats[1,"Pr(>F)"]
  }
  return(Pvals)
}
PermPvalPillaiU = matrix(unlist(parallel::mclapply(seq(1,replic), getPval, mc.cores = num_cores)),ncol = length(delta),byrow = TRUE)

getPval = function(id) {
  Pvals = rep(NA,length(delta))
  for (i in 1:length(delta)) {
    set.seed(id)
    X1 = U_shaped_generator(100)
    X2 = U_shaped_generator(100)
    X3 = U_shaped_generator(100)
    X3 = X3*delta[i]
    Pvals[i] = summary(manova(rbind(X1,X2,X3)~as.factor(c(rep(1,100),rep(2,100),rep(3,100)))),test="Roy")$stats[1,"Pr(>F)"]
  }
  return(Pvals)
}
PermPvalRoyU = matrix(unlist(parallel::mclapply(seq(1,replic), getPval, mc.cores = num_cores)),ncol = length(delta),byrow = TRUE)

df4 = data.frame(scaling_parameter = rep(delta,6),
                 Method = c(rep("KMD",length(delta)),rep("Pillai",length(delta)),rep("Roy",length(delta)),rep("MCM",length(delta)),rep("MMCM",length(delta)),rep("DISCO",length(delta))),
                 Power = c(apply(PermPval30NNU<=0.05,2,mean),apply(PermPvalPillaiU<=0.05,2,mean),apply(PermPvalRoyU<=0.05,2,mean),apply(PermPvalCMU[,1:length(delta)]<=0.05,2,mean),apply(PermPvalCMU[,(length(delta)+1):(2*length(delta))]<=0.05,2,mean),apply(PermPvaldiscoU<=0.05,2,mean)))
p4 = ggplot(df4, aes(x=scaling_parameter, y=Power, group=Method,colour=Method)) +
  geom_point() + geom_line() + scale_colour_manual(values=cbp1[-3]) +
  theme(legend.position="none") + ggtitle("U shaped scale")

# S_shaped
delta = 0.01*(0:10)*pi
S_shaped_generator = function(n) {
  sample_identity = base::sample(1:3,size = n, replace = TRUE, prob = c(1/3,1/3,1/3))
  X = matrix(NA, nrow = n, ncol = 2)
  if (sum(sample_identity == 1) > 0) {
    X[sample_identity == 1,] = MASS::mvrnorm(sum(sample_identity == 1),c(-9/2,-1/2),matrix(c(3/2,-sqrt(3/8),-sqrt(3/8),1),2,2))
  }
  if (sum(sample_identity == 2) > 0) {
    X[sample_identity == 2,] = MASS::mvrnorm(sum(sample_identity == 2),c(0,-1/2),matrix(c(3/2,sqrt(3/8),sqrt(3/8),1),2,2))
  }
  if (sum(sample_identity == 3) > 0) {
    X[sample_identity == 3,] = MASS::mvrnorm(sum(sample_identity == 3),c(9/2,1),matrix(c(3/2,-sqrt(3/8),-sqrt(3/8),1),2,2))
  }
  return(X)
}
getPval = function(id) {
  Pvals = rep(NA,length(delta))
  for (i in 1:length(delta)) {
    set.seed(id)
    X1 = S_shaped_generator(100)
    X2 = S_shaped_generator(100)
    X3 = S_shaped_generator(100)
    X3 = X3%*%matrix(c(cos(delta[i]),sin(delta[i]),-sin(delta[i]),cos(delta[i])),2,2)
    Pvals[i] = KMD::KMD_test(X=rbind(X1,X2,X3), Y=rep(1:3,each=100), M = 3, Knn = 30)
  }
  return(Pvals)
}
PermPval30NNS_shaped = matrix(unlist(parallel::mclapply(seq(1,replic), getPval, mc.cores = num_cores)),ncol = length(delta),byrow = TRUE)

getPval = function(id) {
  Pvals = rep(NA,2*length(delta))
  for (i in 1:length(delta)) {
    set.seed(id)
    X1 = S_shaped_generator(100)
    X2 = S_shaped_generator(100)
    X3 = S_shaped_generator(100)
    X3 = X3%*%matrix(c(cos(delta[i]),sin(delta[i]),-sin(delta[i]),cos(delta[i])),2,2)
    MMCM_MCM = CMPermTest(list(X1,X2,X3),500)
    Pvals[i+length(delta)] = MMCM_MCM[1]
    Pvals[i] = MMCM_MCM[2]
  }
  return(Pvals)
}
PermPvalCMS_shaped = matrix(unlist(parallel::mclapply(seq(1,replic), getPval, mc.cores = num_cores)),ncol = 2*length(delta),byrow = TRUE)

getPval = function(id) {
  Pvals = rep(NA,length(delta))
  for (i in 1:length(delta)) {
    set.seed(id)
    X1 = S_shaped_generator(100)
    X2 = S_shaped_generator(100)
    X3 = S_shaped_generator(100)
    X3 = X3%*%matrix(c(cos(delta[i]),sin(delta[i]),-sin(delta[i]),cos(delta[i])),2,2)
    Pvals[i] = disco(rbind(X1,X2,X3), gl(3,100), distance=FALSE,R=500)$p.value
  }
  return(Pvals)
}
PermPvaldiscoS_shaped = matrix(unlist(parallel::mclapply(seq(1,replic), getPval, mc.cores = num_cores)),ncol = length(delta),byrow = TRUE)

getPval = function(id) {
  Pvals = rep(NA,length(delta))
  for (i in 1:length(delta)) {
    set.seed(id)
    X1 = S_shaped_generator(100)
    X2 = S_shaped_generator(100)
    X3 = S_shaped_generator(100)
    X3 = X3%*%matrix(c(cos(delta[i]),sin(delta[i]),-sin(delta[i]),cos(delta[i])),2,2)
    Pvals[i] = summary(manova(rbind(X1,X2,X3)~as.factor(c(rep(1,100),rep(2,100),rep(3,100)))),test="Pillai")$stats[1,"Pr(>F)"]
  }
  return(Pvals)
}
PermPvalPillaiS_shaped = matrix(unlist(parallel::mclapply(seq(1,replic), getPval, mc.cores = num_cores)),ncol = length(delta),byrow = TRUE)

getPval = function(id) {
  Pvals = rep(NA,length(delta))
  for (i in 1:length(delta)) {
    set.seed(id)
    X1 = S_shaped_generator(100)
    X2 = S_shaped_generator(100)
    X3 = S_shaped_generator(100)
    X3 = X3%*%matrix(c(cos(delta[i]),sin(delta[i]),-sin(delta[i]),cos(delta[i])),2,2)
    Pvals[i] = summary(manova(rbind(X1,X2,X3)~as.factor(c(rep(1,100),rep(2,100),rep(3,100)))),test="Roy")$stats[1,"Pr(>F)"]
  }
  return(Pvals)
}
PermPvalRoyS_shaped = matrix(unlist(parallel::mclapply(seq(1,replic), getPval, mc.cores = num_cores)),ncol = length(delta),byrow = TRUE)

df5 = data.frame(rotation_angle = rep(delta,6),
                 Method = c(rep("KMD",length(delta)),rep("Pillai",length(delta)),rep("MCM",length(delta)),rep("MMCM",length(delta)),rep("DISCO",length(delta)),rep("Roy",length(delta))),
                 Power = c(apply(PermPval30NNS_shaped<=0.05,2,mean),apply(PermPvalPillaiS_shaped<=0.05,2,mean),apply(PermPvalCMS_shaped[,1:length(dims)]<=0.05,2,mean),apply(PermPvalCMS_shaped[,(length(dims)+1):(2*length(dims))]<=0.05,2,mean),apply(PermPvaldiscoS_shaped<=0.05,2,mean),apply(PermPvalRoyS_shaped<=0.05,2,mean)),
                 task = "Permutation Test")
p5 = ggplot(df5, aes(x=rotation_angle, y=Power, group=Method,colour=Method)) +
  geom_point() + geom_line() + scale_colour_manual(values=cbp1[-3]) +
  theme(legend.position="none") +
  ggtitle("S shaped rotation")


# spherical uniform
delta = 0.05*(0:7)
unif_sphere_generator = function(n,a,b) {
  X = matrix(rnorm(n*3), nrow = n, ncol = 3, byrow = TRUE)
  X = X/matrix(sqrt(apply(X^2, 1, sum))/rbeta(n,a,b), nrow = n, ncol = 3, byrow = FALSE)
  return(X)
}

getPval = function(id) {
  Pvals = rep(NA,length(delta))
  for (i in 1:length(delta)) {
    set.seed(id)
    X1 = unif_sphere_generator(100,1,1)
    X2 = unif_sphere_generator(100,1-delta[i],1+delta[i])
    X3 = unif_sphere_generator(100,1+delta[i],1-delta[i])
    Pvals[i] = KMD::KMD_test(X=rbind(X1,X2,X3), Y=rep(1:3,each=100), M = 3, Knn = 30)
  }
  return(Pvals)
}
PermPval30NNunif_sphere = matrix(unlist(parallel::mclapply(seq(1,replic), getPval, mc.cores = num_cores)),ncol = length(delta),byrow = TRUE)

getPval = function(id) {
  Pvals = rep(NA,2*length(delta))
  for (i in 1:length(delta)) {
    set.seed(id)
    X1 = unif_sphere_generator(100,1,1)
    X2 = unif_sphere_generator(100,1-delta[i],1+delta[i])
    X3 = unif_sphere_generator(100,1+delta[i],1-delta[i])
    MMCM_MCM = CMPermTest(list(X1,X2,X3),500)
    Pvals[i+length(delta)] = MMCM_MCM[1]
    Pvals[i] = MMCM_MCM[2]
  }
  return(Pvals)
}
PermPvalCMunif_sphere = matrix(unlist(parallel::mclapply(seq(1,replic), getPval, mc.cores = num_cores)),ncol = 2*length(delta),byrow = TRUE)

getPval = function(id) {
  Pvals = rep(NA,length(delta))
  for (i in 1:length(delta)) {
    set.seed(id)
    X1 = unif_sphere_generator(100,1,1)
    X2 = unif_sphere_generator(100,1-delta[i],1+delta[i])
    X3 = unif_sphere_generator(100,1+delta[i],1-delta[i])
    Pvals[i] = disco(rbind(X1,X2,X3), gl(3,100), distance=FALSE,R=500)$p.value
  }
  return(Pvals)
}
PermPvaldiscounif_sphere = matrix(unlist(parallel::mclapply(seq(1,replic), getPval, mc.cores = num_cores)),ncol = length(delta),byrow = TRUE)

getPval = function(id) {
  Pvals = rep(NA,length(delta))
  for (i in 1:length(delta)) {
    set.seed(id)
    X1 = unif_sphere_generator(100,1,1)
    X2 = unif_sphere_generator(100,1-delta[i],1+delta[i])
    X3 = unif_sphere_generator(100,1+delta[i],1-delta[i])
    Pvals[i] = summary(manova(rbind(X1,X2,X3)~as.factor(c(rep(1,100),rep(2,100),rep(3,100)))),test="Pillai")$stats[1,"Pr(>F)"]
  }
  return(Pvals)
}
PermPvalPillaiunif_sphere = matrix(unlist(parallel::mclapply(seq(1,replic), getPval, mc.cores = num_cores)),ncol = length(delta),byrow = TRUE)

getPval = function(id) {
  Pvals = rep(NA,length(delta))
  for (i in 1:length(delta)) {
    set.seed(id)
    X1 = unif_sphere_generator(100,1,1)
    X2 = unif_sphere_generator(100,1-delta[i],1+delta[i])
    X3 = unif_sphere_generator(100,1+delta[i],1-delta[i])
    Pvals[i] = summary(manova(rbind(X1,X2,X3)~as.factor(c(rep(1,100),rep(2,100),rep(3,100)))),test="Roy")$stats[1,"Pr(>F)"]
  }
  return(Pvals)
}
PermPvalRoyunif_sphere = matrix(unlist(parallel::mclapply(seq(1,replic), getPval, mc.cores = num_cores)),ncol = length(delta),byrow = TRUE)

df6 = data.frame(alpha = rep(delta,6),
                 Method = c(rep("KMD",length(delta)),rep("MCM",length(delta)),rep("MMCM",length(delta)),rep("DISCO",length(delta)),rep("Pillai",length(delta)),rep("Roy",length(delta))),
                 Power = c(apply(PermPval30NNunif_sphere<=0.05,2,mean),apply(PermPvalCMunif_sphere[,1:length(delta)]<=0.05,2,mean),apply(PermPvalCMunif_sphere[,(length(delta)+1):(2*length(delta))]<=0.05,2,mean),apply(PermPvaldiscounif_sphere<=0.05,2,mean),apply(PermPvalPillaiunif_sphere<=0.05,2,mean),apply(PermPvalRoyunif_sphere<=0.05,2,mean)))

p6 = ggplot(df6, aes(x=alpha, y=Power, group=Method,colour=Method)) +
  geom_point() + geom_line() + scale_colour_manual(values=cbp1[-3])  +
  theme(legend.position="none") +
  ggtitle("Spherical symmetric")

gridExtra::grid.arrange(p1,p2,p3,p4,p5,p6, ncol=3,heights=c(1.27,1))


