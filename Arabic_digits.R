load("/Users/huangzhen/desktop/Twosamples/rcode/ArabicDigits.rda")

MFCCs = array(0,c(93,8800,13))
for (d in 1:13) {
  MFCCs[,,d] = ArabicDigits[[d]]
}

# compute the DTW distance between the ith and jth data point in the Arabic digits data set
# i, j should be in 1,...,8800
# dims is a subset of 1,...,13
DTW_arabic = function(i, j, dims = 1:13) {
  myfun <- function(k, l) crossprod(MFCCs[k,i,dims] - MFCCs[l,j,dims])
  myfun = Vectorize(myfun)
  return(dtw::dtw(outer(1:93,1:93,myfun), distance.only = TRUE)$distance)
}


# The following code computes the pairwise distance matrix
# It takes around 32 hours to run on a server

#Dist = matrix(0, 8800, 8800)
#for (i in 1:8800) {
#  start_time <- Sys.time()
#  compute_DTW = function(j) return(DTW_arabic(i,j))
#  Dist[i,1:(i-1)] = unlist(parallel::mclapply(1:(i-1), compute_DTW, mc.cores = 24))
#  Dist[1:(i-1),i] = Dist[i,1:(i-1)]
#}

# Here we directly used the saved result of the above computation
load("~/Downloads/pairwise_dist_full.RData")
ArabicDigits[[14]] = as.numeric(ArabicDigits[[14]]) - 1

# It takes approximately 5 minutes on a personal laptop
KMD::KMD(pairwise_dist_full, ArabicDigits[[14]] + 1, M = 10, Knn = 1, Kernel = "discrete")
# 0.9976013


# The following code computes the pairwise distance matrix, based on the 13th MFCC alone
# It takes around 27 hours to run on a server

#feature_num = 13
#Dist = matrix(0, 8800, 8800)
#for (i in 2:8800) {
#  compute_DTW = function(j) return(DTW_arabic(i,j,feature_num))
#  Dist[i,1:(i-1)] = unlist(parallel::mclapply(1:(i-1), compute_DTW, mc.cores = 24))
#  Dist[1:(i-1),i] = Dist[i,1:(i-1)]
#}

# Here we directly used the saved result of the above computation
load(paste("~/Downloads/feature13.RData", sep = ""))

# Indices of males speaking 1
one_male = ((1:8800)[ArabicDigits[[14]] == 1])[1:440]
# Data from 44 males and 44 females native Arabic speakers.
# The number of the time series is 8800 (10 digits x 10 repetitions x 88 speakers) in total.
# First half of each class contains male speakers whereas the second half contains female speakers.

# Indices of males speaking 2
two_male = ((1:8800)[ArabicDigits[[14]] == 2])[1:440]
# Indices of females speaking 2
two_female = ((1:8800)[ArabicDigits[[14]] == 2])[441:880]

print(c(
  KMD::KMD(Dist[c(one_male,two_male),c(one_male,two_male)],
           ArabicDigits[[14]][c(one_male,two_male)],Knn = 1),
  KMD::KMD(Dist[c(one_male,two_female),c(one_male,two_female)],
           ArabicDigits[[14]][c(one_male,two_female)],Knn = 1)
))
# 0.5686725 0.6867200




# Plot
MFCCs = c()
MFCC = c()
for (i in 1:13) {
  MFCCs = c(MFCCs,ArabicDigits[[i]][,which(ArabicDigits$class == 1)[441]])
  MFCC = c(MFCC, rep(as.character(i),93))
}
Female1_1 = data.frame(Time = rep(1:93,13),
                       MFCCs = MFCCs,
                       MFCC = MFCC,
                       task = "Female 1 speaking 1")

MFCCs = c()
MFCC = c()
for (i in 1:13) {
  MFCCs = c(MFCCs,ArabicDigits[[i]][,which(ArabicDigits$class == 1)[451]])
  MFCC = c(MFCC, rep(as.character(i),93))
}
Female2_1 = data.frame(Time = rep(1:93,13),
                       MFCCs = MFCCs,
                       MFCC = MFCC,
                       task = "Female 2 speaking 1")

MFCCs = c()
MFCC = c()
for (i in 1:13) {
  MFCCs = c(MFCCs,ArabicDigits[[i]][,which(ArabicDigits$class == 1)[1]])
  MFCC = c(MFCC, rep(as.character(i),93))
}
Male1_1 = data.frame(Time = rep(1:93,13),
                     MFCCs = MFCCs,
                     MFCC = MFCC,
                     task = "Male 1 speaking 1")

MFCCs = c()
MFCC = c()
for (i in 1:13) {
  MFCCs = c(MFCCs,ArabicDigits[[i]][,which(ArabicDigits$class == 2)[1]])
  MFCC = c(MFCC, rep(as.character(i),93))
}
Male1_2 = data.frame(Time = rep(1:93,13),
                     MFCCs = MFCCs,
                     MFCC = MFCC,
                     task = "Male 1 speaking 2")

library(ggplot2)
df = rbind(Male1_1,Male1_2,Female1_1,Female2_1)
a = c()
for (i in 1:13) {
  a = c(a,as.character(i))
}
df$MFCC = factor(df$MFCC, levels = a)
ggplot(df, aes(x=Time, y=MFCCs, group=MFCC,colour=MFCC)) +
  geom_line(aes(color = MFCC, linetype = MFCC)) +
  facet_wrap(vars(task),nrow = 1) + theme(legend.position = "bottom",axis.ticks.x = element_blank(),axis.text.x = element_blank(),
                                          legend.margin=margin(0,0,0,0),
                                          legend.box.margin=margin(-10,-5,-5,-5)) +
  guides(colour = guide_legend(nrow = 1))
