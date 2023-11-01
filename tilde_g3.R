# Produce Figure 8 in Appendix D.4 that shows the converges of \tilde{g}_3

# Function that returns an n by Knn matrix showing the indices of KNN
# (Ties broken at random)
get_neighbors = function(X,Knn) {
  if (!is.matrix(X)) X = as.matrix(X)
  # compute the nearest neighbor of X
  nn_X = RANN::nn2(X, query = X, k = Knn + 2)
  nn_index_X = nn_X$nn.idx[, 2:(Knn+1), drop=F]
  
  # find all data points that are not unique
  repeat_data = which(nn_X$nn.dists[, 2] == 0)
  if (length(repeat_data) > 0) {
    gp_size = id = NULL
    df_X = data.table::data.table(id = repeat_data, group = nn_X$nn.idx[repeat_data, 1])
    df_X[, gp_size := length(id), by = "group"]
    
    for (i in 1:length(repeat_data)) {
      if (df_X$gp_size[i] > Knn) {
        # The number of repeated data is more than Knn
        group_indices = df_X$id[df_X$group==df_X$group[i]]
        if (Knn == 1 & length(group_indices) == 2) {
          nn_index_X[df_X$id[i],] = setdiff(group_indices, df_X$id[i]) # avoid length 1 vector in sample function
        }
        else {
          nn_index_X[df_X$id[i],] = sample(setdiff(group_indices, df_X$id[i]),Knn)
        }
      }
      else {
        if (nn_X$nn.dists[df_X$id[i], Knn+1] < nn_X$nn.dists[df_X$id[i], Knn+2]) {
          # The number of repeated data is less than Knn
          # but there is no tie at the KNN
          nn_index_X[df_X$id[i],] = setdiff(nn_X$nn.idx[df_X$id[i], 1:(Knn+1)], df_X$id[i])
        }
        else {
          # The number of repeated data is less than Knn
          # There are ties at the kNN
          distances <- proxy::dist(matrix(X[df_X$id[i], ], ncol = ncol(X)), matrix(X[-df_X$id[i], ], ncol = ncol(X)))
          tie_dist <- sort(distances, partial = Knn)[Knn]
          id_small <- which(distances < tie_dist)
          id_small = id_small + (id_small >= df_X$id[i])
          nn_index_X[df_X$id[i],1:length(id_small)] = id_small
          id_equal = sample(which(distances == tie_dist),Knn-length(id_small))
          id_equal = id_equal + (id_equal >= df_X$id[i])
          nn_index_X[df_X$id[i],(1+length(id_small)):Knn] = id_equal
        }
      }
    }
  }
  ties = which(nn_X$nn.dists[, Knn+1] == nn_X$nn.dists[, Knn+2])
  ties = setdiff(ties, repeat_data)
  if (length(ties) > 0) {
    for (i in ties) {
      distances <- proxy::dist(matrix(X[i, ], ncol = ncol(X)), matrix(X[-i, ], ncol = ncol(X)))
      tie_dist <- sort(distances, partial = Knn)[Knn]
      id_small <- which(distances < tie_dist)
      if (length(id_small) > 0) {
        id_small = id_small + (id_small >= i)
        nn_index_X[i,1:length(id_small)] = id_small
      }
      id_equal = sample(which(distances == tie_dist),Knn-length(id_small))
      id_equal = id_equal + (id_equal >= i)
      nn_index_X[i,(1+length(id_small)):Knn] = id_equal
    }
  }
  return(nn_index_X)
}


# Compute g_3_tilde with Euclidean k-NN graph
g_3_tilde = function(X, Y, M = length(unique(Y)), Knn = ceiling(length(Y)/10)) {
  if (!is.matrix(X)) X = as.matrix(X)
  n = dim(X)[1]
  nn_index_X = get_neighbors(X,Knn)
  
  num_in_neighbors = rep(0,n)
  g3 = 0
  for (i in 1:n) {
    for (j in nn_index_X[i,]) {
      num_in_neighbors[j] = num_in_neighbors[j] + 1
      if (i %in% nn_index_X[j,]) g3 = g3 + 1
    }
  }
  g3 = g3/n/Knn^2
  return(g3)
}

par(mfrow = c(1, 2))
# d = 1
set.seed(1)
X = rnorm(2000)
Y = sample(3,2000,replace = TRUE)
a = c()
# Compute \tilde{g}_3 with the first i data points
for (i in 10:2000) {
  a = c(a,g_3_tilde(X[1:i],Y[1:i],3,1))
}
plot(10:2000,a,type = "l",xlab = "n",ylab = expression(tilde(g)[3]), main = "d = 1")
abline(h = 2/3,col="red")


# d = 5
set.seed(1)
d = 5
X = matrix(rnorm(20^5*d),ncol = d)
Y = sample(3,20^5,replace = TRUE)
n_d = seq(4,20,by=0.1)
n = round(n_d^5)
a5 = c()
for (i in n) {
  a5 = c(a5,g_3_tilde(X[1:i,],Y[1:i],3,1))
  print(i^(1/5))
}

# This function computes the probability of mutual nearest neighbor
# for a homogeneous Poisson process on R^j (Henze, N. 1986)
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

plot(n_d,a5,type = "l",xlab = expression(n^"1/5"),ylab = expression(tilde(g)[3]),ylim = c(min(a5),mutual_prob(5)),main = "d = 5")
abline(h = mutual_prob(5),col="red")
