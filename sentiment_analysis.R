library(stringr)
library(text2vec)
data("movie_review")

prep_fun = function(x) {
  # make text lower case
  x = str_to_lower(x)
  # remove non-alphanumeric symbols
  x = str_replace_all(x, "[^[:alnum:]]", " ")
  # collapse multiple spaces
  str_replace_all(x, "\\s+", " ")
}
movie_review$review_clean = prep_fun(movie_review$review)

set.seed(1)
it = itoken(movie_review$review_clean, progressbar = FALSE)
v = create_vocabulary(it)
v = prune_vocabulary(v, doc_proportion_max = 0.1, term_count_min = 5)
vectorizer = vocab_vectorizer(v)

dtm = create_dtm(it, vectorizer)

jac_dist = dist2(x = dtm, y = NULL, method = "jaccard", norm = "none")
KMD::KMD(jac_dist, movie_review$sentiment + 1, M = 2, Knn = 1)
# 0.4440855

folds <- splitTools::create_folds(as.factor(movie_review$sentiment), k = 10)
err = c()
for (i in 1:10) {
  model <- e1071::svm(dtm[folds[[i]],], as.factor(movie_review$sentiment[folds[[i]]]), kernel = "linear")
  err = c(err, mean(predict(model, dtm[-folds[[i]],]) != movie_review$sentiment[-folds[[i]]]))
  print(err[i])
}
# cross-validated accuracy
print(1-mean(err))
# 0.8191872