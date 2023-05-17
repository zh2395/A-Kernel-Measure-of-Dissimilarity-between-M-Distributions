# Amazon commerce reviews
Amazon = read.csv("/Users/huangzhen/desktop/Twosamples/rcode/Amazon.csv", header = TRUE)
Reviewer4 = Amazon[(3*30 + 1):(4*30),2:10001]
Reviewer8 = Amazon[(7*30 + 1):(8*30),2:10001]
Reviewer12 = Amazon[(11*30 + 1):(12*30),2:10001]
# different reviewer
set.seed(1)
KMD::KMD(X = rbind(Reviewer4[1:15,],Reviewer8,Reviewer12[1:15,]), Y = c(rep(1,15),rep(2,30),rep(3,15)))
KMD::KMD_test(X = rbind(Reviewer4[1:15,],Reviewer8,Reviewer12[1:15,]), Y = c(rep(1,15),rep(2,30),rep(3,15)), Knn = 1)
# eta = 0.6066667 p-value = 0.001996008

# same reviewer
set.seed(1)
KMD::KMD(X = Reviewer4, Y = c(rep(1,15),rep(2,15)))
KMD::KMD_test(X = Reviewer4, Y = c(rep(1,15),rep(2,15)),Knn = 1)
# 0.03333333 0.502994

# mixture
set.seed(1)
KMD::KMD(X = rbind(Reviewer4,Reviewer8), Y = c(rep(1,15),rep(2,15),rep(3,30)), M = 3, Knn = 1)
KMD::KMD_test(X = rbind(Reviewer4,Reviewer8), Y = c(rep(1,15),rep(2,15),rep(3,30)), M = 3, Knn = 1)
# 0.4231111 0.001996008





# Semeion Handwritten Digits
Digit = read.table("/Users/huangzhen/desktop/Twosamples/rcode/semeion.txt")
Digit6 = Digit[Digit$V263 == 1,1:256]
Digit8 = Digit[Digit$V265 == 1,1:256]
Digit7 = Digit[Digit$V264 == 1,1:256]
set.seed(1)
# different digits
KMD::KMD(X = rbind(Digit6[1:81,],Digit7[1:80,],Digit8), Y = c(rep(1,81),rep(2,80),rep(3,155)), M = 3, Knn = 1)
KMD::KMD_test(X = rbind(Digit6[1:81,],Digit7[1:80,],Digit8), Y = c(rep(1,81),rep(2,80),rep(3,155)), M = 3, Knn = 1)
# 0.9749483 0.001996008

# same digit
set.seed(1)
KMD::KMD(X = Digit6, Y = c(rep(1,81),rep(2,80)), M = 2, Knn = 1)
KMD::KMD_test(X = Digit6, Y = c(rep(1,81),rep(2,80)), M = 2, Knn = 1)
# 0.09876543 0.1576846

# mixture
set.seed(1)
KMD::KMD(X = rbind(Digit6,Digit8), Y = c(rep(1,81),rep(2,80),rep(3,155)), M = 3, Knn = 1)
KMD::KMD_test(X = rbind(Digit6,Digit8), Y = c(rep(1,81),rep(2,80),rep(3,155)), M = 3, Knn = 1)
# 0.6041832 0.001996008


# isolet
isolet = rbind(read.table("/Users/huangzhen/desktop/Twosamples/rcode/isolet1+2+3+4.txt",sep = ","),
               read.table("/Users/huangzhen/desktop/Twosamples/rcode/isolet5.txt",sep = ","))
dim(isolet)
letter7 = isolet[isolet$V618 == 7,1:617]
letter14 = isolet[isolet$V618 == 14,1:617]
letter20 = isolet[isolet$V618 == 20,1:617]
set.seed(1)
# different letters
KMD::KMD(X = rbind(letter7[1:150,],letter14[1:150,],letter20), Y = c(rep(1,150),rep(2,150),rep(3,300)), M = 3, Knn = 1)
KMD::KMD_test(X = rbind(letter7[1:150,],letter14[1:150,],letter20), Y = c(rep(1,150),rep(2,150),rep(3,300)), M = 3, Knn = 1)
# 0.8855244 0.001996008

# same letter
set.seed(1)
KMD::KMD(X = letter7, Y = c(rep(1,150),rep(2,150)), M = 2, Knn = 1)
KMD::KMD_test(X = letter7, Y = c(rep(1,150),rep(2,150)), M = 2, Knn = 1)
# 0.3820667 0.001996008

# mixture
set.seed(1)
KMD::KMD(X = rbind(letter7,letter20), Y = c(rep(1,150),rep(2,150),rep(3,300)), M = 3, Knn = 1)
KMD::KMD_test(X = rbind(letter7,letter20), Y = c(rep(1,150),rep(2,150),rep(3,300)), M = 3, Knn = 1)
# 0.6379378 0.001996008


# lrs
lrs = read.delim("/Users/huangzhen/desktop/Twosamples/rcode/lrs.txt", header = FALSE, sep=c(" ","\n"))
row_index = (1:(dim(lrs)[1]))[is.na(lrs$V1)]
lrs_matrix = matrix(NA, nrow = 531, ncol = 93 + 1)
for (i in 1:length(row_index)) {
  if (i == length(row_index)) {
    row_i = c(t(lrs[(row_index[i]):(dim(lrs)[1]),]))
  }
  else {
    row_i = c(t(lrs[(row_index[i]):(row_index[i+1]-1),]))
  }
  row_i = row_i[!is.na(row_i)]
  lrs_matrix[i,1:93] = row_i[11:103]
  lrs_matrix[i,94] = row_i[2]
}
lrs = matrix(as.numeric(lrs_matrix), nrow = 531)
class1 = lrs[(9 < lrs[,94]) & (lrs[,94] < 20),1:93]
class2 = lrs[(19 < lrs[,94]) & (lrs[,94] < 30),1:93]
class4 = lrs[(39 < lrs[,94]) & (lrs[,94] < 50),1:93]
dim(class1)



set.seed(1)
# different classes
KMD::KMD(X = rbind(class4,class2[97:273,],class1[-c(28,71),]), Y = c(rep(1,96),rep(2,177),rep(3,88)), M = 3, Knn = 1)
KMD::KMD_test(X = rbind(class4,class2[97:273,],class1[-c(28,71),]), Y = c(rep(1,96),rep(2,177),rep(3,88)), M = 3, Knn = 1)
# 0.8683441 0.001996008

# same class
set.seed(1)
KMD::KMD(X = class2, Y = c(rep(1,96),rep(2,177)), M = 2, Knn = 1)
KMD::KMD_test(X = class2, Y = c(rep(1,96),rep(2,177)), M = 2, Knn = 1)
# 0.1676083 0.01197605
# (random selection)
set.seed(1)
a = sample(1:273,96)
b = setdiff(1:273,a)
KMD::KMD_test(X = class2[c(a,b),], Y = c(rep(1,96),rep(2,177)), M = 2, Knn = 1)
# p-value = 0.4690619

# mixture
set.seed(1)
KMD::KMD(X = rbind(class2,class1[-c(28,71),]), Y = c(rep(1,96),rep(2,177),rep(3,88)), M = 3, Knn = 1)
KMD::KMD_test(X = rbind(class2,class1[-c(28,71),]), Y = c(rep(1,96),rep(2,177),rep(3,88)), M = 3, Knn = 1)
# 0.5084845 0.001996008


