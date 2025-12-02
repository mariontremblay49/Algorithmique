library(Rcpp)
Rcpp::sourceCpp("C:/Users/flavi/OneDrive/Documents/ENSIIE/Algo/testcpp.cpp")

scores <- c(9, 8, 7, 10, 6)
groupes <- c("B", "A, B", "B", "A")
groups <- c("A", "B")
max_cap <- c(2, 2)

ranking_max_cpp(scores, groupes, 2, groups, max_cap)
