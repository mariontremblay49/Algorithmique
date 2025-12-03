library(Rcpp)


# EXEMPLE MAX
# Exemple de données
data <- data.frame(
  id = 1:5,
  score = c(9, 6, 8, 7, 10),
  groupes = c("B", "A", "A,B", "B", "A"),
  stringsAsFactors = FALSE
)

# Contraintes
k <- 3
max_par_groupe <- list(A = 2, B = 2)

# Test Max
res <- ranking_max_cpp(data, k, max_par_groupe)
print(res)
