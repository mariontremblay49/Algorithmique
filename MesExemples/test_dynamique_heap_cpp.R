library(Rcpp)

# Données d'exemple
data <- data.frame(
  id = 1:10,
  score = c(10, 9, 8, 7, 6, 5, 4, 3, 2, 1),
  groupes = c("A", "A", "B", "B", "C", "A,B", "C", "A", "B", "C")
)

# Contraintes
max_par_groupe <- list(A = 2, B = 2, C = 1)

# Poids des positions
poids <- c(5, 4, 3, 2, 1)

# Résolution
result <- ranking_max_dp_heap_cpp(data, k = 5, max_par_groupe, poids_positions = poids)

print("Éléments sélectionnés:")
print(result$selected_items)
print(paste("Score total:", result$best_score))

