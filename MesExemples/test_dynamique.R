### TEST RANKING DYNAMIQUE ###

# EXEMPLE MAX

# Données d'exemple
data <- data.frame(
  id = 1:10,
  score = c(1, 9, 8, 7, 6, 5, 4, 3, 2, 10),
  groupes = c("C", "A", "B", "B", "C", "A,B", "C", "A", "B", "A")
)

# Contraintes: max 2 éléments du groupe A, 2 du groupe B, 1 du groupe C
max_par_groupe <- list(A = 2, B = 2, C = 1)

# Poids décroissants pour les positions (position 1 = poids 5, position 2 = poids 4, etc.)
poids <- c(5, 4, 3, 2, 1)

# Résolution
result <- ranking_max(data, k = 5, max_par_groupe, poids_positions = poids)

print("Éléments sélectionnés:")
print(result$selected_items)
print(paste("Score total:", result$best_score))
