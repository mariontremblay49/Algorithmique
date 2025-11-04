## test ranking naif (max)
# Exemple de données
items <- data.frame(
  id = 1:8,
  score = c(95, 90, 88, 85, 80, 78, 75, 70),
  groupes = c("A,B", "A", "B,C", "B", "A", "A,C", "C", "B")
)

# Contraintes
k <- 5
max_par_groupe <- list(A = 2, B = 3, C = 2)

# Exécution
resultat <- ranking_naif_max(items, k, max_par_groupe)
print(resultat)


## test ranking naif (min)
min_par_groupe <- list(A = 2, B = 2, C = 1)

resultat <- ranking_naif_min(items, k, min_par_groupe)
print(resultat)
