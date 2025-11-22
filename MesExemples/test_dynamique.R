### TEST RANKING DYNAMIQUE ###

# EXEMPLE MAX
# Exemple de données
data <- data.frame(
  id = 1:5,
  score = c(10, 9, 8, 7, 6),
  groupes = c("A", "B", "A,B", "B", "A"),
  stringsAsFactors = FALSE
)

# Contraintes
k <- 3
max_par_groupe <- list(A = 2, B = 2)

# Test Max
res <- ranking_max(data, k, max_par_groupe)
print(res)

#####################################################
# EXEMPLE MIN
data <- data.frame(
  id = 1:5,
  score = c(10, 9, 8, 7, 6),
  groupes = c("A", "B", "A,B", "B", "A"),
  stringsAsFactors = FALSE
)

# Contraintes
k <- 4
min_par_groupe <- list(A = 2, B = 1)

# Test Min
ranking_min(data, k, min_par_groupe)

####################################################
# EXEMPLE MIN AND MAX

# Exemple de données
data <- data.frame(
  id = 1:6,
  score = c(10, 9, 8, 7, 6, 5),
  groupes = c("A", "B", "A,B", "B", "A", "A"),
  stringsAsFactors = FALSE
)

# Contraintes
k <- 4
min_par_groupe <- list(A = 2, B = 1)
max_par_groupe <- list(A = 3, B = 2)

# Test max and min
ranking_minmax(data, k, min_par_groupe, max_par_groupe)
