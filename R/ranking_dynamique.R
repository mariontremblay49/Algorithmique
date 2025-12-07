ranking_max <- function(data, k, max_par_groupe, poids_positions = NULL) {
  #---------------------------------------------
  # DYNAMIC ALGORITHM: CONSTRAINED WEIGHTED RANKING
  #
  # Objectif:
  #   Sélectionner k éléments et les assigner aux positions 1 à k
  #   pour maximiser le score total pondéré par les positions,
  #   tout en respectant les contraintes de groupes.
  #
  # Entrées:
  #   data : data.frame contenant au moins:
  #           - colonne "score" (score s_i de chaque élément)
  #           - colonne "groupes" (groupes séparés par virgules)
  #   k : nombre de positions à remplir
  #   max_par_groupe : liste nommée donnant le max par groupe (ex: list(A=2, B=3))
  #   poids_positions : vecteur des poids w_p pour chaque position p=1..k
  #                     (par défaut: tous égaux à 1)
  #
  # Sortie:
  #   Liste contenant:
  #     - selected_items: data.frame des éléments sélectionnés avec leur position
  #     - best_score: score total optimal
  #---------------------------------------------

  # Poids par défaut si non fournis
  if (is.null(poids_positions)) {
    poids_positions <- rep(1, k)
  } else if (length(poids_positions) != k) {
    stop("Le vecteur poids_positions doit avoir exactement k éléments")
  }

  # Étape 1: Préparation des données
  data$group_list <- lapply(strsplit(data$groupes, ","), trimws)
  groups <- names(max_par_groupe)
  G <- length(groups)
  n <- nrow(data)
  group_index <- setNames(seq_len(G), groups)
  max_cap <- unlist(max_par_groupe)

  # Représentation binaire des groupes
  item_groups <- matrix(0, n, G)
  for (i in 1:n) {
    for (g in data$group_list[[i]]) {
      if (g %in% groups) {
        item_groups[i, group_index[g]] <- 1
      }
    }
  }

  # Étape 2: Programmation dynamique
  # État: (position p remplie, compteurs de groupes)
  # DP[[p+1]][[key]] = meilleur score pour avoir rempli les p premières positions
  #                    avec les compteurs de groupes représentés par key

  DP <- vector("list", k + 1)
  for (idx in seq_len(k + 1)) DP[[idx]] <- list()

  # État initial: aucune position remplie
  key0 <- paste(rep(0, G), collapse = ",")
  DP[[1]][[key0]] <- list(score = 0, items = integer(0))

  # Pour chaque position p de 1 à k
  for (p in 1:k) {
    w_p <- poids_positions[p]

    # Pour chaque état atteignable à la position p-1
    for (key_prev in names(DP[[p]])) {
      state_prev <- DP[[p]][[key_prev]]
      if (is.null(state_prev)) next
      prev_counts <- as.numeric(strsplit(key_prev, ",")[[1]])
      items_used <- state_prev$items

      # Essayer d'assigner chaque élément non encore utilisé à la position p
      for (i in 1:n) {
        # Vérifier si l'élément i n'est pas déjà utilisé
        if (i %in% items_used) next
        # Calculer les nouveaux compteurs de groupes
        grp_i <- item_groups[i, ]
        new_counts <- prev_counts + grp_i
        # Vérifier les contraintes de groupes
        if (any(new_counts > max_cap)) next

        # Calculer le nouveau score
        new_score <- state_prev$score + w_p * data$score[i]
        new_items <- c(items_used, i)

        # Créer la clé du nouvel état
        key_new <- paste(new_counts, collapse = ",")

        # Mettre à jour DP si ce chemin est meilleur
        if (is.null(DP[[p + 1]][[key_new]]) ||
            new_score > DP[[p + 1]][[key_new]]$score) {
          DP[[p + 1]][[key_new]] <- list(
            score = new_score,
            items = new_items
          )
        }
      }
    }
  }


  # Étape 3: Trouver la meilleure solution finale
  best_score <- -Inf
  best_items <- NULL

  for (key in names(DP[[k + 1]])) {
    state <- DP[[k + 1]][[key]]
    if (!is.null(state) && state$score > best_score) {
      best_score <- state$score
      best_items <- state$items
    }
  }

  # Étape 4: Construire le résultat
  if (is.null(best_items) || length(best_items) == 0) {
    warning("Aucune solution trouvée respectant les contraintes")
    return(list(
      selected_items = data.frame(),
      best_score = 0
    ))
  }

  # Créer le data.frame résultat avec les positions
  result_df <- data[best_items, , drop = FALSE]
  result_df$position <- 1:k
  result_df$poids_position <- poids_positions
  result_df$score_pondere <- poids_positions * result_df$score

  return(list(
    selected_items = result_df,
    best_score = best_score
  ))
}
