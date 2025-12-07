
ranking_naif_max <- function(data, k, max_par_groupe, poids_positions = NULL) {
  #---------------------------------------------
  # NAIVE GREEDY ALGORITHM: CONSTRAINED WEIGHTED RANKING
  #
  # Objectif:
  #   Assigner k éléments aux positions 1 à k pour maximiser le score pondéré,
  #   tout en respectant les contraintes de groupes.
  #   Approche gloutonne : à chaque position, choisir l'élément qui maximise
  #   le gain immédiat (w_p × score_i).
  #
  # Entrées:
  #   data : data.frame contenant au moins:
  #           - colonne "score"
  #           - colonne "groupes" (groupes séparés par virgules)
  #   k : nombre de positions à remplir
  #   max_par_groupe : liste nommée (ex: list(A=2, B=3, C=1))
  #   poids_positions : vecteur des poids pour chaque position (défaut: tous à 1)
  #
  # Sortie:
  #   Liste contenant:
  #     - selected_items: data.frame avec position et score pondéré
  #     - best_score: score total
  #---------------------------------------------

  # Poids par défaut si non fournis
  if (is.null(poids_positions)) {
    poids_positions <- rep(1, k)
  } else if (length(poids_positions) != k) {
    stop("Le vecteur poids_positions doit avoir exactement k éléments")
  }

  # Initialisation
  selection <- data.frame()
  indices_utilises <- integer(0)  # indices des éléments déjà assignés
  compte_groupes <- as.list(rep(0, length(max_par_groupe)))
  names(compte_groupes) <- names(max_par_groupe)
  score_total <- 0

  # Pour chaque position p de 1 à k
  for (p in 1:k) {
    w_p <- poids_positions[p]

    # Trouver les éléments candidats pour cette position
    # Un élément est candidat s'il n'a pas encore été utilisé
    # et si ses contraintes de groupes sont respectées
    meilleur_idx <- NULL
    meilleur_gain <- -Inf

    for (i in 1:nrow(data)) {
      # Vérifier si l'élément i n'est pas déjà utilisé
      if (i %in% indices_utilises) next

      # Extraire les groupes de l'élément i
      groupes_i <- trimws(unlist(strsplit(data$groupes[i], ",")))

      # Vérifier les contraintes de groupes
      respect_contraintes <- TRUE
      for (g in groupes_i) {
        if (!is.null(max_par_groupe[[g]])) {
          if (compte_groupes[[g]] >= max_par_groupe[[g]]) {
            respect_contraintes <- FALSE
            break
          }
        }
      }

      if (!respect_contraintes) next

      # Calculer le gain de cet élément à cette position
      gain_i <- w_p * data$score[i]

      # Garder le meilleur
      if (gain_i > meilleur_gain) {
        meilleur_gain <- gain_i
        meilleur_idx <- i
      }
    }

    # Si aucun élément n'est sélectionnable, arrêter
    if (is.null(meilleur_idx)) {
      warning(paste("Impossible de remplir toutes les", k, "positions.",
                    "Seulement", p-1, "positions remplies."))
      break
    }

    # Ajouter l'élément sélectionné
    choix <- data[meilleur_idx, , drop = FALSE]
    choix$position <- p
    choix$poids_position <- w_p
    choix$score_pondere <- meilleur_gain

    selection <- rbind(selection, choix)
    indices_utilises <- c(indices_utilises, meilleur_idx)
    score_total <- score_total + meilleur_gain

    # Mettre à jour les compteurs de groupes
    groupes_choix <- trimws(unlist(strsplit(choix$groupes, ",")))
    for (g in groupes_choix) {
      if (!is.null(compte_groupes[[g]])) {
        compte_groupes[[g]] <- compte_groupes[[g]] + 1
      }
    }
  }

  # Nettoyer les noms de lignes
  rownames(selection) <- NULL

  return(list(
    selected_items = selection,
    best_score = score_total
  ))
}
