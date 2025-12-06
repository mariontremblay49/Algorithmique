
ranking_naif_max <- function(data, k, max_par_groupe) {
  #---------------------------------------------
  # NAIVE GREEDY ALGORITHM: CONSTRAINED RANKING
  #
  # Goal:
  #   Select up to k elements to maximize total score,
  #   while respecting a maximum number of elements per group.
  #
  # Inputs:
  #   data : data.frame containing at least:
  #           - a "score" column
  #           - a "groupes" column (string with one or more groups separated by commas)
  #   k : maximum number of elements to select
  #   max_par_groupe : named list giving the maximum allowed per group (e.g., list(A=2, B=3, C=1))
  #
  # Output:
  #   data.frame of selected elements, sorted by descending score
  #---------------------------------------------

  # Step 1: sort elements by descending score (best first)
  data <- data[order(-data$score), ]

  # Step 2: initialize tracking structures
  selection <- data.frame()  # will contain chosen elements
  compte_groupes <- as.list(rep(0, length(max_par_groupe)))  # counter for each group
  names(compte_groupes) <- names(max_par_groupe)

  # Step 3: main loop
  # Continue until k elements are selected or no valid candidates remain
  while (nrow(selection) < k && nrow(data) > 0) {

    # Step 3.1: check for each remaining element if it is selectable
    # An element is "possible" if for all its groups,
    # the group's maximum has not been reached yet
    possibles <- sapply(1:nrow(data), function(i) {
      groupes_i <- unlist(strsplit(data$groupes[i], ","))  # extract groups of element i
      # Check the constraint for each group
      all(sapply(groupes_i, function(g) {
        # If group is not in the constraints list -> no limit (Inf)
        is.null(max_par_groupe[[g]]) || compte_groupes[[g]] < max_par_groupe[[g]]
      }))
    })

    # Step 3.2: if no element is selectable, stop
    if (!any(possibles)) break

    # Step 3.3: among possible elements, choose the one with the highest score
    # (the one that maximizes total score at this moment)
    meilleur <- which.max(ifelse(possibles, data$score, -Inf))
    choix <- data[meilleur, , drop = FALSE]

    # Step 3.4: add this element to the selection
    selection <- rbind(selection, choix)

    # Step 3.5: update counters for affected groups
    groupes_choix <- unlist(strsplit(choix$groupes, ","))
    for (g in groupes_choix) {
      if (!is.null(compte_groupes[[g]])) {
        compte_groupes[[g]] <- compte_groupes[[g]] + 1
      }
    }

    # Step 3.6: remove the chosen element from remaining candidates
    data <- data[-meilleur, ]
  }

  # Step 4: sort the final selection by descending score for clean display
  selection <- selection[order(-selection$score), ]
  rownames(selection) <- NULL  # clean up row names

  # Step 5: return final result
  return(selection)
}

