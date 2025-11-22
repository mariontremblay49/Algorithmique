ranking_max <- function(data, k, max_par_groupe) {
  #---------------------------------------------
  # DYNAMIC ALGORITHM: CONSTRAINED RANKING
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

  # Step 1 : data managing
  data$group_list <- lapply(strsplit(data$groupes, ","), trimws) # transforms column groupes in a list of vectors
  groups <- names(max_par_groupe) # take the list of groups concerned by constraints
  G <- length(groups) # number of groups
  n <- nrow(data)

  group_index <- setNames(seq_len(G), groups) # a group corresponds to a number
  max_cap <- unlist(max_par_groupe) # simple numeric vector

  # Binary representation of groups
  item_groups <- matrix(0, n, G) # create matrix where every line shows if an item belongs to a group g
  for (i in 1:n) {
    for (g in data$group_list[[i]]) { # go through every group of i
      if (g %in% groups) { # ignore groups without limits
        item_groups[i, group_index[g]] <- 1 # item belongs to group g
      }
    }
  }

  # Step 2 : Dynamic programming

  DP <- vector("list", k + 1) # create dictionary
  for (idx in seq_len(k + 1)) DP[[idx]] <- list()

  # initial state :
  key0 <- paste(rep(0, G), collapse = ",") # no group used
  DP[[1]][[key0]] <- 0 # no item selected

  # For each state has previous state and if i was taken
  parent <- list()  # parent[["i,s,key"]] = list(prev_key, took)

  # Main algo
  for (i in 1:n) {
    grp_i <- item_groups[i, ] # binary vector of groups
    score_i <- data$score[i] # score of i

    # Iteration in going down for no re-use
    for (s_idx in seq(k + 1, 2)) {
      s <- s_idx - 1
      prev_idx <- s_idx - 1


      for (key_prev in names(DP[[prev_idx]])) { # go through all old states
        prev_counts <- as.numeric(strsplit(key_prev, ",")[[1]]) # a,b,c --> c(a,b,c)
        new_counts <- prev_counts + grp_i

        # Verify limits
        if (any(new_counts > max_cap)) next # if beyond limit of the group --> we go next group

        key_new <- paste(new_counts, collapse = ",")

        new_score <- DP[[prev_idx]][[key_prev]] + score_i # score with + item i

        # Update DP
        if (is.null(DP[[s_idx]][[key_new]]) || new_score > DP[[s_idx]][[key_new]]) { # update is DP not exist or better score found
          DP[[s_idx]][[key_new]] <- new_score # update score
          parent[[paste(i, s, key_new, sep = "|")]] <- list(prev_key = key_prev, took = TRUE) # save parent
        }
      }
    }

    # propagation --> we don't take the item
    for (s_idx in seq_len(k + 1)) {
      s <- s_idx - 1
      for (key_prev in names(DP[[s_idx]])) {
        parent_key <- paste(i, s, key_prev, sep = "|")
        if (is.null(parent[[parent_key]])) { # if we didn't save a parent for this state
          parent[[parent_key]] <- list(prev_key = key_prev, took = FALSE) # save that the item is not taken
        }
      }
    }
  }

  # Step 3 : find the best final score
  best_s <- 0
  best_key <- NULL
  best_score <- -Inf

  for (s_idx in seq_len(k + 1)) {
    s <- s_idx - 1
    for (key in names(DP[[s_idx]])) {
      val <- DP[[s_idx]][[key]]
      if (!is.null(val) && val > best_score) { # if a best score is found
        best_score <- val # we update the best state
        best_s <- s
        best_key <- key
      }
    }
  }

  # Step 4 : Construct again the optimal solution
  selected <- integer(0)
  s <- best_s
  key <- best_key # begin with the best final state

  for (i in n:1) { # go through DP
    p <- parent[[paste(i, s, key, sep = "|")]] # take the parent associated with the state
    if (!is.null(p) && isTRUE(p$took)) { # if item used in this state
      selected <- c(selected, i) # add item to result
      key <- p$prev_key
      s <- s - 1 # go in the previous state
    } else if (!is.null(p)) {
      key <- p$prev_key
    }
  }

  selected <- sort(selected) # sort

  return(list(
    selected_items = data[selected, , drop = FALSE],
    best_score = best_score,
    best_s = best_s
  ))
}

##############################################################################


ranking_min <- function(data, k, min_par_groupe) {
  #---------------------------------------------
  # DYNAMIC ALGORITHM: CONSTRAINED RANKING
  #
  # Goal:
  #   Select up to k elements to maximize total score,
  #   while respecting a minimum number of elements per group.
  #
  # Inputs:
  #   data : data.frame containing at least:
  #           - a "score" column
  #           - a "groupes" column (string with one or more groups separated by commas)
  #   k : maximum number of elements to select
  #   min_par_groupe : named list giving the minimum allowed per group (e.g., list(A=2, B=3, C=1))
  #
  # Output:
  #   data.frame of selected elements, sorted by descending score
  #---------------------------------------------

  # Step 1 : data managing
  data$group_list <- lapply(strsplit(data$groupes, ","), trimws)
  groups <- names(min_par_groupe)
  G <- length(groups)
  n <- nrow(data)

  group_index <- setNames(seq_len(G), groups)
  min_cap <- unlist(min_par_groupe)

  # Binary representation of the groups
  item_groups <- matrix(0, n, G)
  for (i in 1:n) {
    for (g in data$group_list[[i]]) {
      if (g %in% groups) item_groups[i, group_index[g]] <- 1
    }
  }

  # Step 2 : Dynamic programming
  DP <- vector("list", k + 1)
  for (s in seq_len(k + 1)) DP[[s]] <- list()

  key0 <- paste(rep(0, G), collapse = ",")
  DP[[1]][[key0]] <- 0

  parent <- list()

  # Main algo
  for (i in 1:n) {
    grp_i <- item_groups[i, ]
    score_i <- data$score[i]

    for (s_idx in seq(k + 1, 2)) {
      prev_idx <- s_idx - 1
      s <- s_idx - 1

      for (key_prev in names(DP[[prev_idx]])) {

        prev_counts <- as.numeric(strsplit(key_prev, ",")[[1]])
        new_counts <- prev_counts + grp_i
        key_new <- paste(new_counts, collapse = ",")

        new_score <- DP[[prev_idx]][[key_prev]] + score_i

        if (is.null(DP[[s_idx]][[key_new]]) || new_score > DP[[s_idx]][[key_new]]) {
          DP[[s_idx]][[key_new]] <- new_score
          parent[[paste(i, s, key_new, sep = "|")]] <-
            list(prev_key = key_prev, took = TRUE)
        }
      }
    }

    # propagation
    for (s_idx in seq_len(k + 1)) {
      s <- s_idx - 1
      for (key_prev in names(DP[[s_idx]])) {
        parent_key <- paste(i, s, key_prev, sep = "|")
        if (is.null(parent[[parent_key]])) {
          parent[[parent_key]] <- list(prev_key = key_prev, took = FALSE)
        }
      }
    }
  }

  # Step 3 : Selection of the best final state
  best_s <- NA
  best_key <- NA
  best_score <- -Inf

  for (s_idx in seq_len(k + 1)) {
    s <- s_idx - 1
    for (key in names(DP[[s_idx]])) {
      counts <- as.numeric(strsplit(key, ",")[[1]])

      # verify minimum
      if (any(counts < min_cap)) next

      score <- DP[[s_idx]][[key]]
      if (score > best_score) {
        best_score <- score
        best_s <- s
        best_key <- key
      }
    }
  }

  if (best_score == -Inf) {
    stop("No solution satisfies the minimum per group")
  }

  # Step 4 : Construct again the optimal solution
  selected <- c()
  key <- best_key
  s <- best_s

  for (i in n:1) {
    p <- parent[[paste(i, s, key, sep = "|")]]
    if (p$took) {
      selected <- c(selected, i)
      key <- p$prev_key
      s <- s - 1
    } else {
      key <- p$prev_key
    }
  }

  selected <- sort(selected)

  return(list(
    selected_items = data[selected, ],
    best_score = best_score,
    best_s = best_s,
    min_par_groupe = min_par_groupe
  ))
}

##############################################################################

ranking_minmax <- function(data, k, min_par_groupe, max_par_groupe) {
  #---------------------------------------------
  # DYNAMIC ALGORITHM: CONSTRAINED RANKING
  #
  # Goal:
  #   Select up to k elements to maximize total score,
  #   while respecting a maximum and a minimum number of elements per group.
  #
  # Inputs:
  #   data : data.frame containing at least:
  #           - a "score" column
  #           - a "groupes" column (string with one or more groups separated by commas)
  #   k : maximum number of elements to select
  #   max_par_groupe : named list giving the maximum allowed per group
  #   min_par_groupe : named list giving the minimum allowed per group
  #
  # Output:
  #   data.frame of selected elements, sorted by descending score
  #---------------------------------------------

  # Step 1 : Data managing
  data$group_list <- lapply(strsplit(data$groupes, ","), trimws)
  groups <- names(max_par_groupe)
  G <- length(groups)
  n <- nrow(data)

  group_index <- setNames(seq_len(G), groups)
  max_cap <- unlist(max_par_groupe)
  min_cap <- unlist(min_par_groupe)

  # binary representation of the groups
  item_groups <- matrix(0, n, G)
  for (i in 1:n) {
    for (g in data$group_list[[i]]) {
      if (g %in% groups) item_groups[i, group_index[g]] <- 1
    }
  }

  # Step 2 : Dynamic programming
  DP <- vector("list", k + 1)
  for (s in 0:k) DP[[s + 1]] <- list()

  key0 <- paste(rep(0, G), collapse = ",")
  DP[[1]][[key0]] <- 0

  parent <- list()

  # main algo
  for (i in 1:n) {
    grp_i <- item_groups[i, ]
    score_i <- data$score[i]

    for (s in seq(k, 1)) {
      for (key_prev in names(DP[[s]])) {

        prev_counts <- as.numeric(strsplit(key_prev, ",")[[1]])
        new_counts <- prev_counts + grp_i

        # Verify max
        if (any(new_counts > max_cap)) next

        key_new <- paste(new_counts, collapse = ",")
        new_score <- DP[[s]][[key_prev]] + score_i

        # update DP
        if (is.null(DP[[s + 1]][[key_new]]) || new_score > DP[[s + 1]][[key_new]]) {
          DP[[s + 1]][[key_new]] <- new_score
          parent[[paste(i, s, key_new, sep = "|")]] <-
            list(prev_key = key_prev, took = TRUE)
        }
      }
    }

    # Propagation
    for (s in 0:k) {
      s_idx <- s + 1
      for (key_prev in names(DP[[s_idx]])) {
        parent_key <- paste(i, s, key_prev, sep = "|")
        if (is.null(parent[[parent_key]])) {
          parent[[parent_key]] <- list(prev_key = key_prev, took = FALSE)
        }
      }
    }
  }

  # Step 3 : Slection of the best score that respects the min and max
  best_score <- -Inf
  best_s <- NA
  best_key <- NA

  for (s in 0:k) {
    s_idx <- s + 1
    for (key in names(DP[[s_idx]])) {
      counts <- as.numeric(strsplit(key, ",")[[1]])

      # Vérification des minimums
      if (any(counts < min_cap)) next

      score <- DP[[s_idx]][[key]]
      if (score > best_score) {
        best_score <- score
        best_key <- key
        best_s <- s
      }
    }
  }

  if (best_score == -Inf) {
    stop("No solution satisfies min and max for the groups.")
  }

  # Step 4 : Construction of the optimal solution
  selected <- c()
  key <- best_key
  s <- best_s

  for (i in n:1) {
    p <- parent[[paste(i, s, key, sep = "|")]]
    if (p$took) {
      selected <- c(selected, i)
      key <- p$prev_key
      s <- s - 1
    } else {
      key <- p$prev_key
    }
  }

  selected <- sort(selected)

  return(list(
    selected_items = data[selected, ],
    best_score = best_score,
    selected_count = best_s,
    min_par_groupe = min_par_groupe,
    max_par_groupe = max_par_groupe
  ))
}
