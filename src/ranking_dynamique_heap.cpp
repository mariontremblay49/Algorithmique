#include <Rcpp.h>
#include <unordered_map>
#include <unordered_set>
#include <sstream>
#include <algorithm>
#include <string>
#include <vector>
#include <cmath>
#include <limits> // Pour std::numeric_limits
#include <functional> // Pour std::hash

using namespace Rcpp;
using namespace std;

// -----------------------------------------------------------
// Structures et Hacheur pour la Performance
// -----------------------------------------------------------

// Hacheur personnalisé pour std::vector<int> (clé de DP)
struct VectorIntHash {
  size_t operator()(const std::vector<int>& v) const {
    size_t seed = v.size();
    for(int i : v) {
      seed ^= std::hash<int>{}(i) + 0x9e3779b9 + (seed << 6) + (seed >> 2);
    }
    return seed;
  }
};

// **STRUCTURE D'ÉTAT SIMPLIFIÉE POUR ÉCONOMIE DE MÉMOIRE**
struct State {
  double score;                   // Score total cumulé
  vector<int> counts;             // Compteurs par groupe [g1, g2, ..., gG]

  // Champs de Backtracking (pour reconstruire la solution)
  int parent_item_index;          // Indice de l'élément sélectionné à cette étape
  vector<int> parent_counts_key;  // La clé (counts) de l'état parent

  // Constructeur par défaut
  State() : score(0.0), parent_item_index(-1) {}

  // Constructeur de copie
  State(const State& other) = default;
};

// Comparateur pour le tri des états par score (décroissant)
struct StateComparator {
  bool operator()(const State& a, const State& b) const {
    return a.score > b.score;   // Ordre décroissant
  }
};

// Type de Map de DP
using KeyType = vector<int>;
using DPMap = unordered_map<KeyType, State, VectorIntHash>;


// -----------------------------------------------------------
// Fonctions utilitaires
// -----------------------------------------------------------

// Découpe et nettoie une chaîne de groupes séparés par des virgules
vector<string> split_trim(const string &s) {
  vector<string> res;
  string token;
  stringstream ss(s);
  while (getline(ss, token, ',')) {
    // Supprimer les espaces au début et à la fin
    token.erase(0, token.find_first_not_of(" \t"));
    token.erase(token.find_last_not_of(" \t") + 1);
    if (!token.empty()) {
      res.push_back(token);
    }
  }
  return res;
}

// -----------------------------------------------------------
// ALGORITHME OPTIMISÉ : DP avec Beam Search et Backtracking
// -----------------------------------------------------------

// [[Rcpp::export]]
Rcpp::List ranking_max_dp_heap_cpp(
    Rcpp::DataFrame data,
    int k,
    Rcpp::List max_par_groupe,
    Rcpp::Nullable<Rcpp::NumericVector> poids_positions = R_NilValue,
    int beam_size = 5000,
    bool verbose = false
) {

  // ============================================================
  // PHASE 1-2 : EXTRACTION, VALIDATION & POIDS
  // ============================================================

  NumericVector score = data["score"];
  CharacterVector groupes = data["groupes"];
  int n = score.size();

  if (n == 0) stop("Le DataFrame data est vide");
  if (k <= 0) stop("k doit être positif");
  if (k > n) { warning("k est plus grand que le nombre d'éléments disponibles. Ajusté à n."); k = n; }

  vector<double> weights(k, 1.0);
  if (poids_positions.isNotNull()) {
    NumericVector w = poids_positions.get();
    if (w.size() != k) stop("poids_positions doit avoir exactement k éléments");
    for (int i = 0; i < k; i++) {
      if (w[i] < 0) stop("Les poids doivent être non-négatifs");
      weights[i] = w[i];
    }
  }

  // ============================================================
  // PHASE 3 : PARSING DES GROUPES ET CAPACITÉS (CORRIGÉ pour Rcpp List)
  // ============================================================

  CharacterVector group_names = max_par_groupe.names();
  int G = group_names.size();

  if (G == 0) stop("max_par_groupe ne doit pas être vide");

  vector<int> max_cap(G);
  for (int g = 0; g < G; g++) {
    // Correction de l'accès à l'élément scalaire d'une Rcpp::List
    max_cap[g] = Rcpp::as<Rcpp::IntegerVector>(max_par_groupe[g])[0];
    if (max_cap[g] < 0) stop("Les capacités maximales doivent être non-négatives");
  }

  unordered_map<string, int> gmap;
  for (int g = 0; g < G; g++) gmap[string(group_names[g])] = g;

  // ============================================================
  // PHASE 4 : CRÉATION DE LA MATRICE D'APPARTENANCE
  // ============================================================

  vector<vector<int>> belongs(n, vector<int>(G, 0));
  for (int i = 0; i < n; i++) {
    vector<string> item_groups = split_trim(string(groupes[i]));
    for (const auto &gname : item_groups) {
      if (gmap.count(gname)) {
        belongs[i][gmap[gname]] = 1;
      }
    }
  }

  if (verbose) {
    Rcout << "Initialisation terminée: n=" << n << ", k=" << k
          << ", G=" << G << ", beam_size=" << beam_size << endl;
  }

  // ============================================================
  // PHASE 5 : PROGRAMMATION DYNAMIQUE AVEC BACKTRACKING
  // ============================================================

  // DP_maps[p] stocke la meilleure State pour chaque KeyType (counts) à la position p
  vector<DPMap> DP_maps(k + 1);

  // État initial : position 0
  State init;
  init.score = 0.0;
  init.counts = vector<int>(G, 0);
  init.parent_item_index = -1; // Marqueur de début

  DP_maps[0][init.counts] = init;

  // Pour vérifier l'unicité des éléments *à l'intérieur* de la boucle
  // Nous avons besoin d'une carte pour chaque état qui suit son historique.
  // Pour contourner le problème mémoire, on utilise un set global d'indices pour le Beam Search.

  // Le Beam Search nécessite de trier les états. On va utiliser DP_curr comme conteneur
  // temporaire avant de copier vers DP_maps[p].

  for (int p = 1; p <= k; p++) {
    double w_p = weights[p - 1];
    DPMap DP_curr;
    const DPMap& DP_prev = DP_maps[p - 1]; // Référence à l'étape précédente

    // Structure pour le tri : Key=new_counts, Value=new_state
    // Pour gérer l'unicité des éléments: on stocke l'ensemble des items utilisés
    // dans une map temporaire (items_used[new_counts] = set<int>) pour vérifier les conflits.

    // Utiliser une structure temporaire pour l'itération, avec l'historique nécessaire à la vérification.
    struct TempState {
      State state;
      unordered_set<int> items_set;
    };

    unordered_map<KeyType, TempState, VectorIntHash> Temp_DP_curr;

    if (verbose && (p % 5 == 0 || p == 1 || p == k)) {
      Rcout << "Position " << p << "/" << k
            << " - États précédents: " << DP_prev.size() << endl;
    }

    // Pour chaque état de la position précédente
    for (auto &kv : DP_prev) {
      const KeyType &prev_key = kv.first;
      const State &prev_state = kv.second;

      // Il nous faut reconstruire l'ensemble des éléments utilisés par l'état parent
      // pour vérifier l'unicité.

      // Pour des contraintes mémoire, on va accepter l'incohérence et
      // vérifier l'unicité EN AMONT dans R ou forcer k petit.
      // OU, on utilise un Beam Search qui NE se base PAS sur l'état complet
      // MAIS sur une liste triée des *meilleurs* indices d'items disponibles.

      // Finalement, la solution la plus courante pour la DP avec Beam Search et Contraintes
      // est de conserver la structure initiale (set et order) et d'appliquer le Beam Search
      // TRES TÔT et de manière TRES AGRESSIVE.

      // **On va revenir à la structure initiale et corriger les bugs d'indexation/de type Rcpp**

      // État pour la reconstruction de l'historique:
      vector<int> current_items_order;
      unordered_set<int> current_items_set;

      KeyType temp_key = prev_key; // Clé pour le parcours (counts)
      int current_p = p - 1;

      // Reconstruction de l'historique du parent (très lent, mais garantit l'unicité)
      while (current_p > 0) {
        const State& parent_state = DP_maps[current_p][temp_key];
        int item_idx = parent_state.parent_item_index;

        if (item_idx != -1) {
          current_items_order.push_back(item_idx);
          current_items_set.insert(item_idx);
        }
        temp_key = parent_state.parent_counts_key;
        current_p--;
      }
      reverse(current_items_order.begin(), current_items_order.end());


      // Essayer d'ajouter chaque élément disponible
      for (int i = 0; i < n; i++) {

        // -----------------------------------------------
        // VÉRIFICATION 1 : Élément déjà utilisé ? (Reconstruit à chaque fois!)
        // -----------------------------------------------
        if (current_items_set.count(i) > 0) {
          continue; // Élément déjà sélectionné
        }

        // -----------------------------------------------
        // VÉRIFICATION 2 : Contraintes de groupes
        // -----------------------------------------------
        vector<int> new_counts = prev_state.counts;
        bool constraints_ok = true;

        for (int g = 0; g < G; g++) {
          new_counts[g] += belongs[i][g];
          if (new_counts[g] > max_cap[g]) {
            constraints_ok = false;
            break;
          }
        }

        if (!constraints_ok) {
          continue; // Contrainte violée
        }

        // -----------------------------------------------
        // CRÉATION DU NOUVEL ÉTAT
        // -----------------------------------------------
        // Sécurisation de l'accès au vecteur Rcpp score
        double new_score = prev_state.score + w_p * score.at(i);

        // Ne garder que le meilleur état pour chaque configuration
        if (DP_curr.count(new_counts) == 0 || DP_curr[new_counts].score < new_score) {
          State new_state;
          new_state.score = new_score;
          new_state.counts = new_counts;

          // Stockage du parent (pour la reconstruction finale)
          new_state.parent_item_index = i; // Élément sélectionné
          new_state.parent_counts_key = prev_key; // Clé de l'état parent

          DP_curr[new_counts] = new_state;
        }
      }
    }

    // -----------------------------------------------
    // BEAM PRUNING : Ne garder que les meilleurs états
    // -----------------------------------------------
    if ((int)DP_curr.size() > beam_size) {
      // ... (Implémentation du Beam Pruning inchangée) ...

      vector<State> all_states;
      all_states.reserve(DP_curr.size());
      for (auto &kv : DP_curr) {
        all_states.push_back(kv.second);
      }

      // Tri partiel : mettre les beam_size meilleurs au début
      nth_element(all_states.begin(),
                  all_states.begin() + beam_size,
                  all_states.end(),
                  StateComparator());

      // Copier les beam_size meilleurs dans DP_maps[p]
      DP_maps[p].clear();
      for (int i = 0; i < beam_size; i++) {
        DP_maps[p][all_states[i].counts] = all_states[i];
      }

      if (verbose) {
        Rcout << "  → Pruning appliqué: " << all_states.size()
              << " → " << beam_size << " états" << endl;
      }
    } else {
      // Si pas de pruning, tout copier
      DP_maps[p] = std::move(DP_curr);
    }

    // Vérification de faisabilité
    if (DP_maps[p].empty()) {
      warning("Aucun état valide à la position " + to_string(p) +
        ". Les contraintes sont peut-être trop restrictives.");
      k = p - 1; // Ajuster k à la dernière position valide
      break;
    }
  }

  // ============================================================
  // PHASE 6 : EXTRACTION & RECONSTRUCTION (BACKTRACKING)
  // ============================================================

  if (k == 0) {
    warning("Aucune solution trouvée respectant les contraintes");
    // Retourne la liste vide
  }

  const DPMap& final_DP_map = DP_maps[k];

  double best_score = -std::numeric_limits<double>::infinity();
  State best_state;
  KeyType best_key;

  for (auto &kv : final_DP_map) {
    if (kv.second.score > best_score) {
      best_score = kv.second.score;
      best_state = kv.second;
      best_key = kv.first;
    }
  }

  // Backtracking pour reconstruire items_order
  vector<int> final_items_order;
  KeyType current_key = best_key;
  int current_p = k;

  while (current_p > 0) {
    // La clé doit exister si k a été bien ajusté
    if (DP_maps[current_p].count(current_key) == 0) break;

    const State& state = DP_maps[current_p].at(current_key);

    int item_idx = state.parent_item_index;
    if (item_idx != -1) {
      final_items_order.push_back(item_idx);
    }

    current_key = state.parent_counts_key;
    current_p--;
  }

  // Inverser l'ordre (on a reconstruit de k à 1)
  reverse(final_items_order.begin(), final_items_order.end());

  if (final_items_order.empty()) {
    warning("Aucune solution trouvée respectant les contraintes");
    return List::create(
      Named("selected_items") = DataFrame::create(),
      Named("best_score") = 0.0,
      Named("total_items") = 0,
      Named("beam_size") = beam_size,
      Named("is_approximate") = (beam_size < 100000),
                               Named("final_counts") = IntegerVector(G, 0)
    );
  }

  // ... (PHASE 7 : CONSTRUCTION DU RÉSULTAT avec final_items_order et best_state.counts) ...

  int n_selected = final_items_order.size();

  // Vecteurs pour le DataFrame de sortie
  IntegerVector result_indices(n_selected);
  NumericVector result_scores(n_selected);
  CharacterVector result_groupes(n_selected);
  IntegerVector result_positions(n_selected);
  NumericVector result_poids(n_selected);
  NumericVector result_score_pondere(n_selected);

  for (int i = 0; i < n_selected; i++) {
    int idx = final_items_order[i]; // Utilisation du vecteur reconstruit
    result_indices[i] = idx + 1;    // Indexation R (1-based)
    result_scores[i] = score[idx];
    result_groupes[i] = groupes[idx];
    result_positions[i] = i + 1;
    result_poids[i] = weights[i];
    result_score_pondere[i] = weights[i] * score[idx];
  }

  // Créer le DataFrame de sortie
  DataFrame result_df = DataFrame::create(
    Named("index") = result_indices,
    Named("score") = result_scores,
    Named("groupes") = result_groupes,
    Named("position") = result_positions,
    Named("poids_position") = result_poids,
    Named("score_pondere") = result_score_pondere
  );

  // Compteurs finaux par groupe
  IntegerVector final_counts(G);
  for (int g = 0; g < G; g++) {
    final_counts[g] = best_state.counts[g]; // Utilisation des counts du meilleur état final
  }
  final_counts.names() = group_names;

  if (verbose) {
    Rcout << "Solution trouvée avec " << n_selected << " éléments et score "
          << best_score << endl;
  }

  return List::create(
    Named("selected_items") = result_df,
    Named("best_score") = best_score,
    Named("total_items") = n_selected,
    Named("beam_size") = beam_size,
    Named("is_approximate") = (beam_size < 100000),
    Named("final_counts") = final_counts
  );
}
