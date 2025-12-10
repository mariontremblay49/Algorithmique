#include <Rcpp.h>
#include <unordered_map>
#include <unordered_set>
#include <sstream>
#include <algorithm>
#include <string>
#include <vector>
#include <cmath>

using namespace Rcpp;
using namespace std;

// -----------------------------------------------------------
// Structures optimisées
// -----------------------------------------------------------

struct State {
  double score;                      // Score total cumulé
  vector<int> counts;                // Compteurs par groupe [g1, g2, ..., gG]
  unordered_set<int> items_set;      // Pour vérification O(1) de l'unicité
  vector<int> items_order;           // Pour conserver l'ordre de sélection
  
  // Constructeur par défaut
  State() : score(0.0) {}
  
  // Constructeur de copie optimisé
  State(const State& other) 
    : score(other.score)
    , counts(other.counts)
    , items_set(other.items_set)
    , items_order(other.items_order) {}
};

// Comparateur pour le tri des états par score (décroissant)
struct StateComparator {
  bool operator()(const State& a, const State& b) const {
    return a.score > b.score;  // Ordre décroissant
  }
};

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

// Convertit un vecteur d'entiers en clé string pour hachage
string vec2key(const vector<int> &v) {
  string s;
  for (size_t i = 0; i < v.size(); i++) {
    s += to_string(v[i]);
    if (i + 1 < v.size()) s += ",";
  }
  return s;
}

// -----------------------------------------------------------
// ALGORITHME OPTIMISÉ : DP avec Beam Search et Hash Map
// -----------------------------------------------------------

// [[Rcpp::export]]
Rcpp::List ranking_max_dp_optimized_cpp(
    Rcpp::DataFrame data,
    int k,
    Rcpp::List max_par_groupe,
    Rcpp::Nullable<Rcpp::NumericVector> poids_positions = R_NilValue,
    int beam_size = 5000,
    bool verbose = false
) {
  
  // ============================================================
  // PHASE 1 : EXTRACTION ET VALIDATION DES DONNÉES
  // ============================================================
  
  NumericVector score = data["score"];
  CharacterVector groupes = data["groupes"];
  int n = score.size();
  
  if (n == 0) {
    stop("Le DataFrame data est vide");
  }
  
  if (k <= 0) {
    stop("k doit être positif");
  }
  
  if (k > n) {
    warning("k est plus grand que le nombre d'éléments disponibles. Ajusté à n.");
    k = n;
  }
  
  // ============================================================
  // PHASE 2 : CONFIGURATION DES POIDS DE POSITION
  // ============================================================
  
  vector<double> weights(k, 1.0);
  if (poids_positions.isNotNull()) {
    NumericVector w = poids_positions.get();
    if (w.size() != k) {
      stop("poids_positions doit avoir exactement k éléments");
    }
    for (int i = 0; i < k; i++) {
      if (w[i] < 0) {
        stop("Les poids doivent être non-négatifs");
      }
      weights[i] = w[i];
    }
  }
  
  // ============================================================
  // PHASE 3 : PARSING DES GROUPES ET CAPACITÉS
  // ============================================================
  
  CharacterVector group_names = max_par_groupe.names();
  int G = group_names.size();
  
  if (G == 0) {
    stop("max_par_groupe ne doit pas être vide");
  }
  
  // Extraction des capacités maximales
  vector<int> max_cap(G);
  for (int g = 0; g < G; g++) {
    max_cap[g] = as<int>(max_par_groupe[g]);
    if (max_cap[g] < 0) {
      stop("Les capacités maximales doivent être non-négatives");
    }
  }
  
  // Mapping nom_groupe → indice
  unordered_map<string, int> gmap;
  for (int g = 0; g < G; g++) {
    gmap[string(group_names[g])] = g;
  }
  
  // ============================================================
  // PHASE 4 : CRÉATION DE LA MATRICE D'APPARTENANCE
  // ============================================================
  
  // belongs[i][g] = 1 si l'élément i appartient au groupe g
  vector<vector<int>> belongs(n, vector<int>(G, 0));
  
  for (int i = 0; i < n; i++) {
    vector<string> item_groups = split_trim(string(groupes[i]));
    for (const auto &gname : item_groups) {
      if (gmap.count(gname)) {
        belongs[i][gmap[gname]] = 1;
      } else {
        warning("Groupe '" + gname + "' trouvé dans les données mais absent de max_par_groupe");
      }
    }
  }
  
  if (verbose) {
    Rcout << "Initialisation terminée: n=" << n << ", k=" << k 
          << ", G=" << G << ", beam_size=" << beam_size << endl;
  }
  
  // ============================================================
  // PHASE 5 : PROGRAMMATION DYNAMIQUE AVEC BEAM SEARCH
  // ============================================================
  
  // Utilisation de hash maps pour éviter les doublons
  // Clé = configuration des compteurs de groupes
  // Valeur = meilleur état pour cette configuration
  unordered_map<string, State> DP_prev, DP_curr;
  
  // État initial : aucun élément sélectionné
  State init;
  init.score = 0.0;
  init.counts = vector<int>(G, 0);
  init.items_set.clear();
  init.items_order.clear();
  
  string init_key = vec2key(init.counts);
  DP_prev[init_key] = init;
  
  // ============================================================
  // BOUCLE PRINCIPALE : Position par position (1 à k)
  // ============================================================
  
  for (int p = 1; p <= k; p++) {
    double w_p = weights[p - 1];  // Poids de cette position
    DP_curr.clear();
    
    if (verbose && (p % 5 == 0 || p == 1 || p == k)) {
      Rcout << "Position " << p << "/" << k 
            << " - États explorés: " << DP_prev.size() << endl;
    }
    
    // Pour chaque état de la position précédente
    for (auto &kv : DP_prev) {
      const State &prev_state = kv.second;
      
      // Essayer d'ajouter chaque élément disponible
      for (int i = 0; i < n; i++) {
        
        // -----------------------------------------------
        // VÉRIFICATION 1 : Élément déjà utilisé ?
        // -----------------------------------------------
        if (prev_state.items_set.count(i) > 0) {
          continue;  // Élément déjà sélectionné
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
          continue;  // Contrainte violée
        }
        
        // -----------------------------------------------
        // CRÉATION DU NOUVEL ÉTAT
        // -----------------------------------------------
        double new_score = prev_state.score + w_p * score[i];
        string new_key = vec2key(new_counts);
        
        // Ne garder que le meilleur état pour chaque configuration
        if (DP_curr.count(new_key) == 0 || DP_curr[new_key].score < new_score) {
          State new_state;
          new_state.score = new_score;
          new_state.counts = new_counts;
          new_state.items_set = prev_state.items_set;
          new_state.items_set.insert(i);
          new_state.items_order = prev_state.items_order;
          new_state.items_order.push_back(i);
          
          DP_curr[new_key] = new_state;
        }
      }
    }
    
    // -----------------------------------------------
    // BEAM PRUNING : Ne garder que les meilleurs états
    // -----------------------------------------------
    if ((int)DP_curr.size() > beam_size) {
      
      // Extraire tous les états dans un vecteur
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
      
      // Reconstruire la hash map avec seulement les meilleurs
      DP_curr.clear();
      for (int i = 0; i < beam_size; i++) {
        string key = vec2key(all_states[i].counts);
        DP_curr[key] = all_states[i];
      }
      
      if (verbose) {
        Rcout << "  → Pruning appliqué: " << all_states.size() 
              << " → " << beam_size << " états" << endl;
      }
    }
    
    // Passer à la position suivante
    DP_prev = move(DP_curr);
    
    // Vérification de faisabilité
    if (DP_prev.empty()) {
      warning("Aucun état valide à la position " + to_string(p) + 
              ". Les contraintes sont peut-être trop restrictives.");
      break;
    }
  }
  
  // ============================================================
  // PHASE 6 : EXTRACTION DE LA MEILLEURE SOLUTION
  // ============================================================
  
  double best_score = -INFINITY;
  State best_state;
  
  for (auto &kv : DP_prev) {
    if (kv.second.score > best_score) {
      best_score = kv.second.score;
      best_state = kv.second;
    }
  }
  
  // ============================================================
  // PHASE 7 : CONSTRUCTION DU RÉSULTAT
  // ============================================================
  
  if (best_state.items_order.empty()) {
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
  
  int n_selected = best_state.items_order.size();
  
  // Vecteurs pour le DataFrame de sortie
  IntegerVector result_indices(n_selected);
  NumericVector result_scores(n_selected);
  CharacterVector result_groupes(n_selected);
  IntegerVector result_positions(n_selected);
  NumericVector result_poids(n_selected);
  NumericVector result_score_pondere(n_selected);
  
  for (int i = 0; i < n_selected; i++) {
    int idx = best_state.items_order[i];
    result_indices[i] = idx + 1;  // Indexation R (1-based)
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
    final_counts[g] = best_state.counts[g];
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
