#include <Rcpp.h>
#include <unordered_map>
#include <sstream>
#include <algorithm>
#include <string>
#include <vector>
#include <queue>

using namespace Rcpp;
using namespace std;

// -----------------------------------------------------------
// Structures
// -----------------------------------------------------------

struct State {
  double score;
  vector<int> counts;
  vector<int> items;  // éléments déjà utilisés (indices originaux)
  string key;

  bool operator<(State const &other) const {
    return score < other.score; // max-heap
  }
};

// -----------------------------------------------------------
// Fonctions utilitaires
// -----------------------------------------------------------

vector<string> split_trim(const string &s) {
  vector<string> res;
  string token;
  stringstream ss(s);
  while (getline(ss, token, ',')) {
    token.erase(0, token.find_first_not_of(" \t"));
    token.erase(token.find_last_not_of(" \t") + 1);
    res.push_back(token);
  }
  return res;
}

string vec2key(const vector<int> &v) {
  string s;
  for (size_t i=0; i<v.size(); i++) {
    s += to_string(v[i]);
    if (i+1 < v.size()) s += ",";
  }
  return s;
}

vector<int> key2vec(const string &key) {
  vector<int> r;
  string token;
  stringstream ss(key);
  while (getline(ss, token, ',')) r.push_back(stoi(token));
  return r;
}


// -----------------------------------------------------------
// PROGRAMMATION DYNAMIQUE AVEC TAS (Beam Search)
// -----------------------------------------------------------

// [[Rcpp::export]]
Rcpp::List ranking_max_dp_heap_cpp(
    Rcpp::DataFrame data,
    int k,
    Rcpp::List max_par_groupe,
    Rcpp::Nullable<Rcpp::NumericVector> poids_positions = R_NilValue,
    int beam_size = 5000
) {
  // ---------------------
  // Extraction R
  // ---------------------
  NumericVector score = data["score"];
  CharacterVector groupes = data["groupes"];
  int n = score.size();

  // Poids de position
  vector<double> weights(k, 1.0);
  if (poids_positions.isNotNull()) {
    NumericVector w = poids_positions.get();
    if (w.size() != k) {
      stop("poids_positions doit avoir exactement k éléments");
    }
    for (int i=0; i<k; i++) weights[i] = w[i];
  }

  CharacterVector group_names = max_par_groupe.names();
  int G = group_names.size();
  vector<int> max_cap(G);
  for (int g=0; g<G; g++) max_cap[g] = as<int>(max_par_groupe[g]);

  // ---------------------
  // Parsing des groupes
  // ---------------------
  unordered_map<string,int> gmap;
  for (int g=0; g<G; g++) gmap[string(group_names[g])] = g;

  vector<vector<int>> belongs(n, vector<int>(G,0));
  for (int i=0; i<n; i++) {
    for (auto &gname : split_trim(string(groupes[i]))) {
      if (gmap.count(gname)) belongs[i][gmap[gname]] = 1;
    }
  }

  // -------------------------------------------------------
  // DP avec tas (beam search)
  // -------------------------------------------------------
  vector< priority_queue<State> > DP(k+1);

  // État initial
  vector<int> zero(G, 0);
  State init;
  init.score = 0.0;
  init.counts = zero;
  init.items.clear();
  init.key = vec2key(zero);
  DP[0].push(init);

  // -------------------------------------------------------
  // Programmation dynamique avec beam pruning
  // -------------------------------------------------------
  for (int p = 1; p <= k; p++) {
    double w_p = weights[p-1];

    // Extraire les états de DP[p-1]
    vector<State> prev_states;
    while (!DP[p-1].empty()) {
      prev_states.push_back(DP[p-1].top());
      DP[p-1].pop();
    }

    // Pour chaque état précédent
    for (State &prev_state : prev_states) {
      // Essayer chaque élément
      for (int i = 0; i < n; i++) {
        // Vérifier si déjà utilisé
        bool already_used = false;
        for (int used : prev_state.items) {
          if (used == i) {
            already_used = true;
            break;
          }
        }
        if (already_used) continue;

        // Vérifier les contraintes
        vector<int> new_counts = prev_state.counts;
        bool ok = true;
        for (int g = 0; g < G; g++) {
          new_counts[g] += belongs[i][g];
          if (new_counts[g] > max_cap[g]) {
            ok = false;
            break;
          }
        }
        if (!ok) continue;

        // Créer le nouvel état
        State new_state;
        new_state.score = prev_state.score + w_p * score[i];
        new_state.counts = new_counts;
        new_state.items = prev_state.items;
        new_state.items.push_back(i);
        new_state.key = vec2key(new_counts);

        DP[p].push(new_state);

        // Beam pruning
        if ((int)DP[p].size() > beam_size) {
          DP[p].pop();
        }
      }
    }
  }

  // -------------------------------------------------------
  // Meilleur état final
  // -------------------------------------------------------
  double best_score = -1e18;
  vector<int> best_items;

  if (!DP[k].empty()) {
    State best = DP[k].top();
    best_score = best.score;
    best_items = best.items;
  }

  // Construire le résultat
  if (best_items.empty()) {
    warning("Aucune solution trouvée respectant les contraintes");
    return List::create(
      Named("selected_items") = DataFrame::create(),
      Named("best_score") = 0.0,
      Named("beam_size") = beam_size,
      Named("approximation") = (beam_size < 100000)
    );
  }

  // Créer les vecteurs de résultat
  int n_selected = best_items.size();
  IntegerVector result_indices(n_selected);
  NumericVector result_scores(n_selected);
  CharacterVector result_groupes(n_selected);
  IntegerVector result_positions(n_selected);
  NumericVector result_poids(n_selected);
  NumericVector result_score_pondere(n_selected);

  for (int i = 0; i < n_selected; i++) {
    int idx = best_items[i];
    result_indices[i] = idx + 1; // R indexing starts at 1
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

  return List::create(
    Named("selected_items") = result_df,
    Named("best_score") = best_score,
    Named("beam_size") = beam_size,
    Named("approximation") = (beam_size < 100000)
  );
}
