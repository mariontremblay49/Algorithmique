#include <Rcpp.h>
#include <unordered_map>
#include <string>
#include <vector>
#include <algorithm>

using namespace Rcpp;

// Structure pour représenter un état dans la programmation dynamique
struct State {
  double score;
  std::vector<int> items;

  State() : score(-std::numeric_limits<double>::infinity()) {}
  State(double s, std::vector<int> itms) : score(s), items(itms) {}
};

// Fonction pour créer une clé unique à partir des compteurs de groupes
std::string make_key(const std::vector<int>& counts) {
  std::string key;
  for (size_t i = 0; i < counts.size(); i++) {
    if (i > 0) key += ",";
    key += std::to_string(counts[i]);
  }
  return key;
}

// Fonction pour parser une clé en vecteur de compteurs
std::vector<int> parse_key(const std::string& key) {
  std::vector<int> counts;
  std::stringstream ss(key);
  std::string item;
  while (std::getline(ss, item, ',')) {
    counts.push_back(std::stoi(item));
  }
  return counts;
}

// [[Rcpp::export]]
List ranking_max_cpp(DataFrame data, int k, List max_par_groupe,
                     NumericVector poids_positions) {

  // Extraction des données
  NumericVector scores = data["score"];
  CharacterVector groupes = data["groupes"];
  int n = scores.size();

  // Extraction des groupes et leurs limites
  CharacterVector group_names = max_par_groupe.names();
  int G = group_names.size();
  std::vector<int> max_cap(G);
  std::unordered_map<std::string, int> group_index;

  for (int g = 0; g < G; g++) {
    std::string gname = as<std::string>(group_names[g]);
    group_index[gname] = g;
    max_cap[g] = as<int>(max_par_groupe[gname]);
  }

  // Création de la matrice binaire des groupes
  std::vector<std::vector<int>> item_groups(n, std::vector<int>(G, 0));

  for (int i = 0; i < n; i++) {
    std::string grp_str = as<std::string>(groupes[i]);
    std::stringstream ss(grp_str);
    std::string token;

    while (std::getline(ss, token, ',')) {
      // Trim whitespace
      token.erase(0, token.find_first_not_of(" \t"));
      token.erase(token.find_last_not_of(" \t") + 1);

      if (group_index.find(token) != group_index.end()) {
        item_groups[i][group_index[token]] = 1;
      }
    }
  }

  // Programmation dynamique
  std::vector<std::unordered_map<std::string, State>> DP(k + 1);

  // État initial
  std::vector<int> init_counts(G, 0);
  std::string key0 = make_key(init_counts);
  DP[0][key0] = State(0.0, std::vector<int>());

  // Pour chaque position
  for (int p = 0; p < k; p++) {
    double w_p = poids_positions[p];

    // Pour chaque état à la position p
    for (auto& kv : DP[p]) {
      const std::string& key_prev = kv.first;
      const State& state_prev = kv.second;

      std::vector<int> prev_counts = parse_key(key_prev);
      const std::vector<int>& items_used = state_prev.items;

      // Essayer chaque élément
      for (int i = 0; i < n; i++) {
        // Vérifier si déjà utilisé
        if (std::find(items_used.begin(), items_used.end(), i) != items_used.end()) {
          continue;
        }

        // Calculer nouveaux compteurs
        std::vector<int> new_counts = prev_counts;
        bool valid = true;

        for (int g = 0; g < G; g++) {
          new_counts[g] += item_groups[i][g];
          if (new_counts[g] > max_cap[g]) {
            valid = false;
            break;
          }
        }

        if (!valid) continue;

        // Calculer nouveau score
        double new_score = state_prev.score + w_p * scores[i];
        std::vector<int> new_items = items_used;
        new_items.push_back(i);

        // Créer clé du nouvel état
        std::string key_new = make_key(new_counts);

        // Mettre à jour si meilleur
        if (DP[p + 1].find(key_new) == DP[p + 1].end() ||
            new_score > DP[p + 1][key_new].score) {
          DP[p + 1][key_new] = State(new_score, new_items);
        }
      }
    }
  }

  // Trouver la meilleure solution
  double best_score = -std::numeric_limits<double>::infinity();
  std::vector<int> best_items;

  for (auto& kv : DP[k]) {
    const State& state = kv.second;
    if (state.score > best_score) {
      best_score = state.score;
      best_items = state.items;
    }
  }

  // Construire le résultat
  if (best_items.empty()) {
    warning("Aucune solution trouvée respectant les contraintes");
    return List::create(
      Named("selected_items") = DataFrame::create(),
      Named("best_score") = 0.0
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
    result_scores[i] = scores[idx];
    result_groupes[i] = groupes[idx];
    result_positions[i] = i + 1;
    result_poids[i] = poids_positions[i];
    result_score_pondere[i] = poids_positions[i] * scores[idx];
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
    Named("best_score") = best_score
  );
}
