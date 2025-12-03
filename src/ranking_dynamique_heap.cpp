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
  string key;

  bool operator<(State const &other) const {
    return score < other.score; // max-heap
  }
};

// split et trim
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
  string token; stringstream ss(key);
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
    int beam_size = 5000          // largeur du beam = nb max d'états gardés
) {
  // ---------------------
  // Extraction R
  // ---------------------
  NumericVector score = data["score"];
  CharacterVector groupes = data["groupes"];
  int n = score.size();

  CharacterVector group_names = max_par_groupe.names();
  int G = group_names.size();

  vector<int> max_cap(G);
  for (int g=0; g<G; g++) max_cap[g] = as<int>(max_par_groupe[g]);

  // ---------------------
  // Parsing items
  // ---------------------
  unordered_map<string,int> gmap;
  for (int g=0; g<G; g++) gmap[string(group_names[g])] = g;

  vector<vector<int>> belongs(n, vector<int>(G,0));

  for (int i=0; i<n; i++) {
    for (auto &gname : split_trim(string(groupes[i]))) {
      if (gmap.count(gname)) belongs[i][gmap[gname]] = 1;
    }
  }

  // ---------------------
  // Tri par score décroissant (améliore DP)
  // ---------------------
  vector<int> order(n);
  iota(order.begin(), order.end(), 0);
  sort(order.begin(), order.end(),
       [&](int a, int b){ return score[a] > score[b]; });

  vector<double> S(n);
  vector<vector<int>> B(n, vector<int>(G));
  for (int i=0; i<n; i++) {
    S[i] = score[order[i]];
    B[i] = belongs[order[i]];
  }

  // -------------------------------------------------------
  // DP avec tas
  // DP[s] = heap max contenant les meilleurs états
  // -------------------------------------------------------
  vector< priority_queue<State> > DP(k+1);

  // état initial
  vector<int> zero(G, 0);
  State init = {0.0, zero, vec2key(zero)};
  DP[0].push(init);

  // table parent pour reconstruction
  unordered_map<string, pair<string,bool>> parent;
  // key = "i|s|key"

  // -------------------------------------------------------
  // Programmation dynamique
  // -------------------------------------------------------
  for (int i=0; i<n; i++) {
    for (int s=k; s>=1; s--) {

      priority_queue<State> &prevQ = DP[s-1];
      if (prevQ.empty()) continue;

      // copier les états du niveau précédent
      vector<State> prevStates;
      prevStates.reserve(prevQ.size());
      while(!prevQ.empty()) {
        prevStates.push_back(prevQ.top());
        prevQ.pop();
      }
      for (auto &st : prevStates) prevQ.push(st);

      for (State &st : prevStates) {

        // calcul nouveaux comptes
        vector<int> nc = st.counts;
        bool ok = true;
        for (int g=0; g<G; g++) {
          nc[g] += B[i][g];
          if (nc[g] > max_cap[g]) { ok = false; break; }
        }
        if (!ok) continue;

        State nxt;
        nxt.counts = nc;
        nxt.key = vec2key(nc);
        nxt.score = st.score + S[i];

        // insérer dans DP[s]
        DP[s].push(nxt);

        // beam pruning
        if ((int)DP[s].size() > beam_size)
          DP[s].pop();

        // parent
        string pkey = to_string(i)+"|"+to_string(s)+"|"+nxt.key;
        string prevkey = vec2key(st.counts);
        parent[pkey] = {prevkey, true};
      }
    }

    // Cas "ne pas prendre"
    for (int s=0; s<=k; s++) {
      auto q = DP[s];
      while(!q.empty()) {
        State st = q.top(); q.pop();
        string pkey = to_string(i)+"|"+to_string(s)+"|"+st.key;
        if (!parent.count(pkey)) {
          parent[pkey] = {st.key, false};
        }
      }
    }
  }

  // -------------------------------------------------------
  // Chercher le meilleur état final
  // -------------------------------------------------------
  double best_score = -1e18;
  int best_s = 0;
  string best_key;

  for (int s=0; s<=k; s++) {
    if (DP[s].empty()) continue;
    State best = DP[s].top();
    if (best.score > best_score) {
      best_score = best.score;
      best_s = s;
      best_key = best.key;
    }
  }

  // -------------------------------------------------------
  // Reconstruction
  // -------------------------------------------------------
  vector<int> selected;
  int s = best_s;
  string key = best_key;

  for (int i = n-1; i >= 0; i--) {
    string pkey = to_string(i)+"|"+to_string(s)+"|"+key;
    if (!parent.count(pkey)) continue;

    auto p = parent[pkey];
    if (p.second) {
      selected.push_back(order[i] + 1);
      key = p.first;
      s--;
    } else key = p.first;
  }

  reverse(selected.begin(), selected.end());

  return List::create(
    _["best_score"] = best_score,
    _["selected_items"] = selected,
    _["beam_size"] = beam_size,
    _["approximation"] = (beam_size < 100000)
  );
}
