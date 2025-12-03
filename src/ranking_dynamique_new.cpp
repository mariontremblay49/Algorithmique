#include <Rcpp.h>
#include <unordered_map>
#include <sstream>
#include <algorithm>
#include <string>
#include <vector>
using namespace Rcpp;
using namespace std;

struct Item {
  double score;
  vector<string> groups;
};

struct ParentInfo {
  string prev_key;
  bool took;
};

vector<string> split_and_trim(const string &s) {
  vector<string> result;
  string token;
  stringstream ss(s);
  while (getline(ss, token, ',')) {
    token.erase(token.begin(), find_if(token.begin(), token.end(),
                            [](unsigned char ch) { return !isspace(ch); }));
    token.erase(find_if(token.rbegin(), token.rend(),
                        [](unsigned char ch) { return !isspace(ch); }).base(),
                        token.end());
    result.push_back(token);
  }
  return result;
}

string vec_to_key(const vector<int> &v) {
  string s;
  for (size_t i = 0; i < v.size(); i++) {
    s += to_string(v[i]);
    if (i + 1 < v.size()) s += ",";
  }
  return s;
}

vector<int> key_to_vec(const string &key) {
  vector<int> res;
  string token;
  stringstream ss(key);
  while (getline(ss, token, ',')) {
    res.push_back(stoi(token));
  }
  return res;
}


// [[Rcpp::export]]
Rcpp::List ranking_max_cpp_new(
    Rcpp::DataFrame data,
    int k,
    Rcpp::List max_par_groupe
) {
  // ----------------------------
  // 1) Extraire les colonnes
  // ----------------------------
  NumericVector score = data["score"];
  CharacterVector groupes = data["groupes"];
  
  int n = score.size();
  
  // groupes nommés
  CharacterVector group_names = max_par_groupe.names();
  int G = group_names.size();
  
  // capacités par groupe
  IntegerVector max_cap(G);
  for (int g = 0; g < G; g++) {
    max_cap[g] = as<int>( max_par_groupe[g] );
  }
  
  // ----------------------------
  // 2) Parser les items
  // ----------------------------
  vector<Item> items(n);
  for (int i = 0; i < n; i++) {
    items[i].score = score[i];
    items[i].groups = split_and_trim( string(groupes[i]) );
  }
  
  // Map groupe -> index
  unordered_map<string,int> group_index;
  for (int g = 0; g < G; g++) {
    group_index[ string(group_names[g]) ] = g;
  }
  
  // Matrice d'appartenance aux groupes
  vector<vector<int>> item_groups(n, vector<int>(G, 0));
  for (int i = 0; i < n; i++) {
    for (auto &g : items[i].groups) {
      if (group_index.count(g)) {
        item_groups[i][group_index[g]] = 1;
      }
    }
  }
  
  // ----------------------------
  // 3) DP
  // ----------------------------
  vector< unordered_map<string,double> > DP(k + 1);
  unordered_map<string, ParentInfo> parent;
  
  vector<int> zero_counts(G, 0);
  string key0 = vec_to_key(zero_counts);
  DP[0][key0] = 0.0;
  
  for (int i = 0; i < n; i++) {
    const vector<int> &grp_i = item_groups[i];
    double score_i = items[i].score;
    
    for (int s = k; s >= 1; s--) {
      int prev_s = s - 1;
      
      for (auto &kv : DP[prev_s]) {
        const string &key_prev = kv.first;
        double prev_score = kv.second;
        
        vector<int> prev_counts = key_to_vec(key_prev);
        vector<int> new_counts(G);
        
        bool ok = true;
        for (int g = 0; g < G; g++) {
          new_counts[g] = prev_counts[g] + grp_i[g];
          if (new_counts[g] > max_cap[g]) { ok = false; break; }
        }
        if (!ok) continue;
        
        string key_new = vec_to_key(new_counts);
        double new_score = prev_score + score_i;
        
        if (!DP[s].count(key_new) || new_score > DP[s][key_new]) {
          DP[s][key_new] = new_score;
          parent[to_string(i) + "|" + to_string(s) + "|" + key_new] =
            { key_prev, true };
        }
      }
    }
    
    // Cas où on ne prend pas l’item
    for (int s = 0; s <= k; s++) {
      for (auto &kv : DP[s]) {
        string key_prev = kv.first;
        string pkey = to_string(i) + "|" + to_string(s) + "|" + key_prev;
        if (!parent.count(pkey)) {
          parent[pkey] = { key_prev, false };
        }
      }
    }
  }
  
  // ----------------------------
  // 4) Trouver le meilleur état final
  // ----------------------------
  double best_score = -1e18;
  int best_s = 0;
  string best_key = "";
  
  for (int s = 0; s <= k; s++) {
    for (auto &kv : DP[s]) {
      if (kv.second > best_score) {
        best_score = kv.second;
        best_s = s;
        best_key = kv.first;
      }
    }
  }
  
  // ----------------------------
  // 5) Reconstruction
  // ----------------------------
  vector<int> selected;
  int s = best_s;
  string key = best_key;
  
  for (int i = n - 1; i >= 0; i--) {
    string pkey = to_string(i) + "|" + to_string(s) + "|" + key;
    if (!parent.count(pkey)) continue;
    
    ParentInfo p = parent[pkey];
    if (p.took) {
      selected.push_back(i + 1);   // indexation R
      key = p.prev_key;
      s--;
    } else {
      key = p.prev_key;
    }
  }
  
  reverse(selected.begin(), selected.end());
  
  return List::create(
    _["selected_items"] = selected,
    _["best_score"] = best_score,
    _["best_s"] = best_s
  );
}
