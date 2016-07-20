#include<iostream>
#include<algorithm>
#include<vector>
#include<cstdio>
using namespace std;

class UnionFind{
private:
  vector<int> parent;
  vector<int> rank;
public:
  UnionFind(int n):parent(n), rank(n,0){
    for(int i = 0; i < n; i++){
      parent[i] = i;
    }
    return;
  }

  int find(int x){
    if (parent[x] == x){
      return x;
    } else {
      parent[x] = find(parent[x]);
      return parent[x];
    }
  }

  void unite(int x, int y){
    int px = find(x);
    int py = find(y);
    if (px == py) {
      return;
    }

    if (rank[px] < rank[py]) {
      parent[px] = py;
    } else {
      parent[py] = px;
      if (rank[px] == rank[py]) rank[px]++;
    }
  }

  bool equiv(int x, int y){
    return find(x) == find(y);
  }
};

int main(){
  int n;
  cin >> n;

  int a[n];
  int root = -1;
  for(int i=0;i<n;i++){
      scanf("%d",&a[i]);
      a[i]--;
      if(a[i]==i && root<0){
          root=i;
      }
  }

  UnionFind uf(n);
  int res = 0;

  for(int i=0;i<n;i++){
      if(i == root) continue;
      if(uf.equiv(a[i], i)){
          if(root < 0){
              root = i;
          }
          res++;
          a[i] = root;
          uf.unite(i, root);
      }else{
          uf.unite(a[i], i);
      }
  }

  cout << res << endl;
  for(int i=0;i<n;i++){
      printf("%d ", a[i]+1);
  }

  return 0;
}