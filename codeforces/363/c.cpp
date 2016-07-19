#include<iostream>
using namespace std;

#define N 100
#define INF 0x3f3f3f3f

#define REST 0
#define CONTEST 1
#define GYM 2

int a[N];
int dp[N+1][N+1];

int main(){
    int n;
    cin >> n;

    for(int i=0;i<n;i++){
        cin >> a[i];
    }

    dp[0][REST] = 0;
    dp[0][CONTEST] = INF;
    dp[0][GYM] = INF;

    for (int i=0;i<n;i++){
        switch (a[i]){
            case REST:
                dp[i+1][REST] = min(dp[i][REST]+1, dp[i][CONTEST]+1);
                dp[i+1][REST] = min(dp[i+1][REST], dp[i][GYM]+1);
                dp[i+1][CONTEST] = INF;
                dp[i+1][GYM] = INF;
                break;
            case CONTEST:
                dp[i+1][REST] = min(dp[i][REST]+1, dp[i][CONTEST]+1);
                dp[i+1][REST] = min(dp[i+1][REST], dp[i][GYM]+1);
                dp[i+1][CONTEST] = min(dp[i][REST], dp[i][GYM]);
                dp[i+1][GYM] = INF;
                break;
            case GYM:
                dp[i+1][REST] = min(dp[i][REST]+1, dp[i][CONTEST]+1);
                dp[i+1][REST] = min(dp[i+1][REST], dp[i][GYM]+1);
                dp[i+1][CONTEST] = INF;
                dp[i+1][GYM] = min(dp[i][REST], dp[i][CONTEST]);
                break;
            default:
                dp[i+1][REST] = min(dp[i][REST]+1, dp[i][CONTEST]+1);
                dp[i+1][REST] = min(dp[i+1][REST], dp[i][GYM]+1);
                dp[i+1][CONTEST] = min(dp[i][REST], dp[i][GYM]);
                dp[i+1][GYM] = min(dp[i][REST], dp[i][CONTEST]);
        }
    }

    int res = INF;
    res = min(res, dp[n][REST]);
    res = min(res, dp[n][CONTEST]);
    res = min(res, dp[n][GYM]);

    cout << res << endl;

    return 0;
}