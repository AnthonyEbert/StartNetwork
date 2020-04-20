#include <bits/stdc++.h>
using namespace std;

// [[Rcpp::export]]
bool graphExists(std::vector<int> a)
{
  int n = a.size();
  // Keep performing the operations until one
  // of the stopping condition is met
  while (1)
  {
    // Sort the list in non-decreasing order
    sort(a.begin(), a.end());
    reverse(a.begin(), a.end());

    // Check if all the elements are equal to 0
    if (a[0] == 0 and a[a.size() - 1] == 0)
      return true;

    // Store the first element in a variable
    // and delete it from the list
    int v = a[0];
    a.erase(a.begin() + 0);

    // Check if enough elements
    // are present in the list
    if (v > a.size())
      return false;

    // Subtract first element from next v elements
    for (int i = 0; i < v; i++)
    {
      a[i]--;

      // Check if negative element is
      // encountered after subtraction
      if (a[i] < 0)
        return false;
    }
  }
}
